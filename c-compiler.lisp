(in-package :pc)

(defvar *var-access-exprs* (make-hash-table :test #'eq))
(defvar *var-address-exprs* (make-hash-table :test #'eq))
(defvar *available-registers*)

(defvar *current-var-address* 0)

(defun match-global-const (def)
  (optima:match def
                ((list var val)
                 (push val (gethash var *var-access-exprs*)))
                (otherwise
                 (error "Is not valid constant def"))))

(defun parse-global-consts (global-consts)
  (dolist (const global-consts)
    (match-global-const const)))

(defun match-global-def (def)
  (optima:match def
                ((guard var (symbolp var))
                 (push (list *current-var-address*) (gethash var *var-access-exprs*))
                 (setf (gethash var *var-address-exprs*) *current-var-address*)
                 (incf *current-var-address*))
                ((list var size)
                 (push (list *current-var-address*) (gethash var *var-access-exprs*))
                 (setf (gethash var *var-address-exprs*) *current-var-address*)
                 (incf *current-var-address* size))
                (otherwise
                 (error "Is not valid global def"))))

(defun parse-global-defs (global-defs)
  (dolist (def global-defs)
    (match-global-def def)))

(defvar *locals*)

(defun adjust-out-vars (out-vars)
  (loop repeat (- 2 (length out-vars)) do
        (setf out-vars (append out-vars (list (gensym)))))
  out-vars)

(defun make-label ()
  (gensym "label"))

(defvar *func-ret-label-vars* (make-hash-table :test #'eq))
(defvar *func-par-vars* (make-hash-table :test #'eq))
(defvar *cur-func-ret-var* nil)

(defvar *peephole-optimizations* t)

(defun push-ghc-instruction (instr)
  (labels ((%same-loc-move-instr (instr)
             (optima:match instr
               ((list 'mov a b)
                (equal a b))
               (otherwise nil)))
           (%find-top-instr ()
             (find-if #'listp *current-instructions*))
           (%remove-top-instr ()
             (setf *current-instructions* (remove-if #'listp *current-instructions* :count 1))
             (%adjust-after-top-instr))
           (%adjust-after-top-instr ()
             (labels ((%inner-remove (lst labels)
                        (if (null lst)
                            labels
                            (if (symbolp (car lst))
                                (%inner-remove (cdr lst) (cons (car lst) labels))
                                (if (%is-goto (car lst) labels)
                                    (%inner-remove (cdr lst) labels)
                                    (append labels lst))))))
               (setf *current-instructions* (%inner-remove *current-instructions* nil))))
           (%is-goto (instr labels)
             (optima:match instr
               ((list 'mov 'pc lbl)
                (member lbl labels :test #'eq))
               (otherwise nil))))
    (if *peephole-optimizations*
        (unless (%same-loc-move-instr instr)
          (let ((top-instr (%find-top-instr)))
            (when (equal top-instr instr)
              (%remove-top-instr))
            (when (and (symbolp instr)
                       (%is-goto top-instr (list instr)))
              (%remove-top-instr))
            (push instr *current-instructions*)))
        (push instr *current-instructions*))))

(defun handle-stmt (stmt)
  (optima:match 
   stmt
   ((guard label (symbolp label))
    (push-ghc-instruction label))
   ((list* 'block stmts)
    (parse-block stmts))
   ((list 'goto label)
    (push-ghc-instruction `(mov pc ,label)))
   ((list 'goto-if label cond)
    (let ((*label-target* label))
      (ghc-compile-expr cond (car *available-registers*) (cdr *available-registers*) *var-access-exprs* *var-address-exprs*)))
   ((list 'if cond then-stmt else-stmt)
    (let ((*label-target* (make-label))
          (end-label (make-label)))
      (ghc-compile-expr cond (car *available-registers*) (cdr *available-registers*) *var-access-exprs* *var-address-exprs*)
      (handle-stmt else-stmt)
      (handle-stmt `(goto ,end-label))
      (handle-stmt *label-target*)
      (handle-stmt then-stmt)
      (handle-stmt end-label)))
   ((list* 'locals decls)
    (let ((available-locals (remove 'a (remove 'b *available-registers*))))
      (when (< (length available-locals) (length decls))
        (error "Cannot allocate locals"))
      (loop for decl in decls
            for local in available-locals do
            (push local (gethash decl *var-access-exprs*))
            (push decl *locals*)
            (setf *available-registers* (remove local *available-registers*)))))
   ((list 'halt)
    (push-ghc-instruction `(hlt)))
   ((list 'int e in-exprs out-vars)
    (setf *available-registers* (remove 'a (remove 'b *available-registers*)))
    (setf out-vars (adjust-out-vars out-vars))
    (loop for out-var in out-vars
          for local in '(a b) do
          (push local (gethash out-var *var-access-exprs*))
          (push out-var *locals*))
    (loop for in-expr in in-exprs
          for out-var in out-vars do
          (handle-stmt `(:= ,out-var ,in-expr)))
    (let ((val (ghc-compile-expr e (car *available-registers*) (cdr *available-registers*) *var-access-exprs* *var-address-exprs*)))
      (push-ghc-instruction `(int ,val))))
   ((list* 'func label in-params out-params body)
    (let ((ret-label (gensym "ret")))
      (parse-global-defs (cons ret-label (append in-params out-params)))
      (setf (gethash label *func-par-vars*) in-params)
      (setf (gethash label *func-ret-label-vars*) ret-label)
      (handle-stmt label)
      (let ((*cur-func-ret-var* ret-label))
        (handle-stmt `(block ,@body)))
      (handle-stmt `(:= pc ,ret-label))))
   ((list* 'call func params)
    (let ((vars (gethash func *func-par-vars*))
          (ret-label-var (gethash func *func-ret-label-vars*))
          (ret-label (gensym "ret-label")))
      (loop for v in vars
         for p in params
         do (handle-stmt `(:= ,v ,p)))
      (handle-stmt `(:= ,ret-label-var (label ,ret-label)))
      (handle-stmt `(goto ,func))
      (handle-stmt ret-label)))
   ((list 'return)
    (handle-stmt `(:= pc ,*cur-func-ret-var*)))
   (e
    (ghc-compile-expr e nil *available-registers* *var-access-exprs* *var-address-exprs*))
   ))

(defun parse-block (stmts)
  (let* ((*locals* nil))
    (dolist (stmt stmts)
      (handle-stmt stmt))
    (dolist (var *locals*)
      (let ((register (pop (gethash var *var-access-exprs*))))
        (push register *available-registers*)))))

(defun compile-c-program (program)
  (let ((*var-access-exprs* (make-hash-table :test #'eq))
        (*var-address-exprs* (make-hash-table :test #'eq))
        (*func-ret-label-vars* (make-hash-table :test #'eq))
        (*func-par-vars* (make-hash-table :test #'eq))
        (*available-registers* (list 'a 'b 'c 'd 'e 'f 'g 'h))
        (*current-var-address* 0)
        (*cur-func-ret-var* nil))

    (push 'pc (gethash 'pc *var-access-exprs*))
    (parse-global-consts (first program))
    (parse-global-defs (second program))
    (parse-block (third program))))

(defun full-c-compliation (program &key (transform-labels t))
  (let ((*current-instructions* nil))
    (compile-c-program program)
    (ghc-asm-dump (funcall (if transform-labels #'transform-labels #'identity)
                           (reverse *current-instructions*)) t)))

(defparameter *ghost-prog-greedy*
  '(((+up+ 0)
     (+right+ 1)
     (+down+ 2)
     (+left+ 3)
     (+lookaheads+ 5)
     (+lm-found+ 20))

    (lm-x
     lm-y
     ghost-index
     ghost-x
     ghost-y
     ghost-direction
     upv
     rightv
     downv
     leftv
     bestv
     bestd
     

     eval-cell-ret
     eval-cell-val
     eval-cell-x
     eval-cell-y

     eval-direction-ret
     eval-direction-val
     eval-direction-x
     eval-direction-y

     looks)

    (block
        (goto main)

      eval-cell
      (block
          (+= looks 1)
        (if (< looks +lookaheads+)
          (block
              (:= eval-cell-val 0)
            (if (= eval-cell-x lm-x)
                (if (= eval-cell-y lm-y)
                    (+= eval-cell-val +lm-found+)
                  (block))
              (block))
              (int 7 (eval-cell-x eval-cell-y) (content))
            (if (= content 0)
                (:= pc eval-cell-ret)
              ;; analyze surrounded cells: up, right, down, left and return
              (block
                  (block (int 7 (eval-cell-x (- eval-cell-y 1)) (content))
                    (if (> content 0) (++ eval-cell-val) (block)))
                (block (int 7 ((+ eval-cell-x 1) eval-cell-y) (content))
                  (if (> content 0) (++ eval-cell-val) (block)))
                (block (int 7 (eval-cell-x (+ eval-cell-y 1)) (content))
                  (if (> content 0) (++ eval-cell-val) (block)))
                (block (int 7 ((- eval-cell-x 1) eval-cell-y) (content))
                  (if (> content 0) (++ eval-cell-val) (block)))
                (:= pc eval-cell-ret))))
          (block
              (:= eval-cell-val 0)
            (:= pc eval-cell-ret))))

      eval-direction-up
      (block        
          (:= looks 0)

        (:= eval-direction-val 0)
        
        (:= eval-cell-ret (label eval-cell-ret-up))
        (:= eval-cell-x eval-direction-x)
        (:= eval-cell-y eval-direction-y)
        
        eval-direction-loop-up
        ;; eval current cell
        (goto eval-cell)
        eval-cell-ret-up
        (if (= eval-cell-val 0)
            ;; it's deadend, so lets return
            (:= pc eval-direction-ret)
          ;; lets continue
          (block
              ;; we don't need to calc prev position
              (+= eval-direction-val (- eval-cell-val 1))
            ;; move next
            (-= eval-cell-y 1)
            (goto eval-direction-loop-up))))

      eval-direction-right
      (block
          (:= looks 0)
        
        (:= eval-direction-val 0)

        (:= eval-cell-ret (label eval-cell-ret-right))
        (:= eval-cell-x eval-direction-x)
        (:= eval-cell-y eval-direction-y)
        
        eval-direction-loop-right
        ;; eval current cell
        (goto eval-cell)
        eval-cell-ret-right
        (if (= eval-cell-val 0)
            ;; it's deadend, so lets return
            (:= pc eval-direction-ret)
          ;; lets cont
          (block
              ;; we dont neet to calc prev position
              (+= eval-direction-val (- eval-cell-val 1))
            ;; move next
            (+= eval-cell-x 1)
            (goto eval-direction-loop-right))))

      
      eval-direction-down
      (block
          (:= looks 0)

        (:= eval-direction-val 0)

        (:= eval-cell-ret (label eval-cell-ret-down))
        (:= eval-cell-x eval-direction-x)
        (:= eval-cell-y eval-direction-y)
        
          eval-direction-loop-down
        ;; eval current cell
          (goto eval-cell)
          eval-cell-ret-down
          (if (= eval-cell-val 0)
              ;; it's deadend, so lets return
              (:= pc eval-direction-ret)
            ;; lets cont
            (block
                ;; we dont need to calc prev position
                (+= eval-direction-val (- eval-cell-val 1))
              ;; move next
              (+= eval-cell-y 1)
              (goto eval-direction-loop-down))))
      
                        
      eval-direction-left
      (block 
          (:= looks 0)

        (:= eval-direction-val 0)

        (:= eval-cell-ret (label eval-cell-ret-left))
        (:= eval-cell-x eval-direction-x)
        (:= eval-cell-y eval-direction-y)

        eval-direction-loop-left
        ;; eval current cell
        (goto eval-cell)
        eval-cell-ret-left
        (if (= eval-cell-val 0)
            ;; it's deadend, so lets return
            (:= pc eval-direction-ret)
          ;; lets cont
          (block
              ;; we dont need to calc prev position
              (+= eval-direction-val (- eval-cell-val 1))
            ;; move next
            (-= eval-cell-x 1)
            (goto eval-direction-loop-left))))
      
      main
      ;; init
      (block
          (int 1 () (x y))
        (:= lm-x x)
        (:= lm-y y))
      (block 
          (int 3 () (index))
        (:= ghost-index index))
      (block
          (int 5 (ghost-index) (x y))
        (:= ghost-x x)
        (:= ghost-y y))
      (block
        (int 6 (ghost-index) (vitality direction))
        (:= ghost-direction direction))
        


      ;; analyze-up
      (if (= ghost-direction +down+)
          (:= upv 0)
        (block
            (:= eval-direction-ret (label analyze-up-ret))
          (:= eval-direction-x ghost-x)
          (:= eval-direction-y (- ghost-y 1))
          (goto eval-direction-up)
          analyze-up-ret
          (:= upv eval-direction-val)
          (if (> upv 0)
              (if (> ghost-y lm-y)
                  (++ upv)
                (block))
            (block))))
      
      ;; analyze-right
      (if (= ghost-direction +left+)
          (:= rightv 0)
        (block
            (:= eval-direction-ret (label analyze-right-ret))
          (:= eval-direction-x (+ ghost-x 1))
          (:= eval-direction-y ghost-y)
          (goto eval-direction-right)
          analyze-right-ret
          (:= rightv eval-direction-val)
          (if (> rightv 0)
              ;; analyze lambda man position
              (if (< ghost-x lm-x)
                  (++ rightv)
                (block)))))

      ;; analyze-down
      (if (= ghost-direction +up+)
          (:= downv 0)
        (block
            (:= eval-direction-ret (label analyze-down-ret))
          (:= eval-direction-x ghost-x)
          (:= eval-direction-y (+ ghost-y 1))
          (goto eval-direction-down)
          analyze-down-ret
          (:= downv eval-direction-val)
          (if (> downv 0)
              (if (< ghost-y lm-y)
                  (++ downv)
                (block))
            (block))))
      
      ;; analyze-left
      (if (= ghost-direction +right+)
          (:= leftv 0)
        (block
            (:= eval-direction-ret (label analyze-left-ret))
          (:= eval-direction-x (- ghost-x 1))
          (:= eval-direction-y ghost-y)
          (goto eval-direction-left)
          analyze-left-ret
          (:= leftv eval-direction-val)
          ;; analze lambda man position
          (if (> leftv 0)
              (if (> ghost-x lm-x)
                  (++ leftv)
                (block))
            (block))))

      (if (> upv rightv)
          (block (:= bestv upv) (:= bestd +up+))
        (block (:= bestv rightv) (:= bestd +right+)))
      (if (> downv bestv)
          (block (:= bestv downv) (:= bestd +down+))
        (block))
      (if (> leftv bestv)
          (block (:= bestv leftv) (:= bestd +left+))
        (block))

      (block (int 0 (bestd) ()))
      (halt))))


          
      
;; (defparameter *ghost-prog-adv*
;;   '(((+up+ 0)
;;      (+right+ 1)
;;      (+down+ 2)
;;      (+left+ 3))

;;     (square-fit-ret
;;      square-fit-x
;;      square-fit-y
;;      square-fit-value

;;      best-fit
;;      best-direction

;;      top-fit right-fit down-fit left-fit)

;;     (block
;;         (goto main)
;;       square-fit
;;       (block
;;           (locals fit)
;;         (:= fit 0)
;;         ;; center 
;;         (block
;;             (int 7 (square-fit-x square-fit-y) (center))
;;           (if (= center 0)
;;               (block 
;;                   (:= square-fit-value fit)
;;                 (:= pc square-fit-ret))
;;             (if (> center 4)
;;                 (+= fit 1)
;;               (+= fit center))))

;;         ;; top
;;         (block
;;             (int 7 (square-fit-x (- square-fit-y 1)) (center))
;;           (if (> center 4)
;;               (+= fit 1)
;;             (+= fit center)))

;;         ;; top right
;;         (block
;;             (int 7 ((+ square-fit-x 1) (- square-fit-y 1)) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; right
;;         (block
;;             (int 7 ((+ square-fit-x 1) square-fit-y) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; down right
;;         (block
;;             (int 7 ((+ square-fit-x 1) (+ square-fit-y 1)) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; down
;;         (block
;;             (int 7 (square-fit-x (+ square-fit-y 1)) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; down left
;;         (block
;;             (int 7 ((- square-fit-x 1) (+ square-fit-y 1)) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; left
;;         (block
;;             (int 7 ((- square-fit-x 1) square-fit-y) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center)))

;;         ;; top left
;;         (block
;;             (int 7 ((- square-fit-x 1) (- square-fit-y 1)) (center))
;;           (if (> center 4) (+= fit 1) (+= fit center))
;;           (:= square-fit-value fit)
;;           (:= pc square-fit-ret)))

;;         main
;;         (block
;;             (block
;;                 (int 3 () (index))
;;               (int 4 (index) (ghost-x ghost-y)) 
;;               (:= square-fit-ret (label ret-top))
;;               (:= square-fit-x ghost-x)
;;               (:= square-fit-y (+ ghost-y 1))
;;               (goto square-fit)
;;               ret-top
;;               (:= top-fit square-fit-value)

;;               (:= square-fit-ret (label ret-right))
;;               (:= square-fit-x (+ ghost-x 1))
;;               (:= square-fit-y ghost-y)
;;               (goto square-fit)
;;               ret-right
;;               (:= right-fit square-fit-value)

;;               (:= square-fit-ret (label ret-down))
;;               (:= square-fit-x ghost-x)
;;               (:= square-fit-y (+ ghost-y 1))
;;               (goto square-fit)
;;               ret-down
;;               (:= down-fit square-fit-value)

;;               (:= square-fit-ret (label ret-left))
;;               (:= square-fit-x (- ghost-x 1))
;;               (:= square-fit-y ghost-y)
;;               (goto square-fit)
;;               ret-left
;;               (:= left-fit square-fit-value)

;;               ;; choose best
;;               (if (> top-fit right-fit)
;;                   (block
;;                       (:= best-fit top-fit)
;;                     (:= best-direction +up+))
;;                 (block
;;                     (:= best-fit right-fit)
;;                   (:= best-direction +right+)))
;;               (if (> best-fit down-fit)
;;                   (:= best-fit best-fit)
;;                 (block
;;                     (:= best-fit down-fit)
;;                   (:= best-direction +down+)))
;;               (if (> best-fit left-fit)
;;                   (:= best-fit best-fit)
;;                 (block 
;;                     (:= best-fit left-fit)
;;                   (:= best-direction +left+)))

;;               (int 0 (best-direction) ())
;;               (halt))))))
                

      
          

(defparameter *ghost-prog1*
 '(((+up+ 0)
    (+right+ 1)
    (+down+ 2)
    (+left+ 3)
    
    (+wall+ 0))
   
   (var1
    ret-addr)
   
   (
       (goto start)
       (func calc1 (c1x c1y) (c1r)
             (if (> c1x c1y)
                 (return)
                 (:= c1r (+ c1x c1y))))
     (block
         start
       ;; (block
       ;;     (int 3 () (index))
       ;;   (if (= (& index 254) 0)
       ;;       (:= prefer-h 0)
       ;;       (:= prefer-h 1)))
       ;; (block
       ;;     (locals dirh dirv)
       ;;   (locals man-x man-y)
       ;;   (block
       ;;       (int 1 () (x y))
       ;;     (:= man-x x)
       ;;     (:= man-y y))
       ;;   (block
       ;;     (int 3 () (index))
       ;;     (int 4 (index) (ghost-x ghost-y))
       ;;     (if (> man-x ghost-x)
       ;;         (:= dirh +right+)
       ;;         (:= dirh +left+))
       ;;     (if (> man-y ghost-y)
       ;;         (:= dirv +down+)
       ;;         (:= dirv +up+)))
       ;;   (block (if (= prefer-h 1)
       ;;              (int 0 (dirh) ())
       ;;              (int 0 (dirv) ())))
       ;;   (halt))
       (block
           (:= c1x 5)
         (call calc1 5 10)
         (:= var1 c1r)
         (call calc1 20 30)
         (+= var1 c1r))
     ))))

(defparameter *ghost-prog2*
 '(((+up+ 0)
    (+right+ 1)
    (+down+ 2)
    (+left+ 3)
    
    (+wall+ 0)

    (+seek+ 0)
    (+follow+ 1)
    (+lookahead-num+ 12)
    (+tmask+ 31))
   
   (state
    follow-ind
    last-known-x
    last-known-y
    last-x
    last-y
    (tdirs 32)
    ;; Index for delta for nextdir:
    delta-index
    delta-inited
    deltas
    deltas1
    deltas2
    deltas3
    deltas4
    rndval)
   
   (
    (if (= delta-inited 0)
        (block
            ;; Regular - same dir, right, left, back
            (:= deltas1 #b11100100)
          (:= deltas4 #b11100100)
          ;; Right, left, same dir, back
          (:= deltas2 #b10011001)
          ;; Left, right, same dir, back
          (:= deltas3 #b10111011)
          (:= delta-inited 1)
          (:= rndval 13)
          (:= deltas deltas1))
        (block))
    (goto-if seek-prog (= state +seek+))
    (goto-if follow-prog (= state +follow+))
    (goto seek-prog)
    
    (func move-dir (mdx mdy mdir) ()
     (if (= mdir +up+)
         (-- mdy)
         (block))
     (if (= mdir +down+)
         (++ mdy)
         (block))
     (if (= mdir +right+)
         (++ mdx)
         (block))
     (if (= mdir +left+)
         (-- mdx)
         (block)))

    (func nextdir (nx ny ndir) ()
     (locals ind delta)
     (:= ind 3)
     (:= delta deltas)
     nl1
     (:= ndir (& (+ ndir (& delta 3)) 3))
     (call move-dir nx ny ndir)
     ;; (block
     ;;     (int 8 (mdx mdy) ()))
     ;; (block
     ;;     (int 8 (ndir) ()))
     (block
         (int 7 (mdx mdy) (cont))
       ;; (int 8 (cont) ())
       (if (= cont 0)
           (block)
           (block
               ;;(int 8 (222) ())
               (return))))
     (/= delta 4)
     (if (= ind 0)
         (block)
         (block
             (-- ind)
           (goto nl1))))

    (func traverse-from (tx ty tlmx tlmy tdir tnum) (tfound tind)
     (locals ind)
     (-- tnum)
     (:= ind 0)
     (:= tfound 0)
     ;; (block
     ;;     (int 8 (tlmx tlmy) ()))
     tl
     (:= tind ind)
     (call nextdir tx ty tdir)
     (:= ind tind)
     ;; (block
     ;;     (int 8 (nx ny) ()))
     ;; (block
     ;;     (int 8 (ndir) ()))
     (:= (val (+ (address tdirs) ind)) ndir)
     ;; (block
     ;;     (locals ci)
     ;;   (:= ci (+ (* ind 2) (address tcoords)))
     ;;   (:= (val ci) mdx)
     ;;   (:= (val (+ ci 1)) mdy))
     (:= tx mdx)
     (:= ty mdy)
     (:= tdir ndir)
     (if (= tx tlmx)
         (if (= ty tlmy)
             (block
                 (:= tfound 1)
               (:= tnum (+ ind 1))
               (return))
             (block))
         (block))
     (if (< ind tnum)
         (block
             (++ ind)
           (goto tl))
         (block)))

    (func get-dir (x1 y1 x2 y2) (gdir)
     (if (> x1 x2)
         (:= gdir +left+)
         (block))
     (if (< x1 x2)
         (:= gdir +right+)
         (block))
     (if (> y1 y2)
         (:= gdir +up+)
         (block))
     (if (< y1 y2)
         (:= gdir +down+)
         (block)))

    (func update-last-known (fx fy ul-ind) (stop-follow)
     (locals dir)
     ;;(:= ind follow-ind)
     ;; (block (int 8 (44) ()))
     ;; (block (int 8 (fx fy) ()))
     fl
     (:= dir (val (+ (address tdirs) ul-ind)))
     (call move-dir fx fy dir)
     (if (= mdx tlmx)
         (if (= mdy tlmy)
             (block
                 ;;(:= last-known-x mdx)
                 ;;(:= last-known-y mdy)
                 ;; (block (int 8 (99) ()))
                 ;; (block (int 8 (fx fy) ()))
                 (:= stop-follow 0)
                 (:= tnum (+ ul-ind 1))
                 (return))
             (block))
         (block))
     (:= fx mdx)
     (:= fy mdy)
     (++ ul-ind)
     (&= ul-ind +tmask+)
     (if (= ul-ind tnum)
         (block)
         (goto fl))
     (call get-dir fx fy tlmx tlmy)
     (:= (val (+ (address tdirs) tnum)) gdir)
     (call move-dir fx fy gdir)
     (block (int 7 (mdx mdy) (cont))
       (if (= cont 0)
           (block
               ;; Prefer x change over y change
               (call get-dir fx tlmy tlmx tlmy)
             (:= (val (+ (address tdirs) tnum)) gdir)
             (call move-dir fx fy gdir))
           (block)))
     (if (= mdx tlmx)
         (if (= mdy tlmy)
             (block
                 (:= stop-follow 0))
             (block))
         (block))
     ;; (block
     ;;     (locals ci)
     ;;   (:= ci (+ (* tnum 2) (address tcoords)))
     ;;   (:= (val ci) tlmx)
     ;;   (:= (val (+ ci 1)) tlmy))
     ;;(block (int 8 (55) ()))
     ;;(block (int 8 (fx fy) ()))
     (++ tnum)
     (&= tnum +tmask+)
     (goto-if seek-prog-re (= tnum follow-ind)))

    ;; (func seek-when-lost-path () ()
    ;;  ;;(block (int 8 (tx ty) ()))
    ;;  ;;(block (int 8 (last-x last-y) ()))
    ;;  (if (= tx last-x)
    ;;      (if (= ty last-y)
    ;;          (return)
    ;;          (block))
    ;;      (block))
    ;;  (goto seek-prog-re))

    (func read-positions () ()
     (block
        (int 1 () (x y))
      (:= tlmx x)
      (:= tlmy y))
    (block
        (int 3 () (index))
      (int 5 (index) (x y))
      (:= tx x)
      (:= ty y)))
    seek-prog-re
    ;;(block (int 8 (111) ())
    ;;  (int 8 (tx ty) ()))
    seek-prog
    (:= state +seek+)
    ;; (block
    ;;     (int 8 (111) ()))
    (call read-positions)
    (block      
      (int 3 () (index))
      (int 6 (index) (vit dir))
      (:= tdir dir))

    (call traverse-from tx ty tlmx tlmy tdir +lookahead-num+)

    (++ delta-index)
    (&= delta-index 3)
    (:= deltas (val (+ (address deltas1) delta-index)))

    (if (= tfound 1)
        (block
            ;; (block (int 8 (222) ())
            ;;   (int 8 (tx ty) ()))
          (:= state +follow+)
          (:= follow-ind 0)
          ;;(:= last-known-x tlmx)
          ;;(:= last-known-y tlmy)
          (goto follow-prog))
        (block))
    
    (:= rndval (* rndval 17))
    (+= rndval 131)
    (block
        (int 0 ((& rndval 3)) ()))
    
    follow-prog
    ;; (block
    ;;     (int 8 (222) ()))
    (block
        (int 3 () (index))
      (int 6 (index) (vit dir))
      (goto-if seek-prog-re (> vit 0)))
    
    (call read-positions)
    ;; (block
    ;;     (locals ci)
    ;;   (:= ci (+ (* follow-ind 2) (address tcoords)))
    ;;   (:= last-x (val ci))
    ;;   (:= last-y (val (+ ci 1))))
    ;;(call seek-when-lost-path)
    (:= stop-follow 1)
    (call update-last-known tx ty follow-ind)
    (goto-if okok1 (= stop-follow 0))
    (call update-last-known fx fy ul-ind)
    okok1
    (goto-if seek-prog-re (= stop-follow 1))
    ;; (block
    ;;     (int 8 (last-known-x last-known-y) ()))
    ;; (block
    ;;     (int 8 (follow-ind tnum) ()))
    (goto-if seek-prog-re (= follow-ind tnum))
    (block
        (locals dir)
      (:= dir (val (+ (address tdirs) follow-ind)))
      (call move-dir tx ty dir)
      (block
          (int 7 (mdx mdy) (cont))
        (if (= cont 0)
            (block
                (goto seek-prog-re))
            (block
                (block
                    (int 0 (dir) ()))
              (++ follow-ind)
              (&= follow-ind +tmask+))))
      (halt))
    )))
