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
     (+left+ 3))

    (lm-x
     lm-y
     ghost-x
     ghost-y
     delta-x
     delta-y
     upv
     rightv
     downv
     leftv
     bestv
     bestd
     ghost-index)

    (block
        ;; update values
        (:= upv 0)
      (:= rightv 0)
      (:= downv 0)
      (:= leftv 0)
      
      (block
          (int 1 () (x y))
        (:= lm-x x)
        (:= lm-y y))
      (block 
          (int 3 () (index))
        (int 5 (index) (x y))
        (:= ghost-index index)
        (:= ghost-x x)
        (:= ghost-y y))
      
      ;; analyze-top
      (int 7 (ghost-x (- ghost-y 1)) (content))
      (if (> content 0)
          (++ upv)
        (block))
      
      ;; analyze-right
      (int 7 ((+ ghost-x 1) ghost-y) (content))
      (if (> content 0)
          (++ rightv)
        (block))
      
      ;; analyze-down
      (int 7 (ghost-x (+ ghost-y 1)) (content))
      (if (> content 0)
          (++ downv)
        (block))
      
      ;; analyze-left
      (int 7 ((- ghost-x 1) ghost-y) (content))
      (if (> content 0)
          (++ leftv)
        (block))

      ;; analyze lambda man position
      (if (> ghost-x lm-x)
          (++ leftv)
        (block))
      (if (< ghost-x lm-x)
          (++ rightv)
        (block))
      (if (> ghost-y lm-y)
          (++ upv)
        (block))
      (if (< ghost-y lm-y)
          (++ downv)
        (block))
      
      (if (> upv rightv)
          (block (:= bestv upv) (:= bestd +up+))
        (block (:= bestv rightv) (:= bestd +right+)))
      (if (> downv bestv)
          (block (:= bestv downv) (:= bestd +down+))
        (block))
      (if (> leftv bestv)
          (block (:= bestv leftv) (:= bestd +left+))
        (block))

      (block (int 8 (ghost-index bestd) ()))
      (block (int 8 (upv rightv) ()))
      (block (int 8 (downv leftv) ()))

      (block (int 0 (bestd) ()))
      (halt)
      )))


          
      
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
    (+lookahead-num+ 20))
   
   (state
    follow-ind
    last-known-x
    last-known-y
    last-x
    last-y
    (tdirs 32))
   
   (
    (if (= state +seek+)
        (goto seek-prog)
        (if (= state +follow+)
            (goto follow-prog)
            (goto seek-prog)))
    
    (func move-dir (mdx mdy mdir) ()
     (if (= mdir +up+)
         (-- mdy)
         (if (= mdir +down+)
             (++ mdy)
             (block)))
     (if (= mdir +right+)
         (++ mdx)
         (if (= mdir +left+)
             (-- mdx)
             (block))))

    (func nextdir (nx ny ndir)
     (locals ind delta)
     (:= ind 3)
     (:= delta 0)
     nl1
     (:= ndir (& (+ ndir delta) 3))
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
     (++ delta)
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

    (func update-last-known (fx fy)
     (locals ind dir)
     (:= ind follow-ind)
     fl
     (:= dir (val (+ (address tdirs) ind)))
     (call move-dir fx fy dir)
     (if (= mdx tlmx)
         (if (= mdy tlmy)
             (block
                 (:= last-known-x mdx)
                 (:= last-known-y mdy)
                 (:= tnum (+ ind 1))
                 (return))
             (block))
         (block))
     (:= fx mdx)
     (:= fy mdy)
     (if (< ind tnum)
         (block
             (++ ind)
           (goto fl))
         (block)))

    (func seek-after-kill () (sak-moved)
     ;; (block (int 8 (tx ty) ()))
     ;; (block (int 8 (last-x last-y) ()))
     (:= sak-moved 0)
     (if (= tx (+ last-x 1))
         (:= sak-moved 1)
         (if (= tx last-x)
             (block)
             (if (= tx (- last-x 1))
                 (:= sak-moved 1)
                 (:= sak-moved 2))))
     (if (= ty (+ last-y 1))
         (++ sak-moved)
         (if (= ty last-y)
             (block)
             (if (= ty (- last-y 1))
                 (++ sak-moved)
                 (+= sak-moved 2))))
     (:= last-x tx)
     (:= last-y ty)
     (if (> sak-moved 1)
         (block
             (:= state +seek+)
           (goto seek-prog))
         (block)))

    seek-prog
    ;; (block
    ;;     (int 8 (111) ()))
    (block
        (int 3 () (index))
      (int 5 (index) (x y))
      (:= tx x)
      (:= ty y)
      
      (int 3 () (index))
      (int 6 (index) (vit dir))
      (:= tdir dir)

      (int 1 () (x y))
      (:= tlmx x)
      (:= tlmy y))
    (call seek-after-kill)
    (call traverse-from tx ty tlmx tlmy tdir +lookahead-num+)
      
    (if (= tfound 1)
        (block
            ;; (block
            ;;     (int 8 (tnum) ()))
          (:= state +follow+)
          (:= last-known-x tlmx)
          (:= last-known-y tlmy)
          (goto follow-prog))
        (halt))
    follow-prog
    ;; (block
    ;;     (int 8 (222) ()))
    (block
        (int 7 () (x y))
      (:= tlmx x)
      (:= tlmy y))
    (block
        (int 3 () (index))
      (int 5 (index) (x y))
      (:= tx x)
      (:= ty y))
    (call seek-after-kill)
    
    (block
        (int 3 () (index))
      (int 5 (index) (x y))
      (update-last-known x y))
    ;; (block
    ;;     (int 8 (last-known-x last-known-y) ()))
    ;; (block
    ;;     (int 8 (follow-ind tnum) ()))
    (if (= follow-ind tnum)
        (goto seek-prog)
        (if (> follow-ind tnum)
            (block
                (:= state +seek+)
                (goto seek-prog))
            (block)))
    (block
        (locals dir)
      (:= dir (val (+ (address tdirs) follow-ind)))
      (block
          (int 0 (dir)))
      (++ follow-ind)
      (halt))
    )))
