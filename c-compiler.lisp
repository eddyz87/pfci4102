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
