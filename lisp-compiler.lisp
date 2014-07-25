(in-package :pc)

(deftype lms-instruction () '(or symbol list)
  "symbol - label in the code
          list - (instruction &rest params)")

(defun find-variable (name bindings)
  (loop for i from 0
        for env in bindings do
          (loop for j from 0
                for var in env
                when (eq var name)
                  do (return-from find-variable (list i j))))
  (error "Can't find variable ~A" name))


(defvar *current-program* nil)
(defvar *current-blocks* nil)

(defmacro new-block (return-instruction &body body)
  (let ((label (gensym "label-name")))
    `(let ((*current-program* nil)
           (,label (gensym "label")))
       ,@body
       (push-instruction `(,,return-instruction))
       (push (cons ,label (reverse *current-program*))
             *current-blocks*)
       ,label)))

(defun push-instruction (instr)
  (push instr *current-program*))

(defun lms-compile (term bindings)
  "bindings - list of all environments"
  (optima:match term
    ((list 'if cond then else)
     (lms-compile cond bindings)
     (let* ((then-label (new-block 'join (lms-compile then bindings)))
            (else-label (new-block 'join (lms-compile else bindings))))
       (push-instruction `(sel ,then-label ,else-label))))
    
    ((list* 'lambda params body)
     (let ((label (new-block 'rtn (lms-compile
                                   `(progn ,@body)
                                   (cons params bindings)))))
       (push-instruction `(ldf ,label))))
    
    ((list* 'progn body)
     (dolist (b body)
       (lms-compile b bindings)))
    
    ((list* 'rap function params)
     (dolist (p params)
       (lms-compile p bindings))
     (lms-compile function bindings)
     (push-instruction `(dum ,(length params)))
     (push-instruction `(rap ,(length params))))
    
    ((list* function params)
     (dolist (p params)
       (lms-compile p bindings))
     (case function
       (+ (push-instruction `(add)))
       (- (push-instruction `(sub)))
       (* (push-instruction `(mul)))
       (/ (push-instruction `(div)))
       (= (push-instruction `(ceq)))
       (> (push-instruction `(cgt)))
       (>= (push-instruction `(cgte)))
       (car (push-instruction `(car)))
       (cdr (push-instruction `(cdr)))
       (cons (push-instruction `(cons)))
       (atom (push-instruction `(atom)))
       (otherwise 
        (lms-compile function bindings)
        (push-instruction `(ap ,(length params))))))

    ((guard x (symbolp x))
     (push-instruction `(ld ,@(find-variable x bindings))))

    ((guard x (numberp x))
     (push-instruction `(ld ,x)))

    )
  *current-program*)

(defun compile-lisp (term)
  (let ((*current-program* nil)
        (*current-blocks* nil))
    (lms-compile term '((initial-state ghost-programs)))
    (push-instruction `(rtn))
    (append
     (reverse *current-program*)
     (mapcan #'copy-list *current-blocks*))))
