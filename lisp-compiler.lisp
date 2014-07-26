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

(defvar *macro-forms* (make-hash-table :test #'eq))

(defmacro define-client-macro (name params &body body)
  `(progn
    (setf (gethash ',name *macro-forms*) (lambda ,params ,@body))))

(defvar *client-constants* (make-hash-table :test #'eq))

(defmacro define-client-constant (name value)
  `(progn
     (defconstant ,name ,value)
     (setf (gethash ,name *client-constants*) ,value)))

(defmacro new-block (return-instruction &body body)
  (let ((label (gensym "label-name")))
    `(let ((*current-program* nil)
           (,label (gensym "label")))
       ,@body
       (push-instruction `(,,return-instruction))
       (push (cons ,label (reverse *current-program*))
             *current-blocks*)
       ,label)))

(defun change-top-instr-to (val)
  (setf *current-program*
        (cons (cons val (cdr (first *current-program*)))
              (cdr *current-program*))))

(defun push-instruction (instr)
  (cond 
    ((and (eq (first instr) 'join)
          (eq (first (car *current-program*))
              'sel))
     (change-top-instr-to 'tsel))
    ((and (eq (first instr) 'rtn)
          (eq (first (car *current-program*))
              'ap))
     (change-top-instr-to 'tap))
    ((and (eq (first instr) 'rtn)
          (eq (first (car *current-program*))
              'rap))
     (change-top-instr-to 'trap))
    (t (push instr *current-program*))))

(defun client-macroexpand-1 (term)
  (optima:match term 
    ((guard (list* function _) (gethash function *macro-forms*))
     (apply (gethash function *macro-forms*) (cdr term)))
    (x x)))

(defun lms-compile (term bindings)
  "bindings - list of all environments"
  (optima:match term
    ((list 'if cond then else)
     (lms-compile cond bindings)
     (let* ((then-label (new-block 'join (lms-compile then bindings)))
            (else-label (new-block 'join (lms-compile else bindings))))
       (push-instruction `(sel ,then-label ,else-label))))

    ((list 'setq var value)
     (lms-compile value bindings)
     (push-instruction `(st ,@(find-variable var bindings))))
    
    ((list* 'lambda params body)
     (let ((label (new-block 'rtn (lms-compile
                                   `(progn ,@body)
                                   (cons params bindings)))))
       (push-instruction `(ldf ,label))))
    
    ((list* 'progn body)
     (dolist (b body)
       (lms-compile b bindings)))
    
    ((list* 'rap function params)
     (push-instruction `(dum ,(length params)))
     (dolist (p params)
       (lms-compile p bindings))
     (lms-compile function bindings)
     (push-instruction `(rap ,(length params))))

    ((guard (list* function _) (gethash function *macro-forms*))
     (lms-compile (client-macroexpand-1 term) bindings))
    
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
       (dbug (push-instruction `(dbug)))
       (brk (push-instruction `(brk)))
       (otherwise
        (lms-compile function bindings)
        (push-instruction `(ap ,(length params)))
        )))

    ((guard x (symbolp x))
     (let ((const (gethash x *client-constants*)))
       (push-instruction (if const
                             `(ld ,const)
                             `(ld ,@(find-variable x bindings))))))

    ((guard x (numberp x))
     (push-instruction `(ldc ,x)))

    )
  *current-program*)

(defun compile-lisp (term &optional (top-names '(initial-state ghost-programs)))
  (let ((*current-program* nil)
        (*current-blocks* nil))
    (lms-compile term (list top-names))
    (push-instruction `(rtn))
    (append
     (reverse *current-program*)
     (mapcan #'copy-list *current-blocks*))))

(defun compile-lisp-top (forms)
  (let* ((main-params nil)
         (main-body nil)
         (bodies nil))
    (dolist (form forms)
      (if (eq (second form)
              'main)
          (progn
            (setf main-params (third form))
            (setf main-body (cdddr form)))
          (push (cdr form) bodies)))
    (compile-lisp `(labels ,bodies ,@main-body) main-params)))

(define-client-macro test-macro (a b)
  (format t "bzzzzz~%")
  `(+ ,a ,b))

(define-client-macro labels (forms &rest body)
  (let ((param-names (mapcar #'first forms))
        (param-forms (mapcar (lambda (x) (cons 'lambda x))
                             (mapcar #'cdr forms))))
    `(rap (lambda ,param-names ,@body) ,@param-forms)))

(define-client-macro let (forms &rest body)
  (let ((param-names (mapcar #'first forms))
        (param-forms (mapcar (lambda (x) (cons 'lambda x))
                             (mapcar #'cdr forms))))
    `((lambda ,param-names ,@body) ,@param-forms)))

(define-client-macro let* (forms &rest body)
  (if (null (cdr forms))
      `(let ,forms ,@body)
      `(let (,(car forms)) (let* ,(cdr forms) ,@body))))

(define-client-macro null (val)
  `(= ,val 0))
