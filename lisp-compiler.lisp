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
            do (return-from find-variable (cons i j))))
  (error "Can't find variable ~A" name))

;; TODO: def-test
;; PC> (FIND-VARIABLE 'a '((b c) (d a) (e f)))
;; (1 . 1)
;; PC> (FIND-VARIABLE 'a '((b c) (g h) (d a) (e f)))
;; (2 . 1)
;; PC> (FIND-VARIABLE 'a '((b c) (g h) (d a1) (e f)))
;; error

(defvar *current-program* nil)
(defvar *current-blocks* nil)

(defmacro new-block (&body body)
  (let ((label (gensym "label-name")))
    `(let ((*current-program* nil)
           (,label (gensym "label")))
       ,@body
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
     (let* ((then-label (new-block (lms-compile then bindings)))
            (else-label (new-block (lms-compile else bindings))))
       (push-instruction `(sel ,then-label ,else-label))))
    
    ((list* 'lambda (list* params) body)
     (let ((label (new-block (lms-compile `(progn ,@body) (cons params bindings)))))
       (push-instruction `(ldf ,label))))
    
    ((list* 'rap function params)
     (dolist (p params)
       (lms-compile p bindings))
     (lms-compile function bindings)
     (push-instruction `(dum ,(length params)))
     (push-instruction `(rap ,(length params))))
    
    ((list* function params)
     (dolist (p params)
       (lms-compile p bindings))
     (lms-compile function bindings)
     (push-instruction `(ap ,(length params))))

    ))
