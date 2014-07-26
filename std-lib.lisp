(in-package :pc)

;; Macros
(define-client-macro labels (forms &rest body)
  (let ((param-names (mapcar #'first forms))
        (param-forms (mapcar (lambda (x) (cons 'lambda x))
                             (mapcar #'cdr forms))))
    `(rap (lambda ,param-names ,@body) ,@param-forms)))

(define-client-macro let (forms &rest body)
  (let ((param-names (mapcar #'first forms))
        (param-forms (mapcar (lambda (x) (cons 'progn x))
                             (mapcar #'cdr forms))))
    `((lambda ,param-names ,@body) ,@param-forms)))

(define-client-macro let* (forms &rest body)
  (if (null (cdr forms))
      `(let ,forms ,@body)
      `(let (,(car forms)) (let* ,(cdr forms) ,@body))))

(define-client-macro null (val)
  `(if (atom ,val)
       1
     0))

(define-client-constant nil 0)

(define-client-macro funcall (func &rest args)
  `(,func ,@args))

(define-client-macro function (func)
  func)


;; REVERSE
;; ================================
(defun reverse-inner (lst acc)
  (if (null lst)
      acc
    (reverse-inner (cdr lst) 
                   (cons (car lst)
                         acc))))

(defun reverse (lst)
  (reverse-inner lst nil))
;; ===============================

;; LENGTH
(defun length (lst)
  (labels ((%inner (lst acc)
             (if (null lst)
                 acc
                 (%inner (cdr lst) (+ acc 1)))))
    (%inner lst 0)))
