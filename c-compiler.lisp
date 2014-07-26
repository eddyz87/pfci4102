(in-package :pc)

(defvar *var-access-exprs* (make-hash-table :test #'eq))
(defvar *var-address-exprs* (make-hash-table :test #'eq))
(defvar *available-registers*)

(defvar *current-var-address* 0)

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

(defun compile-c-program (program)
  (let ((*var-access-exprs* (make-hash-table :test #'eq))
        (*var-address-exprs* (make-hash-table :test #'eq))
        (*available-registers* (list 'a 'b 'c 'd 'e 'f 'g 'h))
        (*current-var-address* 0))
    (parse-global-defs (car program))))

