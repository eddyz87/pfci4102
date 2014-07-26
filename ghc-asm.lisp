(in-package :pc)

(defun get-op-instr (sym)
  (case sym
    (+ 'add)
    (- 'sub)
    (* 'mul)
    (/ 'div)
    (& 'and)
    (or 'or)
    (xor 'xor)
    ;; During processing of assignment operators target is nil
    (+= 'add)
    (-= 'sub)
    (*= 'mul)
    (/= 'div)
    (&= 'and)
    (or= 'or)
    (xor= 'xor)
    (:= 'mov)
    (otherwise nil)))

(defvar *current-instructions* nil)

(defun hash-tab-from-alist (alist)
  (let ((tab (make-hash-table :test #'eq)))
    (loop for (key . val) in alist
         do (setf (gethash key tab) val))
    tab))

(defun ghc-compile-expr (expr target available-registers var-access-exprs var-address-exprs)
  (if (atom expr)
      (if (symbolp expr)
          (car (gethash expr var-access-exprs))
          expr)
      (let ((instr (get-op-instr (car expr))))
        (if instr
            (progn
              (when (null available-registers)
                (error "Cannont compile exprs"))
              (let* ((tval (ghc-compile-expr (second expr) target available-registers
                                             var-access-exprs var-address-exprs))
                     (reg1 (ghc-compile-expr (third expr) (car available-registers) (cdr available-registers)
                                             var-access-exprs var-address-exprs)))
                (if target
                    (progn
                      (when (and (not (eq tval target)))
                        (push `(mov ,target ,tval)
                              *current-instructions*))
                      (push `(,instr ,target ,reg1)
                            *current-instructions*)
                      target)
                    (progn
                      (push `(,instr ,tval ,reg1)
                            *current-instructions*)
                      tval))))
            (cond ((eq (first expr) 'address)
                   (gethash (second expr) var-address-exprs))
                  ((eq (first expr) 'val)
                   (let ((val-inner (ghc-compile-expr (second expr) target available-registers
                                                      var-access-exprs var-address-exprs)))
                     (if (symbolp val-inner)
                         (list val-inner)
                         (progn
                           (push `(mov ,target ,val-inner)
                                 *current-instructions*)
                           target)))))))))
