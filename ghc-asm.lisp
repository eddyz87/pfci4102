(in-package :pc)

(defvar *label-target* nil)

(declaim (ftype (function (t) t) push-ghc-instruction))

(defun get-op-instr (sym)
  (case sym
    (+ '(add))
    (- '(sub))
    (* '(mul))
    (/ '(div))
    (& '(and))
    (or '(or))
    (xor '(xor))
    ;; Conditionals
    (< `(jlt ,*label-target*))
    (= `(jeq ,*label-target*))
    (> `(jgt ,*label-target*))
    ;; During processing of assignment operators target is nil
    (+= '(add))
    (-= '(sub))
    (*= '(mul))
    (/= '(div))
    (&= '(and))
    (or= '(or))
    (xor= '(xor))
    (:= '(mov))
    (++ '(inc))
    (-- '(dec))
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
                (error "Cannot compile exprs"))
              (case (length (cdr expr))
                (2 (let* ((tval (ghc-compile-expr (second expr) target available-registers
                                                  var-access-exprs var-address-exprs))
                          (reg1 (ghc-compile-expr (third expr) (car available-registers) (cdr available-registers)
                                                  var-access-exprs var-address-exprs)))
                     (if target
                         (progn
                           (when (and (not (eq tval target)))
                             (push-ghc-instruction `(mov ,target ,tval)))
                           (push-ghc-instruction `(,@instr ,target ,reg1))
                           target)
                         (progn
                           (push-ghc-instruction `(,@instr ,tval ,reg1))
                           tval))))
                (1 (let* ((tval (ghc-compile-expr (second expr) (car available-registers) (cdr available-registers)
                                                  var-access-exprs var-address-exprs)))
                     (push-ghc-instruction `(,@instr ,tval))
                     tval))))
            (cond ((eq (first expr) 'address)
                   (gethash (second expr) var-address-exprs))
                  ((eq (first expr) 'label)
                   (second expr))
                  ((eq (first expr) 'val)
                   (let ((val-inner (ghc-compile-expr (second expr) (car available-registers) (cdr available-registers)
                                                      var-access-exprs var-address-exprs)))
                     (if (symbolp val-inner)
                         (list val-inner)
                         (progn
                           (push-ghc-instruction `(mov ,target ,val-inner))
                           target)))))))))

(defun ghc-asm-dump (body stream)
  (labels ((%param-to-string (par)
             (cond ((symbolp par)
                    (string-downcase (symbol-name par)))
                   ((integerp par)
                    (format nil "~A" par))
                   ((listp par)
                    (format nil "[~A]" (%param-to-string (first par)))))))
    (dolist (instr body)
      (if (listp instr)
          (format stream "~A ~{~A~^,~}~%"
                  (string-downcase (symbol-name (car instr)))
                  (mapcar #'%param-to-string (cdr instr)))
          (format stream "~A~%"
                  (string-downcase (symbol-name instr)))))
    (format *error-output* "Dumped ~A instructions~%" (length body))))
