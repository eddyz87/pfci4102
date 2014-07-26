(in-package :pc)

;; REVERSE
;; ================================
(defun reverse-inner (lst acc)
  (if lst
      (reverse-inner (cdr lst) 
                     (cons (car lst)
                           acc))
    acc))

(defun reverse (lst)
  (reverse-inner lst nil))
;; ===============================

