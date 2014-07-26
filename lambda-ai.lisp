(in-package :pc)

(defconstant +wall+ 0)
(defconstant +empty+ 1)
(defconstant +pill+ 2)
(defconstant +power-pill+ 3)
(defconstant +fruit+ 4)
(defconstant +lm-start-pos+ 5)
(defconstant +gh-start-pos+ 6)

(defun get-trie-for-map (map)
  (list-to-bin-trie (inner-lists-to-bin-tries map nil) +wall+))

(defun inner-lists-to-bin-tries (lsts acc)
  (if lsts
      (let ((current-lst (car lsts)))
        (inner-lists-to-bin-tries (cdr lsts) 
                                  (cons (list-to-bin-trie current-lst +wall+)
                                        acc)))
    (reverse acc)))
   