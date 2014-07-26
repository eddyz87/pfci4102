(in-package :pc)

(define-client-constant +wall+ 0)
(define-client-constant +empty+ 1)
(define-client-constant +pill+ 2)
(define-client-constant +power-pill+ 3)
(define-client-constant +fruit+ 4)
(define-client-constant +lm-start-pos+ 5)
(define-client-constant +gh-start-pos+ 6)

(defun inner-lists-to-bin-tries (lsts acc)
  (if lsts
      (let ((current-lst (car lsts)))
        (inner-lists-to-bin-tries (cdr lsts) 
                                  (cons (list-to-bin-trie current-lst +wall+)
                                        acc)))
    (reverse acc)))
   
(defun get-trie-for-map (map)
  (list-to-bin-trie (inner-lists-to-bin-tries map nil) +wall+))

