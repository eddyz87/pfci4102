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

;; =======================================================
;; QUEUE IMPLEMENTATIN
(defun make-queue ()
  (cons nil nil))

(defun queue-get (queue)
  (let ((get-lst (car queue))
        (put-lst (cdr queue)))
    (if get-lst
        (cons (car get-lst)
              (cons (cdr get-lst) put-lst))
      (if put-lst
          (queue-get (cons (reverse put-lst) nil))
        (cons nil queue)))))

(defun queue-put (new-elem queue)
  (let ((get-lst (car queue))
        (put-lst (cdr queue)))
    (cons get-lst (cons new-elem put-lst))))

(defun queue-empty? (queue)
  (if (car queue)
      0
    (if (cdr queue)
        0
      1)))
;; =========================================

