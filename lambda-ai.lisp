(in-package :pc)

(define-client-constant +wall+ 0)
(define-client-constant +empty+ 1)
(define-client-constant +pill+ 2)
(define-client-constant +power-pill+ 3)
(define-client-constant +fruit+ 4)
(define-client-constant +lm-start-pos+ 5)
(define-client-constant +gh-start-pos+ 6)

(define-client-constant +up+ 0)
(define-client-constant +down+ 2)
(define-client-constant +left+ 3)
(define-client-constant +right+ 1)

(define-client-constant +lambda-man+ 100)
(define-client-constant +no-move+ 101)

(defun free? (val)
  (if (= val +pill+)
      1
      (if (= val +power-pill+)
          1
          (if (= val +empty+)
              1
              0))))

;; (defun pill? (val)
;;   (if (= val +pill+)
;;       1
;;       (if (= val +power-pill+)
;;           1
;;           0)
;;       0))

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


;; (defun get-map-value (map coord)
;;   (let ((x (car coord))
;;         (y (cdr coord)))
;;     (bin-trie-nth (bin-trie-nth x) y)))

;; (defun wave (map front)
;;   (labels ((%decode-move (prev-coord coord)
;;              (let ((px (car prev-coord))
;;                    (py (cdr prev-coord))
;;                    (x (car coord))
;;                    (y (cdr coord)))
;;                (if (= x px)
;;                    (if (> y py) +down+ +up+)
;;                    (if (> x px) +right+ +left+))))
;;            (%restore-path (map prev-coord coord)
;;              (if (= (car (get-map-value map prev-coord))
;;                     +lambda-man+)
;;                  (%decode-move prev-coord coord)
;;                  (%restore-path map (get-map-value prev-coord) prev-coord)))
;;            (%try-coord (map front coord move-func search-func)
;;              (let ((new-coord (funcall move-func coord))
;;                    (val (get-map-value coord)))
;;                (if (= 1 (free? val))
;;                    (if (= 1 (pill? val))
;;                        (%restore-path map coord new-coord)
;;                        (funcall search-func
;;                                 (put-map-value map coord new-coord)
;;                                 (queue-put new-coord front)))
;;                    (funcall search-func map front)))))
;;     (if (= 1 (queue-empty? front))
;;         (cons +no-move+ +no-move+)
;;         (let ((next (queue-get front))
;;               (coord (car next))
;;               (front (cdr next))) 
;;           (%try-coord
;;            map front coord up-coord
;;            (lambda (map front)
;;              (%try-coord
;;               map front coord down-coord
;;               (lambda (map front)
;;                 (%try-coord
;;                  map front coord left-coord
;;                  (lambda (map front)
;;                    (%try-coord
;;                     map front coord right-coord
;;                     #'wave)))))))))))

(define-client-macro return-direction (direction)
  `(cons nil ,direction))

;; LIGHTING MAN
(defun main (init-state ghost-programs)
  (cons nil
        (lambda (ai-state world-state)
          (let ((map (car world-state))
                (lm-status (car (cdr world-state))))
            (let ((lm-coord (car (cdr lm-status))))
              (let ((x (car lm-coord))
                    (y (cdr lm-coord))
                    (parsed-map (get-trie-for-map map)))
                (if (free? (bin-trie-nth (bin-trie-nth parsed-map (- y 1)) x))
                    (cons nil +up+)
                  (if (free? (bin-trie-nth (bin-trie-nth parsed-map y) (+ x 1)))
                      (cons nil +right+)
                    (if (free? (bin-trie-nth (bin-trie-nth parsed-map (+ y 1)) x))
                        (cons nil +down+)
                      (cons nil +left+))))))))))
                
                             