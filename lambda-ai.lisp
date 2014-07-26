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

(defun pill? (val)
  (if (= val +pill+)
      1
      (if (= val +power-pill+)
          1
          0)))

(defun inner-lists-to-bin-tries (lsts acc)
  (if lsts
      (let ((current-lst (car lsts)))
        (inner-lists-to-bin-tries (cdr lsts) 
                                  (cons (list-to-bin-trie current-lst +wall+)
                                        acc)))
      (reverse acc)))

(defun get-trie-for-map (map)
  (list-to-bin-trie (inner-lists-to-bin-tries map nil)
                    (list-to-bin-trie (cons +wall+ nil) +wall+)))

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


(defun get-map-value (map coord)
  (let ((x (car coord))
        (y (cdr coord)))
    (bin-trie-nth (bin-trie-nth map y) x)))

(defun put-map-value (map coord val)
  (let* ((x (car coord))
         (y (cdr coord))
         (y-trie (bin-trie-nth map y)))
    (bin-trie-nth-update map y (bin-trie-nth-update y-trie x val))))

(defun up-coord (c)
  (cons (car c)
        (- (cdr c) 1)))

(defun down-coord (c)
  (cons (car c)
        (+ (cdr c) 1)))

(defun left-coord (c)
  (cons (- (car c) 1)
        (cdr c)))

(defun right-coord (c)
  (cons (+ (car c) 1)
        (cdr c)))

(defun wave (map front)
  ;; (declare (optimize (debug 3) (safety 3)))
  (labels ((%decode-move (prev-coord coord)
             (let ((px (car prev-coord))
                   (py (cdr prev-coord))
                   (x (car coord))
                   (y (cdr coord)))
               (if (= x px)
                   (if (> y py) +down+ +up+)
                   (if (> x px) +right+ +left+))))
           (%restore-path (map prev-coord coord)
             ;; (format t "restore-path: prev-coord = ~A val = ~A~%" coord (get-map-value map prev-coord))
             (if (= (car (get-map-value map prev-coord))
                    +lambda-man+)
                 (%decode-move prev-coord coord)
                 (%restore-path map (get-map-value map prev-coord) prev-coord)))
           (%try-coord (map front coord move-func search-func)
             ;;(format t "map: ~A~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
             (let* ((new-coord (funcall move-func coord))
                    (val (get-map-value map new-coord)))
               ;; (format t "try-coord: coord = ~A new coord = ~A val = ~A~%"
               ;;         coord new-coord val)
               (if (atom val)
                   (if (= 1 (free? val))
                       (if (= 1 (pill? val))
                           (%restore-path map coord new-coord)
                           (funcall search-func
                                    (put-map-value map new-coord coord)
                                    (queue-put new-coord front)))
                       (funcall search-func map front))
                   (funcall search-func map front)))))
    (if (= 1 (queue-empty? front))
        +no-move+
        (let* ((next (queue-get front))
               (coord (car next))
               (front (cdr next))) 
          (%try-coord
           map front coord #'up-coord
           (lambda (map front)
             (%try-coord
              map front coord #'down-coord
              (lambda (map front)
                (%try-coord
                 map front coord #'left-coord
                 (lambda (map front)
                   (%try-coord
                    map front coord #'right-coord
                    #'wave)))))))))))

(defun world-state-map (ws)
  (car ws))

(defun world-state-lambda-man (ws)
  (car (cdr ws)))

(defun make-wave-step ()
  (lambda (ai-state world-state)
    #-secd(declare (ignore ai-state))
    (let* ((lambda-man-coords (world-state-lambda-man world-state))
           (map (get-trie-for-map (world-state-map world-state)))
           (map (put-map-value map lambda-man-coords
                               (cons +lambda-man+ +lambda-man+))))
      (cons nil (wave map (queue-put lambda-man-coords (make-queue)))))))

(defun wave-main (init-state ghost-programs)
  #-secd(declare (ignore init-state ghost-programs))
  (cons nil (make-wave-step)))

#-secd(defun test-wave ()
  (funcall (make-wave-step)
           nil ;; ai state
           (list '((0 0 0 0 0)
                   (0 1 0 2 0)
                   (0 0 1 0 0)
                   (0 2 1 1 0)
                   (0 0 0 0 0))
                 (cons 2 2))))

;; LIGHTING MAN
(defun lightning-main (init-state ghost-programs)
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
