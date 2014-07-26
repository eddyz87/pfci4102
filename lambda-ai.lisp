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

(define-client-constant +ghost-stay-away-distance+ 3)

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
  (if (null lsts)
      (reverse acc)
    (let ((current-lst (car lsts)))
      (inner-lists-to-bin-tries (cdr lsts) 
                                (cons (list-to-bin-trie current-lst +wall+)
                                      acc)))))

(defun get-trie-for-map (map)
  (list-to-bin-trie (inner-lists-to-bin-tries map nil)
                    (list-to-bin-trie (cons +wall+ nil) +wall+)))

(define-client-macro tuple (&rest args)
  (let ((result (reduce (lambda (x acc)
                          (cons x acc))
                        args
                        :from-end t)))
    `(quote ,result)))

;; =======================================================
;; QUEUE IMPLEMENTATIN
(defun make-queue ()
  (cons nil nil))

(defun queue-get (queue)
  (let ((get-lst (car queue))
        (put-lst (cdr queue)))
    (if (null get-lst)
        (if (null put-lst)
            (cons nil queue)
          (queue-get (cons (reverse put-lst) nil)))
      (cons (car get-lst)
            (cons (cdr get-lst) put-lst)))))

(defun queue-put (new-elem queue)
  (let ((get-lst (car queue))
        (put-lst (cdr queue)))
    (cons get-lst (cons new-elem put-lst))))

(defun queue-empty? (queue)
  (if (null (car queue))
      (if (null (cdr queue))
          1
        0)
    0))
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

(defun world-state-lambda-man-status (ws)
  (car (cdr ws)))

(defun world-state-ghost-list (ws)
  (car (cdr (cdr ws))))

(defun world-state-lambda-man-coords (ws)
  (car (cdr (world-state-lambda-man-status ws))))

(defun mark-way (map element coord direction length)
  (if (= 0 length)
      map
      (let* ((next-coord (case direction
                           (+up+      (up-coord coord))
                           (+down+    (down-coord coord))
                           (+left+    (left-coord coord))
                           (otherwise (right-coord coord))))
             (next-value (get-map-value map next-coord)))
        (labels ((%next (map direction)
                   (mark-way (put-map-value map next-coord element)
                             element next-coord direction (- length 1))))
          (if (= 1 (free? next-value))
              (%next map direction)
              (case direction
                (+up+      (%next (%next (%next map +left+) +right+) +down+))
                (+down+    (%next (%next (%next map +left+) +right+) +up+))
                (+left+    (%next (%next (%next map +up+)   +right+) +down+))
                (otherwise (%next (%next (%next map +left+) +up+)    +down+))))))))

(define-client-macro define-tuple (name &rest fields)
  (labels ((%gen (fields)
             (when fields
               (let* ((field (car fields))
                      (rest-fields (cdr fields))
                      (fname (intern (concatenate 'string (symbol-name name) "-" (symbol-name field)))))
                 (cons 
                  `(defun ,fname (x)
                     (car ,(reduce (lambda (acc f) (declare (ignore f)) `(cdr ,acc))
                                   rest-fields
                                   :initial-value 'x)))
                  (%gen rest-fields))))))
    `(progn
       ,@(%gen fields))))

(define-tuple ghost-status vitality coord direction)

(defun mark-ghost-ways (map ghosts)
  (labels ((%mark (map ghosts)
             (if (null ghosts)
                 map
                 (let ((g (car ghosts)))
                   (%mark (mark-way map +wall+
                                    (ghost-status-coord g)
                                    (ghost-status-direction g)
                                    +ghost-stay-away-distance+)
                          (cdr ghosts))))))
    (%mark map ghosts)))

(defun make-wave-step ()
  (lambda (ai-state world-state)
    #-secd(declare (ignore ai-state))
    (let* ((lambda-man-coords (world-state-lambda-man-coords world-state))
           (map (get-trie-for-map (car world-state)))
           (map (mark-ghost-ways map (world-state-ghost-list world-state)))
           (map (put-map-value map lambda-man-coords
                               (cons +lambda-man+ +lambda-man+))))
      ;;(dbug lambda-man-coords)
      (cons nil (wave map (queue-put lambda-man-coords (make-queue))))
      )))

(defun wave-main (init-state ghost-programs)
  #-secd(declare (ignore init-state ghost-programs))
  (cons nil (make-wave-step)))

#-secd
(defun test-wave ()
  (funcall (make-wave-step)
           nil ;; ai state
           (list '((0 0 0 0 0)
                   (0 1 0 2 0)
                   (0 0 1 0 0)
                   (0 2 1 1 0)
                   (0 0 0 0 0))
                 (cons 2 2)
                 (list (tuple 0 (cons 3 3) +left+)))))

;; ;; LIGHTING MAN
;; (defun lightning-main (init-state ghost-programs)
;;   #-secd(declare (ignore init-state ghost-programs))
;;   (cons nil
;;         (lambda (ai-state world-state)
;;           #-secd(declare (ignore ai-state))
;;           (let* ((map (car world-state))
;;                  (lm-status (car (cdr world-state)))
;;                  (lm-coord (car (cdr lm-status)))
;;                  (x (car lm-coord))
;;                  (y (cdr lm-coord))
;;                  (parsed-map (get-trie-for-map map)))
;;             (if (free? (bin-trie-nth (bin-trie-nth parsed-map (- y 1)) x))
;;                 (cons nil +up+)
;;               (if (free? (bin-trie-nth (bin-trie-nth parsed-map y) (+ x 1)))
;;                   (cons nil +right+)
;;                 (if (free? (bin-trie-nth (bin-trie-nth parsed-map (+ y 1)) x))
;;                     (cons nil +down+)
;;                   (cons nil +left+))))))))
