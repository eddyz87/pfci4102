(in-package :pc)

(define-client-constant +wall+ 0)
(define-client-constant +empty+ 1)
(define-client-constant +pill+ 2)
(define-client-constant +power-pill+ 3)
(define-client-constant +fruit+ 4)
(define-client-constant +lm-start-pos+ 5)
(define-client-constant +gh-start-pos+ 6)

(define-client-constant +stay-away-marker+ 102)

(define-client-constant +vitality-standard+ 0)
(define-client-constant +vitality-fright+ 1)
(define-client-constant +vitality-invisible+ 2)

(define-client-constant +up+ 0)
(define-client-constant +down+ 2)
(define-client-constant +left+ 3)
(define-client-constant +right+ 1)

(define-client-constant +lambda-man+ 100)
(define-client-constant +no-move+ 101)
(define-client-constant +frighten-ghost+ 103)

(define-client-constant +frighten-mode-reserve-ticks+ 5)
(define-client-constant +ghost-stay-away-distance+ 4)

(define-client-macro define-tuple (name &rest fields)
  (labels ((%gen (fields top-fields)
             (when fields
               (let* ((field (car fields))
                      (rest-fields (cdr fields))
                      (fname (intern (concatenate 'string (symbol-name name) "-" (symbol-name field))))
                      (cdrs (reduce (lambda (acc f) (declare (ignore f)) `(cdr ,acc))
                                   top-fields
                                   :initial-value 'x)))
                 (cons
                  (if rest-fields
                      `(defun ,fname (x) (car ,cdrs))
                      `(defun ,fname (x) ,cdrs))
                  (%gen rest-fields (cons field top-fields)))))))
    `(progn
       ,@(%gen fields nil))))

(define-client-macro untuple (vars form &rest body)
  (let ((tmp (gensym "tmp")))
    (labels ((%gen (fields accum)
               (if fields
                   (let* ((field (car fields))
                          (rest-fields (cdr fields))
                          (cdrs (reduce (lambda (acc f) (declare (ignore f)) `(cdr ,acc))
                                        accum
                                        :initial-value tmp)))
                     (%gen rest-fields
                           (cons `(,field ,(if rest-fields `(car ,cdrs) cdrs))
                                 accum)))
                   (reverse accum))))
      `(let* ((,tmp ,form)
              ,@(%gen vars nil))
         ,@body))))

(define-tuple world-state
    map lambda-man ghosts fruit)

(define-tuple lambda-man
    vitality coord direction lives score)

(define-tuple ghost
    vitality coord direction)

(define-client-macro mcase (what-expr &rest forms)
  (let ((what (gensym "what")))
    (labels ((%gen (forms)
               (if forms
                   (let ((form (car forms)))
                     (if (eq (car form) 'otherwise)
                         `(progn ,@(cdr form))
                         `(if (= ,what ,(car form))
                              (progn ,@(cdr form))
                              ,(%gen (cdr forms)))))
                   (error "'case' without 'otherwise' final form:~%  ~A" forms))))
      `(let ((,what ,what-expr)) ,(%gen forms)))))

(defun free? (val)
  (mcase val
         (+wall+ 0)
         (+stay-away-marker+ 0)
         (otherwise 1)))

(defun pill? (val)
  (mcase val
         (+pill+ 1)
         (+power-pill+ 1)
;;         (+fruit+ 1)
         (otherwise 0)))

(defun pill-or-fruit? (val fruit-ticks ticks-to-go ghost-hunter power-pills-hunter)
  (if ghost-hunter
      (if (= val +frighten-ghost+) 1 0)
      (if power-pills-hunter
          (if (= val +power-pill+) 1 0)
          (mcase val
                 (+pill+ 1)
                 (+power-pill+ 1)
                 (+fruit+ (if (> fruit-ticks ticks-to-go)
                              1
                              0))
                 ;;         (+fruit+ 1)
                 (otherwise 0)))))

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
  (reduce (lambda (x acc)
            `(cons ,x ,acc))
          args
          :from-end t))

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

(defun wave3 (map front fruit-ticks frighten-ticks power-pills-num)
  #-secd(declare (optimize (debug 3) (safety 3)))
  #-secd(format t "wave3 map:~%~{  ~{~3d ~}~%~}~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
  (let ((orig-map map)
        (longest-path-len 0)
        (longest-path-coords 0)
        (power-pills-hunter (> power-pills-num 2))
        (ghost-hunter (> frighten-ticks (* 137 +frighten-mode-reserve-ticks+))))
    (labels ((%decode-move (prev-coord coord)
               (let ((px (car prev-coord))
                     (py (cdr prev-coord))
                     (x (car coord))
                     (y (cdr coord)))
                 (tuple
                  (if (= x px)
                      (if (> y py) +down+ +up+)
                      (if (> x px) +right+ +left+))
                  coord)))
             (%restore-path (map prev-coord coord)
               ;; (format t "restore-path: prev-coord = ~A val = ~A~%" coord (get-map-value map prev-coord))
               (if (= (car (get-map-value map prev-coord))
                      +lambda-man+)
                   (%decode-move prev-coord coord)
                   (%restore-path map (get-map-value map prev-coord) prev-coord)))
             (%wave (map front)
               (if (= 1 (queue-empty? front))
                   (progn
                     #+secd(dbug 7777)
                     #-secd(format t "rec map:~%~{  ~{~3d ~}~%~}~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
                     #-secd (format t "Trying recovery strategy: len = ~A~%" longest-path-len)
                     (if (> longest-path-len 0)
                         (untuple (coord prev-coord) longest-path-coords
                                  #-secd (format t "Trying recovery strategy: coord = ~A prev-coord = ~A~%"
                                                 coord prev-coord)
                                  (%restore-path map prev-coord coord))
                         (progn
                           #+secd(dbug 8888)
                           (cons +no-move+ (cons 0 0)))))
                   (untuple
                    (step-info front) (queue-get front)
                    (untuple
                     (coord prev-coord len) step-info
                     (let ((val (get-map-value map coord)))
                       ;;#+secd (dbug (cons step-info val))
                       (if (atom val)
                           (if (= 1 (free? val))
                               (progn
                                 (if (> len longest-path-len)
                                     (progn
                                       (setq longest-path-len len)
                                       (setq longest-path-coords (tuple coord prev-coord)))
                                     0)
                                 (if (= 1 (pill-or-fruit? val fruit-ticks (* len 127) ghost-hunter power-pills-hunter))
                                     (%restore-path map prev-coord coord)
                                     (labels ((%update-front (front move-func)
                                                (queue-put (tuple (funcall move-func coord)
                                                                  coord
                                                                  (+ len 1))
                                                           front)))
                                       (%wave (put-map-value map coord prev-coord)
                                              (%update-front
                                               (%update-front
                                                (%update-front
                                                 (%update-front front #'up-coord) #'down-coord) #'left-coord) #'right-coord)))))
                               (%wave map front))
                           (%wave map front))))))))
      (untuple (direction next-coord) (%wave map front)
               (cons (if (= (get-map-value orig-map next-coord)
                            +power-pill+)
                         (- power-pills-num 1)
                         power-pills-num)
                     direction)))))

(defun move-coord (coord direction)
  #-secd(declare (optimize (debug 3) (safety 3)))
  (mcase direction
         (+up+ (up-coord coord))
         (+down+ (down-coord coord))
         (+left+ (left-coord coord))
         (otherwise (right-coord coord))))

(defun wave2 (map front)
  #-secd(declare (optimize (debug 3) (safety 3)))
  #-secd(format t "wave2 map:~%~{  ~{~3d ~}~%~}~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
  (if (= 1 (queue-empty? front))
      map
      (untuple
       (step-info front) (queue-get front)
       (untuple (length1 coord direction) step-info
                (let* ((next-coord (move-coord coord direction))
                       (next-value (get-map-value map next-coord))
                       (map (put-map-value map coord +stay-away-marker+)))
                  #-secd(format t "wave2: dir = ~A c = ~A nc = ~A nv = ~A free? = ~A len = ~A~%"
                                direction coord next-coord next-value (free? next-value) length1)
                  (if (> length1 0)
                      ;; (if (= 1 (free? next-value))
                      ;;     (progn
                      ;;       #-secd (format t ">> taking direction, length = ~A~%" length1)
                      ;;       (wave2 map (queue-put (tuple (- length1 1) next-coord direction)
                      ;;                             front))))
                      (let* ((moves1 (mcase direction
                                            (+up+      (tuple (tuple +up+    +left+ +right+) +down+))
                                            (+down+    (tuple (tuple +down+  +left+ +right+) +up+))
                                            (+left+    (tuple (tuple +left+  +up+   +down+)  +right+))
                                            (otherwise (tuple (tuple +right+ +down+ +up+)    +left+))
                                            ))
                             (moves (car moves1))
                             (opposite (cdr moves1))
                             (good-updates-happened 0))
                        (untuple
                         (m1 m2 m3) moves
                         (labels ((%update-front (front direction)
                                    (let* ((next-coord1 (move-coord coord direction))
                                           (next-value1 (get-map-value map next-coord1)))
                                      #-secd (format t "trying direction ~A nc = ~A nv = ~A free? = ~A~%"
                                                     direction next-coord1 next-value1 (free? next-value1))
                                      (if (= 1 (free? next-value1))
                                          (progn
                                            (setq good-updates-happened 1)
                                            (queue-put (tuple (- length1 1) next-coord1 direction)
                                                       front))
                                          front))))
                           (let ((front (%update-front
                                         (%update-front
                                          (%update-front front m1) m2) m3)))
                             ;;#-secd(format t "<< bad direction~%")
                             (wave2 map
                                    (if (= 1 good-updates-happened)
                                        front
                                        (%update-front front opposite)))))))
                      map))))))

(defun mark-way (map lm-coord coord direction)
  (let* ((lm (get-map-value map lm-coord))
         (map (wave2 (put-map-value map lm-coord +stay-away-marker+)
                     (queue-put (tuple +ghost-stay-away-distance+ coord direction) (make-queue))))
         (map (put-map-value map lm-coord lm)))
    #-secd(format t "map:~%~{  ~{~3d ~}~%~}~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
    map))

(defun mark-ghost-ways (map lm-coord ghosts)
  #-secd(declare (optimize (debug 3)))
  (labels ((%mark (map ghosts)
             (if (null ghosts)
                 map
                 (let ((g (car ghosts)))
                   ;;#+secd(dbug g)
                   (%mark
                    (mcase (ghost-vitality g)
                           (+vitality-standard+ (mark-way map lm-coord (ghost-coord g) (ghost-direction g)))
                           (+vitality-fright+ (put-map-value map (ghost-coord g) +frighten-ghost+))
                           (otherwise map))
                    (cdr ghosts))))))
    (%mark map ghosts)))

(defun count-power-pills (orig-map)
  #+secd (dbug 2222)
  (labels ((%line (cols cnt)
             #-secd (format t "cols = ~A~%" cols)
             (if (null cols)
                 cnt
                 (%line (cdr cols)
                        (if (= (car cols) +power-pill+)
                            (+ 1 cnt)
                            cnt))))
           (%map (lines cnt)
             (if (null lines)
                 cnt
                 (%map (cdr lines) (%line (car lines) cnt)))))
    (%map orig-map 0)))

(defun make-wave-step ()
  (lambda (power-pills-num world-state)
    (let* ((lambda-man (world-state-lambda-man world-state))
           (lambda-man-coords (lambda-man-coord lambda-man))
           (map (get-trie-for-map (car world-state)))
           (map (mark-ghost-ways map lambda-man-coords (world-state-ghosts world-state))))
      #-secd(format t "final map:~%~{  ~{~3d ~}~%~}~%" (mapcar #'bin-trie-to-list (bin-trie-to-list map)))
      ;;(dbug lambda-man-coords)
      (let ((result (wave3 map 
                           (queue-put (tuple lambda-man-coords (cons +lambda-man+ +lambda-man+) 0) (make-queue))
                           (world-state-fruit world-state)
                           (lambda-man-vitality lambda-man)
                           power-pills-num)))
        ;;#+secd(dbug (cons 5555 result))
        result)
      )))

(defun wave-main (init-state ghost-programs)
  #-secd(declare (ignore ghost-programs))
  #+secd (dbug 3333)
  (let ((power-pills-num (count-power-pills (car init-state))))
    #+secd (dbug (cons 4444 power-pills-num))
    (cons power-pills-num
          (make-wave-step))))

#-secd
(defun test-wave ()
  (let ((map '((0 0 0 0 0)  ;; map
               (0 1 3 2 0)
               (0 2 0 1 0)
               (0 1 2 2 0)
               (0 0 0 0 0))))
    (funcall (make-wave-step)
             (count-power-pills map) ;; ai state
             (tuple
              map
              (tuple 0 (cons 1 3) +left+) ;; lambda man
              (list (tuple 0 (cons 3 2) +down+)) ;; ghosts
              nil ;; fruit
              ))))

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
