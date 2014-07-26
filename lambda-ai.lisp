(in-package :pc)

(defun get-map-value (map coord)
  (let ((x (car coord))
        (y (cdr coord)))
    (bin-trie-nth (bin-trie-nth x) y)))

(defun wave (map front)
  (labels ((%decode-move (prev-coord coord)
             (let ((px (car prev-coord))
                   (py (cdr prev-coord))
                   (x (car coord))
                   (y (cdr coord)))
               (if (= x px)
                   (if (> y py) +down+ +up+)
                   (if (> x px) +right+ +left+))))
           (%restore-path (map prev-coord coord)
             (if (= (car (get-map-value map prev-coord))
                    +lambda-man+)
                 (%decode-move prev-coord coord)
                 (%restore-path map (get-map-value prev-coord) prev-coord)))
           (%try-coord (map front coord move-func search-func)
             (let ((new-coord (funcall move-func coord))
                   (val (get-map-value coord)))
               (if (free? val)
                   (if (pill? val)
                       (%restore-path map coord new-coord)
                       (funcall search-func
                                (put-map-value map coord new-coord)
                                (queue-put new-coord front)))
                   (funcall search-func map front)))))
    (if (queue-empty? front)
        (cons +no-move+ +no-move+)
        (let ((next (queue-get front))
              (coord (car next))
              (front (cdr next))) 
          (%try-coord
           map front coord up-coord
           (lambda (map front)
             (%try-coord
              map front coord down-coord
              (lambda (map front)
                (%try-coord
                 map front coord left-coord
                 (lambda (map front)
                   (%try-coord
                    map front coord right-coord
                    #'wave)))))))))))
