(in-package :pc)

(defun make-wall-row (width)
  (loop repeat width collect 0))

(defun make-pills-row (width)
  (loop repeat width collect 2))

(defun gen-grid-maze (width height ghosts-number)
  (let ((grid (loop repeat height collect
                    (loop repeat width collect 0))))
    (loop for i from 1 to (- height 2) do
          (loop for j from 1 to (- width 2) do
                (if (or (evenp i) (not (evenp j)))
                    (setf (nth j (nth i grid)) 2) ;pill
                  (setf (nth j (nth i grid)) 0))))   ;wall
    ;; create ghosts
    (loop repeat ghosts-number do
            (setf (nth (* (random (/ width 2)) 2) 
                       (nth (* (random (/ height 2)) 2) grid))
                  6))
    ;; create fruit
    (setf (nth (* (random (/ width 2)) 2) 
               (nth (* (random (/ height 2)) 2) grid))
          4)
    ;; create lambda man
    (setf (nth (* (random (/ width 2)) 2) 
               (nth (* (random (/ height 2)) 2) grid))
          5)
    grid))

(defun dump-maze (maze stream)
  (dolist (row maze)
    (dolist (cell row)
      (format stream "~A" (case cell
                            (0 #\#)
                            (1 #\Space)
                            (2 #\.)
                            (3 #\o)
                            (4 #\%)
                            (5 #\\)
                            (6 #\=))))
      (format stream "~%")))
          
       