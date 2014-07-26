(in-package :pc)

(defun handle-instruction (instr labels-table)
  "Replaces labels in arguments to the appropriate instruction's numbers"
  (unless (symbolp instr)
    (mapcar (lambda (arg) 
              (let ((address (gethash arg labels-table)))
                (if address
                    address
                  arg)))
            instr)))

(defun get-labels-table (program)
  "Returns mapping from label name to appropriate instruction's number"
  (let ((labels-table (make-hash-table :test #'eq))
        (next-instr 0))
    (mapc (lambda (instr) (if (symbolp instr)
                              (setf (gethash instr labels-table) next-instr)
                            (incf next-instr)))
          program)
    labels-table))

(defun transform-labels (program)
  "Transform labels to instruction's numbers"
  (let ((labels-table (get-labels-table program)))
    (remove nil
            (mapcar (lambda (x) (handle-instruction x labels-table)) 
                    program))))

