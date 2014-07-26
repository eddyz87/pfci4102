(in-package :pc)

(defun dump (program stream)
  (dolist (instr program)
    (format stream "~A ~{~A~^ ~}~%" (car instr) (cdr instr))))

