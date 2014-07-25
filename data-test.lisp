(in-package :pc)


(defun max-2-power (num)
  (labels ((%max-pow (acc)
             (if (>= acc num)
                 acc
                 (%max-pow (* 2 acc)))))
    (%max-pow 1)))

(defun list-to-bin-trie-inner (size lst fill)
  (if (= size 1)
      (if (null lst)
          (cons fill lst)
          lst)
      (let* ((res1 (list-to-bin-trie-inner (/ size 2) lst fill))
             (res2 (list-to-bin-trie-inner (/ size 2) (cdr res1) fill)))
        (cons (cons (car res1)
                    (car res2))
              (cdr res2)))))

(defun list-to-bin-trie (lst default-fill)
  (let ((max-pow (max-2-power (length lst))))
    (cons (car (list-to-bin-trie-inner max-pow lst default-fill))
          max-pow)))

(defun bin-trie-nth-inner (trie trie-size el)
  (if (atom trie)
      trie
      (if (= trie-size 1)
          trie
          (let ((half-size (/ trie-size 2)))
            (if (>= el half-size)
                (bin-trie-nth-inner (cdr trie) half-size (- el half-size))
                (bin-trie-nth-inner (car trie) half-size el))))))

(defun bin-trie-nth (trie el)
  (bin-trie-nth-inner (car trie) (cdr trie) el))

(defun bin-trie-nth-update-inner (trie trie-size el val)
  (labels ((%choose-half (trie)
             (let ((half-size (/ trie-size 2)))
               (if (>= el half-size)
                   (cons (car trie) (bin-trie-nth-update-inner (cdr trie) half-size (- el half-size) val))
                   (cons (bin-trie-nth-update-inner (car trie) half-size el val) (cdr trie))))))
    (if (= trie-size 1)
        val
        (if (atom trie)
            (%choose-half (cons trie trie))
            (%choose-half trie)))))

(defun bin-trie-nth-update (trie el val)
  (cons (bin-trie-nth-update-inner (car trie) (cdr trie) el val)
        (cdr trie)))
