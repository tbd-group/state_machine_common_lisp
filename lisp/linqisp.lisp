(in-package :io.github.rebcabin.linqisp)

(defun take (seq n)
  "(take seq n) produces the first n elements of the seq. (take seq -n) produces
  the last n elements of the seq. This works on strings as well as on lists."
  (check-type seq sequence)
  (check-type n integer)
  (let ((l (length seq)))
    (cond ((>= n 0) (subseq seq 0 (min n l)))
          ((<  n 0) (subseq seq (max 0 (+ n l)) l))
          ((=  n 0) (subseq seq 0 0)))))

(defun drop (seq n)
  "(drop seq n) produces a the seq with the first n elements removed. (drop seq
  -n) produces the seq with the last n elements removed. This works on strings
  as well as on lists."
  (let ((l (length seq)))
    (check-type seq sequence)
    (check-type n integer)
    (cond ((>= n 0) (subseq seq (min n l) l))
          ((<  n 0) (subseq seq 0 (max 0 (+ n l))))
          ((=  n 0)  seq))))

(defun str-last (str)
  "(str-last str) produces a string containing the last character of the input
  string, or the empty string if str is the empty string."
  (check-type str string)
  (let ((l (length str)))
    (cond ((> l 0) (subseq str (- l 1) l))
          ((= l 0) "")
          (t (assert (>= l 0))))))
