(defstruct vertex-t name entry-ac do-ac exit-ac evt-tbl)

(defun coin () (= 0 (random 2)))

(defun take (seq n)
  "(take seq n) gives the first n elements of the seq. (take seq -n) gives the
  last n elements of the seq. This works on strings as well."
  (check-type seq sequence)
  (check-type n integer)
  (let ((l (length seq)))
    (cond ((>= n 0) (subseq seq 0 (min n l)))
          ((<  n 0) (subseq seq (max 0 (+ n l)) l))
          ((=  n 0) (subseq seq 0 0)))))

(defun drop (seq n)
  "(drop seq n) gives the seq with the first n elements removed. (drop seq -n)
  gives the seq with the last n elements removed. This works on strings as
  well."
  (let ((l (length seq)))
    (check-type seq sequence)
    (check-type n integer)
    (cond ((>= n 0) (subseq seq (min n l) l))
          ((<  n 0) (subseq seq 0 (max 0 (+ n l))))
          ((=  n 0)  seq))))

(defun str-last (str)
  "(str-last non-empty-string) produces the last character in a non-empty
  string."
  (check-type str string)
  (let ((l (length str)))
    (assert (> l 0))
    (subseq str (- l 1) l)))

(defun vertex-1-entry () (print "vertex 1 entry"))
(defun vertex-2-entry () (print "vertex 2 entry"))
(defun vertex-3-entry () (print "vertex 3 entry"))
(defun vertex-4-entry () (print "vertex 4 entry"))

(defun vertex-1-do    () (print "vertex 1 do"))
(defun vertex-2-do    () (print "vertex 2 do"))
(defun vertex-3-do    () (print "vertex 3 do"))
(defun vertex-4-do    () (print "vertex 4 do"))

(defun vertex-1-exit  () (print "vertex 1 exit"))
(defun vertex-2-exit  () (print "vertex 2 exit"))
(defun vertex-3-exit  () (print "vertex 3 exit"))
(defun vertex-4-exit  () (print "vertex 4 exit"))

(defun act-a    () (print "action a" ))
(defun act-b    () (print "action b" ))
(defun act-c    () (print "action c" ))
(defun act-d    () (print "action d" ))
(defun act-na   () (print "action na"))
(defun act-self () (print "self-transition action"))

(defun guard-x     () (coin) )
(defun guard-y     () (coin) )
(defun guard-z     () (coin) )
(defun guard-true  () t      )
(defun guard-false () nil    )
(defun guard-na    () t      )

(defparameter *vertices* nil)
(defmacro defvertex (nym evt-tbl)
  (let* ((dynvar (format nil "*~A*"     nym)) ;; "format nil" means
         (entry  (format nil "~A-entry" nym)) ;; "write to a string"
         (doo    (format nil "~A-do"    nym))
         (exit   (format nil "~A-exit"  nym))
         (vtxsym (with-input-from-string (s dynvar) (read s))))
    `(progn
       (defparameter ,vtxsym
         (make-vertex-t
          :name     (format nil "~A" ,nym)
          :entry-ac (function ,(with-input-from-string (s entry) (read s)))
          :do-ac    (function ,(with-input-from-string (s doo  ) (read s)))
          :exit-ac  (function ,(with-input-from-string (s exit ) (read s)))
          :evt-tbl  ,evt-tbl))
       (push ,vtxsym *vertices*))))

(defvertex "vertex-1"
    '((ev-2 (guard-true act-c    *vertex-3* ))
      (ev-3 (guard-x    act-self *vertex-1* ))  ))
(defvertex "vertex-2"
    '((ev-4 (guard-true act-na   *vertex-1* ))
      (ev-6 (guard-x    act-c    *vertex-4* ))  ))
(defvertex "vertex-3"
    '((ev-1 (guard-x    act-na   nil        )
            (guard-y    act-b    *vertex-1* )
            (guard-z    act-na   *vertex-1* ))
      (ev-5 (guard-na   act-d    *vertex-4* ))  ))
(defvertex "vertex-4"
    '((ev-3 (guard-y    act-d    *vertex-2* ))
      (ev-6 (guard-x    act-c    *vertex-3* ))  ))

(defparameter *current-vertex* *vertex-1*)

(defun eval-first-admissible-triple (triples)
  (cond (triples
         (let* ((triple     (first  triples))
                (guard      (first  triple))
                (action     (second triple))
                (new-vertex (eval (third  triple))))
           (if (and (funcall guard) new-vertex)
               (progn
                 (funcall (vertex-t-exit-ac *current-vertex*))
                 (funcall action)
                 (setf *current-vertex* new-vertex)
                 (funcall (vertex-t-entry-ac *current-vertex*)))
               (progn
                 (format t "~%~A: guard failed; trying next guard"
                         (vertex-t-name *current-vertex*))
                 (eval-first-admissible-triple (rest triples))))))
        (t (format t "~%~A: all guards failed; doing nothing"
                   (vertex-t-name *current-vertex*))))
  *current-vertex*)

(defun sm-engine (event-symbol)
  (let ((line (rest (assoc event-symbol
                           (vertex-t-evt-tbl *current-vertex*)))))
    (if line
        (eval-first-admissible-triple line)
        (progn ;; else
          (format t "~%~A: event ~W not found; doing nothing"
                  (vertex-t-name *current-vertex*)
                  event-symbol)
          *current-vertex*)))) ;; return current vertex in this case

(print (equal *current-vertex* *vertex-1*))
(print (eq *current-vertex* *vertex-1*))
(print (eq (sm-engine 'ev-1) *vertex-1*))
(print (eq (sm-engine 'ev-4) *vertex-1*))
(print (eq (sm-engine 'bogus) *vertex-1*))
(print (eq (sm-engine 'ev-3) *vertex-1*))
(print (eq (sm-engine 'ev-2) *vertex-3*))
