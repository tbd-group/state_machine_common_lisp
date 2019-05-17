(defpackage :io.github.rebcabin.temporary
  (:use :common-lisp)
  (:use :lisp-unit)
  #+sbcl (:use :sb-ext))
(in-package :io.github.rebcabin.temporary)

;;; I can definitely define a function with a gensymmed name:

(defmacro m1 ()
  (let ((g (gensym)))
    `(progn
       (defun ,g ()
         (format t "my name is ~A~%" ',g))
       (,g))))

(m1)

;;; I can definitely run tests via lisp-unit:

(define-test dt
    (assert-true t))

;;; I can definitely NOT define a test using a gensymmed name. This compiles,
;;; but the test does not run.

(defmacro m2 ()
  (let ((g (gensym)))
    `(progn
       (define-test ,g
           (format t "my name is ~A~%" ',g))
       )))

(m2)

(setf *print-failures* t)

(run-tests :all)
