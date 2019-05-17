(defpackage :io.github.rebcabin.test-linqisp
  (:use :common-lisp)
  (:use :lisp-unit)
  (:use :io.github.rebcabin.linqisp)
  #+sbcl (:use :sb-ext))
(in-package :io.github.rebcabin.test-linqisp)

(define-test test-float-test ()
  (assert-float-equal 2.0 (+ 1.0 1.0)))

(define-test type-errors ()
  (assert-error 'type-error
                (take 42 42)))

;;; My first attempt to define a macro that gensyms names for tests did not
;;; succeed.

;;; You can inject tests via macros that write code before run time.

(defmacro inject-tests (empty seq fst lst)
  `(progn
     (let ((len (length ,seq)))

       (assert-equal ,empty (take ,seq 0))

       (assert-equal ,fst (take ,seq  1))
       (assert-equal ,lst (take ,seq -1))

       (assert-equal ,empty (take ,empty  1))
       (assert-equal ,empty (take ,empty -1))
       (assert-equal ,empty (take ,empty  0))

       (assert-equal ,seq (take ,seq     (+ 1 len)))
       (assert-equal ,seq (take ,seq  (- (+ 1 len))))

       (assert-equal ,seq (drop ,seq 0))

       (assert-equal ,fst (drop ,seq (- (- len 1))))
       (assert-equal ,lst (drop ,seq    (- len 1)))

       (assert-equal ,empty (drop ,empty  1))
       (assert-equal ,empty (drop ,empty -1))
       (assert-equal ,empty (drop ,empty  0))

       (assert-equal ,empty (drop ,seq     (+ 1 len)))
       (assert-equal ,empty (drop ,seq  (- (+ 1 len))))

       )))

;;; Or you can just call functions that perform the tests.

(defun perform-tests (empty seq fst lst)
  (progn
    (let ((len (length seq)))

      (assert-equal empty (take seq 0))

      (assert-equal fst (take seq  1))
      (assert-equal lst (take seq -1))

      (assert-equal empty (take empty  1))
      (assert-equal empty (take empty -1))
      (assert-equal empty (take empty  0))

      (assert-equal seq (take seq     (+ 1 len)))
      (assert-equal seq (take seq  (- (+ 1 len))))

      (assert-equal seq (drop seq 0))

      (assert-equal fst (drop seq (- (- len 1))))
      (assert-equal lst (drop seq    (- len 1)))

      (assert-equal empty (drop empty  1))
      (assert-equal empty (drop empty -1))
      (assert-equal empty (drop empty  0))

      (assert-equal empty (drop seq     (+ 1 len)))
      (assert-equal empty (drop seq  (- (+ 1 len))))

      )))

(define-test seq-test-3
  (inject-tests "" "asdf" "a" "f")
  (inject-tests "" "a"    "a" "a")
  (inject-tests "" ""     ""  "" )
  (inject-tests () '(1 2 3 4) '(1) '(4))
  (inject-tests () '(1)       '(1) '(1))
  (inject-tests ()  ()         ()   () )

  (perform-tests "" "asdf" "a" "f")
  (perform-tests "" "a"    "a" "a")
  (perform-tests "" ""     ""  "" )
  (perform-tests () '(1 2 3 4) '(1) '(4))
  (perform-tests () '(1)       '(1) '(1))
  (perform-tests ()  ()         ()   () )
  )

(define-test str-test-1
  (assert-error 'type-error (str-last 42))
  (assert-equal "f" (str-last "asdf"))
  (assert-equal "a" (str-last "a"))
  (assert-equal ""  (str-last ""))
  )

(setf *print-failures* t)

(run-tests :all)
