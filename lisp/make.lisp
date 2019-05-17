;;;
;;; USE:
;;;
;;; Launch a copy of ECL and load this file in it
;;;
;;;	(load "make.lisp")
;;;

(compile-file "sm.lisp" :system-p t :c-file "sm.c" :h-file "sm.h" :data-file "sm.data")
(defconstant +standalone-exe+ (compile-file-pathname "sm" :type :program))
(c::build-program +standalone-exe+
		  :lisp-files
      (list (compile-file-pathname "sm.lisp" :type :object))
      :epilogue-code
		  '(si::quit))
(si::system (format nil "./~A" +standalone-exe+))
(delete-file (compile-file-pathname "sm.lisp" :type :object))
;;; (delete-file +standalone-exe+)
