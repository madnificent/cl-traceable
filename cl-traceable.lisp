(defpackage :cl-traceable
  (:use :common-lisp)
  (:shadow :trace :untrace )
  (:export
   :traceable
   :traceable-lambda
   :trace
   :untrace
   :untrace-all))

(in-package :cl-traceable)

(defmacro removef (symbol list-var)
  "removes <symbol> from the list stored in <list-var>."
  `(setf ,list-var
         (remove ,symbol ,list-var)))

(defvar *traced-symbols* nil)

(defparameter *compile-traceable* t)

(defmacro trace (symbol &rest options)
  "traces whatever is bound to <symbol>.  if a function is bound to <symbol> that is traced through cl:trace."
  `(progn
     (when (fboundp ',symbol)
       (cl:trace ,symbol ,@options))
     (push ',symbol *traced-symbols*)))

(defmacro untrace (symbol)
  "untraces whatever is bound to <symbol>.  if a function is bound to <symbol> that is untraced through cl:trace."
  `(progn
     (when (fboundp ',symbol)
       (cl:untrace ,symbol))
     (removef ',symbol *traced-symbols*)))

(defmacro untrace-all ()
  "untraces everything."
  `(progn
     (cl:untrace)
     (setf *traced-symbols* nil)))

(defmacro with-possible-trace-indent (&body body)
  "performs a macro within a possible environment of trace indentation (can be different for various implementations)"
  #+sbcl
  `(let ((sb-debug::*traced-entries* (cons (cons nil t) sb-debug::*traced-entries*))) ;; this cons stuff is magic to me for now, it may break or be broken
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun show-trace-start (log-name &key name args &allow-other-keys)
  "shows the start of a trace"
  #+sbcl
  (progn
    (sb-debug::print-trace-indentation)
    (format *trace-output* "(~A~{ ~A~})~&" (or name log-name) args))
  #-sbcl
  (format *trace-output* "~&(~A ~{~A~})~&" (or name log-name) args))

(defun show-trace-end (log-name result-list &key name &allow-other-keys)
  "shows the end of a trace"
  #+sbcl
  (progn
    (sb-debug::print-trace-indentation)
    (format *trace-output* "~A returned~{ ~A~}~&" (or name log-name) result-list))
  #-sbcl(format *trace-output* "~&~A returned ~{~A~}~&" (or name log-name) result-list))

(defmacro with-shown-trace ((log-name &rest log-args) &body body)
  (let ((result-list (gensym "result-list")))
    `(progn
       (show-trace-start ',log-name ,@log-args)
       (let ((,result-list (multiple-value-list
                            (with-possible-trace-indent ,@body))))
         (show-trace-end ',log-name ,result-list ,@log-args)
         (apply #'values ,result-list)))))

(defmacro traceable ((log-name &rest log-args) &body body)
  "makes a section of code traceable"
  (if *compile-traceable*
      `(if (member ',log-name *traced-symbols*)
           (with-shown-trace (,log-name ,@log-args)
             ,@body)
           (progn ,@body))
      `(progn ,@body)))

(defmacro traceable-lambda ((&rest args) (log-name &rest log-args) &body body)
  (if *compile-traceable*
      (let ((args-sym (gensym "args")))
        `(lambda (&rest ,args-sym)
           (traceable (,log-name :args ,args-sym ,@log-args)
             (destructuring-bind (,@args)
                 ,args-sym
               ,@body))))
      `(lambda (,@args)
         ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; a running example under sbcl

;; cl-user> (defun mk-summator ()
;;            (cl-traceable:traceable-lambda (a b c)
;;                (mk-summator)
;;              (+ a b c)))
;; mk-summator
;; cl-user> (defun mk-multiplier ()
;;            (cl-traceable:traceable-lambda (&rest args)
;;                (mk-multiplier)
;;              (let ((summator (mk-summator)))
;;                (apply #'* (apply summator args) args))))
;; mk-multiplier
;; cl-user> (mk-multiplier)
;; #<FUNCTION (lambda (&rest #:|args0|) :in mk-multiplier) {1006529E5B}>
;; cl-user> (funcall * 1 2 3)
;; 36
;; cl-user> (trace mk-summator)
;; (mk-summator)
;; cl-user> (cl-traceable:trace mk-summator)
;; warning: mk-summator is already TRACE'd, untracing it first.
;; (mk-summator)
;; cl-user> (cl-traceable:trace mk-multiplier)
;; (mk-multiplier mk-summator)
;; cl-user> (mk-multiplier)
;;   0: (mk-multiplier)
;;   0: mk-multiplier returned
;;        #<FUNCTION (lambda # :in mk-multiplier) {1006529E5B}>
;; #<FUNCTION (lambda (&rest #:|args0|) :in mk-multiplier) {1006529E5B}>
;; cl-user> (funcall * 1 2 3)
;;   0: (mk-multiplier 1 2 3)
;;     1: (mk-summator)
;;     1: mk-summator returned #<FUNCTION (lambda # :in mk-summator) {100646C1AB}>
;;     1: (mk-summator 1 2 3)
;;     1: mk-summator returned 6
;;   0: mk-multiplier returned 36
;; 36
