cl-traceable
============

Allows you to write lambda functions which can be traced under SBCL.  Lambda functions keep working in other distributions.

A running example under sbcl:


   cl-user> (defun mk-summator ()
               (cl-traceable:traceable-lambda (a b c)
                   (mk-summator)
                 (+ a b c)))
    mk-summator

    cl-user> (defun mk-multiplier ()
               (cl-traceable:traceable-lambda (&rest args)
                   (mk-multiplier)
                 (let ((summator (mk-summator)))
                   (apply #'* (apply summator args) args))))
    mk-multiplier

    cl-user> (mk-multiplier)
    #<FUNCTION (lambda (&rest #:|args0|) :in mk-multiplier) {1006529E5B}>

    cl-user> (funcall * 1 2 3)
    36

    cl-user> (trace mk-summator)
    (mk-summator)

    cl-user> (cl-traceable:trace mk-summator)
    warning: mk-summator is already TRACE'd, untracing it first.
    (mk-summator)

    cl-user> (cl-traceable:trace mk-multiplier)
    (mk-multiplier mk-summator)

    cl-user> (mk-multiplier)
      0: (mk-multiplier)
      0: mk-multiplier returned
           #<FUNCTION (lambda # :in mk-multiplier) {1006529E5B}>
    #<FUNCTION (lambda (&rest #:|args0|) :in mk-multiplier) {1006529E5B}>

    cl-user> (funcall * 1 2 3)
      0: (mk-multiplier 1 2 3)
        1: (mk-summator)
        1: mk-summator returned #<FUNCTION (lambda # :in mk-summator) {100646C1AB}>
        1: (mk-summator 1 2 3)
        1: mk-summator returned 6
      0: mk-multiplier returned 36
    36
