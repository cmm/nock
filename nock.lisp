(in-package nock)
(in-readtable :standard)

(defvar *nock*)

(defun nock-out (term)
  "The outer Nock evaluator.
Sets things up according to the value of *TRACE*, catches nacks."
  (let ((*reductions* 0)
        (*nock* (cond
                  (*compiledp*	(lambda (term) (funcall (cock term))))
                  (*tracedp*	#'nock-in/traced)
                  (t		#'nock-in))))
    (catch 'nack
      (funcall *nock* term))))

(setf *nock* #'nock-out)

(defun nock (term)
  "Evaluate a Nock term."
  (funcall *nock* term))

