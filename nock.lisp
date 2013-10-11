(in-package nock)
(in-readtable :standard)

(defvar *nock*)

(defun nock-out (term)
  "The outer Nock evaluator.
Sets things up according to the value of *TRACE*, catches nacks."
  (let ((*nock* (if *tracedp* #'nock-in/traced #'nock-in))
        (*reductions* 0))
    (catch 'nack
      (funcall *nock* term))))

(setf *nock* #'nock-out)

(defun nock (term)
  "Evaluate a Nock term."
  (funcall *nock* term))

