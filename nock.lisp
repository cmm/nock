(in-package nock)
(in-readtable :standard)

(defvar *nock* 'nock-out)

(defun nock-out (term)
  "The outer Nock evaluator.
Sets things up according to the value of *TRACE*, catches nacks."
  (let ((*reduction-counter* 0)
        (*nock* (case *evaluation-mode*
                  (:lock	#'lock)
                  (:nock	(if *tracedp* #'nock-in/traced #'nock-in)))))
    (catch 'nack
      (funcall *nock* term))))

(defun nock (term)
  "Evaluate a Nock term."
  (funcall *nock* term))

