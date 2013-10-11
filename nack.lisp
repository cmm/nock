(in-package nock)

;;; NACK - out-of-band errors
(defstruct nack
  "Out-of-band error representation."
  (term (error "what term?") :read-only t)
  (annotation nil :read-only t))

(defmethod make-load-form ((nack nack) &optional environment)
  (declare (ignore environment))
  `(make-nack :term ',(nack-term nack)
              :annotation ',(nack-annotation nack)))

(defmethod print-object ((nack nack) stream)
  (when (nack-annotation nack)
    (format stream "~{~a~^:~}  " (nack-annotation nack)))
  (format stream "FAIL: ~a" (nack-term nack)))

(defun nack (term)
  "Signal that the evaluation is not going to terminate."
  (throw 'nack
    (make-nack :term term :annotation *annotation*)))

(defun nope (term why)
  (let ((*annotation* (list why)))
    (nack term)))

