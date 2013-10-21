(in-package nock)
(in-readtable :standard)

(defstruct (nerm (:predicate nermp))
  "Nock tERM"
  (op (error "missing op") :read-only t :type symbol)
  (noun (error "missing noun") :read-only t :type noun)
  (annotation nil :read-only t))

(defmethod make-load-form ((nerm nerm) &optional environment)
  (declare (ignore environment))
  `(make-nerm :op ',(nerm-op nerm) :noun ',(nerm-noun nerm)
              :annotation ',(nerm-annotation nerm)))

(defun nell-string (noun &optional innerp)
  (typecase noun
    (cons	(format nil (if innerp "~a ~a" "[~a ~a]")
                        (nell-string (car noun))
                        (nell-string (cdr noun) t)))
    (t		noun)))

(defmethod print-object ((nerm nerm) stream)
  (format stream (if (eq (readtable-name *readtable*) 'spel)
                     "{~a ~a}"
                     "~a~a")
          (nerm-op nerm) (nell-string (deworm (nerm-noun nerm)))))
