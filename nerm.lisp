(in-package nock)

(defstruct (nerm (:predicate nermp))
  "Nock tERM"
  (op (error "missing op") :read-only t :type symbol)
  (noun (error "missing noun") :read-only t :type noun)
  (annotation nil :read-only t))

(defmethod make-load-form ((nerm nerm) &optional environment)
  (declare (ignore environment))
  `(make-nerm :op ',(nerm-op nerm) :noun ',(nerm-noun nerm)
              :annotation ',(nerm-annotation nerm)))

(defun nellify (noun &optional innerp)
  (typecase noun
    (wormula	(nellify (wormula-original noun) innerp))
    (cons	(format nil (if innerp "~a ~a" "[~a ~a]")
                        (nellify (car noun))
                        (nellify (cdr noun) t)))
    (t		noun)))

(defmethod print-object ((nerm nerm) stream)
  (format stream (if (eq (readtable-name *readtable*) 'spel)
                     "{~a ~a}"
                     "~a~a")
          (nerm-op nerm) (nellify (nerm-noun nerm))))
