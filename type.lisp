(in-package nock)

(deftype notom ()
  "NOck aTOM: an unsigned integer"
  '(integer 0))

(deftype noun ()
  '(or cons notom))

(deftype nondex ()
  "NOck iIDEX: a value suitable for 0"
  '(integer 1))

(deftype formula ()
  "Nock formula, or Hoon gate: gets a noun, returns a noun."
  '(function (noun) noun))

(deftype noolean ()
  "Nock bOOLEAN"
  '(member 0 1))

(declaim (inline noolify))
(defun noolify (value)
  (if value 0 1))

(defstruct wormula
  "Wrapped fORMULA"
  (formula (error "no formula to wrap") :read-only t :type formula)
  (original (error "no noun to wrap") :read-only t :type noun))

(declaim (inline carn))
(defun carn (noun)
  (let ((thing (car noun)))
    (typecase thing
      (wormula	(wormula-original thing))
      (t	thing))))

(locally
    (declare #.*optimize-speed*)
  (defun eqn (b c)
    (or (eql b c)
        (and (consp b) (consp c)
             (eqn (carn b) (carn c))
             (eqn (cdr b) (cdr c))))))

(defun deworm (noun)
  (etypecase noun
    (cons	(cons (deworm (car noun)) (deworm (cdr noun))))
    (notom	noun)
    (wormula	(deworm (wormula-original noun)))))
