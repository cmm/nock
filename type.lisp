(in-package nock)

(deftype notom ()
  "NOck aTOM: an unsigned integer"
  '(integer 0))

(deftype noun ()
  '(or (cons (or cons notom wormula) (or cons notom)) notom))

(deftype nondex ()
  "NOck iIDEX: a value suitable for 0"
  '(integer 1))

(deftype formula-function ()
  '(function (noun) noun))

(deftype formula ()
  "Nock formula, or Hoon gate: gets a noun, returns a noun.
NIL is identity because checking for it it cheaper than funcalling
#'IDENTITY, go figure."
  '(or formula-function null))

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
