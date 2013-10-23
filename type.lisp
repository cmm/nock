(in-package nock)
(in-readtable :standard)

(deftype notom ()
  "NOck aTOM: an unsigned integer"
  '(integer 0))

(deftype noun ()
  '(or cons notom))

(deftype nondex ()
  "NOck iIDEX: a value suitable for 0"
  'positive-fixnum)

(deftype formula ()
  "Nock formula, or Hoon gate: gets a noun, returns a noun."
  '(function (noun) noun))

(deftype noolean ()
  "Nock bOOLEAN"
  '(member 0 1))

(declaim (inline noolify))
(defun noolify (value)
  (if value 0 1))

(defstruct worm
  "Wrapped fORMula"
  (formula (error "no formula to wrap") :read-only t :type formula)
  (original (error "no noun to wrap") :read-only t :type noun))

(declaim (inline original))
(defun original (thing)
  (typecase thing
    (worm  (worm-original thing))
    (t     thing)))

(declaim (inline carn))
(defun carn (noun)
  "CAR of Noun.
Compiled formulae are cached by way of replacing the respective noun's
car with a worm.  But we still need the ability to treat the noun as
noun."
  (original (car noun)))

(locally
    (declare #.*optimize-speed*)
  (defun eqn (b c)
    "Equality predicate for nouns.
We cannot just use EQUAL, because of worms."
    (or (eql b c)
        (and (consp b) (consp c)
             (eqn (carn b) (carn c))
             (eqn (cdr b) (cdr c))))))

(defun deworm (noun)
  (etypecase noun
    (cons   (cons (deworm (car noun)) (deworm (cdr noun))))
    (notom  noun)
    (worm   (deworm (worm-original noun)))))
