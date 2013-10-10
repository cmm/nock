;;;; Ignore this

(in-package nock)
(in-readtable spel)

(defmacro define-code (name magic-number &rest args)
  `(defun ,name (,@args)
     [,magic-number ,@args]))

(define-code %hint 10 hint f)

(define-code %cellp 3 f)
(define-code %inc 4 f)
(define-code %eq 5 f)

(define-code %compose 7 f g)
(define-code %cell-compose 8 f g)
(define-code %core-apply 9 method-elt-idx core-maker)

(defun %composify (f)
  (%compose (%id) f))

(define-code %elt 0 idx)

(define-code %K 1 value)
(define-code %S 2 a b)
(defun %I () (load-time-value (%elt 1) t))

(define-code %if 6 condition then else)

(defun %dec ()
  ;; is that more or less readable than the numeric form?  dunno.
  (%cell-compose
   (%K 0)
   (%cell-compose
    (%K (%if (%eq [(%elt 7) (%inc (%elt 6))])
             (%elt 6)
             (%core-apply 2 [(%elt 2) (%inc (%elt 6)) (%elt 7)])))
    (%core-apply 2 (%I)))))
