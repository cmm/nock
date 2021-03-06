(in-package nock)
(in-readtable spel)

(defmacro define-primitive (name magic-number &rest args)
  `(defun ,name (,@args)
     [,magic-number ,@args]))

(define-primitive %hint 10 hint f)

(define-primitive %cellp 3 f)
(define-primitive %inc 4 f)
(define-primitive %eq 5 f)

(define-primitive %compose 7 f g)
(define-primitive %cell-compose 8 f g)
(define-primitive %core-apply 9 method-elt-idx core-maker)

(define-primitive %elt 0 idx)

(define-primitive %K 1 value)
(define-primitive %S 2 a b)
(defun %I () (load-time-value (%elt 1)))

(define-primitive %if 6 condition then else)

(defun %dec ()
  "The decrementer example from the crash course."
  ;; is that more or less readable than the numeric form?  dunno.
  (load-time-value
   (%cell-compose
    (%K 0)
    (%cell-compose
     (%K (%if (%eq [(%elt 7) (%inc (%elt 6))])
              (%elt 6)
              (%core-apply 2 [(%elt 2) (%inc (%elt 6)) (%elt 7)])))
     (%core-apply 2 (%I))))))
