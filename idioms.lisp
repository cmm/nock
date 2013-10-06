;;;; Ignore this

(in-package nock)
(in-readtable lisp-friendly-readtable)

(defmacro define-idiom (name (&rest args) magic-number)
  `(defun ,name (,@args)
     [,magic-number ,@args]))

(defmacro define-idiom-transformer (name (&rest args) magic-number)
  `(defun ,name (,@args .tail.)
     [,magic-number ,@args .tail.]))

(define-idiom-transformer %hint (hint) 10)

(define-idiom-transformer %compose (f) 7)
(define-idiom-transformer %cell-compose (f) 8)
(define-idiom-transformer %apply-core-formula (slot) 9)

(define-idiom %select-subtree (addr) 0)
(defun %id ()
  (load-time-value (%select-subtree 1) t))

(define-idiom %const (value) 1)

(define-idiom-transformer %cell? () 3)
(define-idiom-transformer %inc () 4)
(define-idiom-transformer %eq? () 5)

(define-idiom %application (subject-maker op-maker) 2)
