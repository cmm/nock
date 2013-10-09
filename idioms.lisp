;;;; Ignore this

(in-package nock)
(in-readtable spel)

(defmacro curry (f &rest args)
  (flet ((form ()
           `[,@(mapcar (lambda (x) `(%K ,x)) (cons f args)) (%I)]))
    (if args
        (form)
        `(load-time-value ,(form) t))))

(defmacro define-idiom (name magic-number &rest args)
  `(progn
     (defun ,name (,@args)
       [,magic-number ,@args])
     (defun ,(intern (concatenate 'string (symbol-name name) "%") 'nock)
         (,@(butlast args))
       (curry ,magic-number ,@(butlast args)))))

(define-idiom %hint 10 hint f)

(define-idiom %compose 7 f g)
(define-idiom %cell-compose 8 f g)
(define-idiom %core-apply 9 slot f)

(define-idiom %subtree 0 idx)

(define-idiom %K 1 value)
(define-idiom %S 2 a b)
(defun %I () (load-time-value (%subtree 1) t))
