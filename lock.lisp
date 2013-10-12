(in-package nock)
(in-readtable impl)

(deftype noun ()
  '(or cons (integer 0)))

(deftype notom ()
  '(integer 0))

(deftype nondex ()
  '(integer 1))

(deftype noolean ()
  '(member 0 1))

(deftype formula ()
  '(or (function (noun) *) null))

(declaim (inline boolify))
(defun boolify (value)
  (if value 0 1))

(defmacro gen-accessors ()
  (labels ((ads (i)
             (case i
               (2	'(#\A))
               (3	'(#\D))
               (otherwise (multiple-value-bind (quotent remainder)
                              (floor i 2)
                            (append (ads (+ 2 remainder)) (ads quotent)))))))
    `#(nil ,@(loop :for i :from 2 :below 32
                   :collect (find-symbol
                             (format nil "C~{~a~}R" (ads i))
                             :cl)))))

(defvar +primitive-accessors+ (gen-accessors))

(defun tree-elt (idx tree)
  (declare (optimize (debug 0) (safety 1) (speed 3))
           (type nondex idx))
  (case idx
    (1		tree)
    (2		(car tree))
    (3		(cdr tree))
    (otherwise	(multiple-value-bind (quotent remainder)
                    (floor idx 2)
                  (tree-elt (+ 2 remainder) (tree-elt quotent tree))))))
(defun (setf tree-elt) (new idx tree)
  (declare (optimize (debug 0) (safety 1) (speed 3))
           (type nondex idx))
  (case idx
    (1		new)
    (2		(setf (car tree) new))
    (3		(setf (cdr tree) new))
    (otherwise	(multiple-value-bind (quotent remainder)
                    (floor idx 2)
                  (setf (tree-elt (+ 2 remainder) (tree-elt quotent tree)) new)))))

(defmacro lockf-formula (place &environment env)
  (multiple-value-bind (vars vals new setter getter)
      (get-setf-expansion place env)
    `(the formula
          (let* (,@(mapcar #'list vars vals)
                 (.formula. ,getter))
            (if (consp .formula.)
                (let ((,(car new) (lock-formula .formula.)))
                  ,setter)
                .formula.)))))

(declaim (inline call))
(defun call (function a)
  (if (null function)
      a
      (funcall function a)))

(defun lock (term)
  (let ((noun (nerm-noun term)))
    (ecase (nerm-op term)
      (*	(call (lockf-formula (cdr noun)) (car noun)))

      (?	(boolify (consp noun)))
      (+	(1+ noun))
      (=	(boolify (equal (car noun) (cdr noun))))
      (/	(tree-elt (car noun) (cdr noun))))))

(defmacro wrap (&body body &environment env)
  `(locally
       (declare (optimize (debug 0) (safety 1) (speed 3)))
     (named-lambda ,(intern (format nil "rule-~{~a~^:~}"
                                    (reverse (macroexpand-1 '(annotation) env)))
                            'nock)
         (a)
       (declare (type noun a))
       ,@body)))

(declaim (ftype (function (cons) (or null (function (noun) *))) lock-formula))

(defun lock-formula (noun)
  (declare (optimize (debug 2) (safety 2) (speed 3))
           (type cons noun))
  (ematch noun
    ([(place b) (place c)]
      when (consp b)		$ 19	(wrap
                                          (cons (call (lockf-formula b) a)
                                                (call (lockf-formula c) a))))

    ([0 b]			$ 21	(progn
                                          (check-type b nondex)
                                          (cond
                                            ((= 1 b)
                                             nil)
                                            ((> b (length +primitive-accessors+))
                                             (wrap
                                               (tree-elt b a)))
                                            (t
                                             (fdefinition (elt +primitive-accessors+ (1- b)))))))

    ([1 b]			$ 22	(constantly b))
    ([2 (place b) (place c)]	$ 23	(wrap
                                          (call (lock-formula (call (lockf-formula c) a))
                                                (call (lockf-formula b) a))))
    ([3 (place b)]		$ 24	(wrap
                                          (boolify (consp (call (lockf-formula b) a)))))
    ([4 (place b)]		$ 25	(wrap
                                          (1+ (the notom (call (lockf-formula b) a)))))
    ([5 (place b)]		$ 26	(wrap
                                          (let ((noun (call (lockf-formula b) a)))
                                            (boolify (equal (car noun) (cdr noun))))))
    ([6 (place b)
        (place c)
        (place d)]		$ 28	(wrap
                                          (if (zerop (the noolean (call (lockf-formula b) a)))
                                              (call (lockf-formula c) a)
                                              (call (lockf-formula d) a))))
    ([7 (place b) (place c)]	$ 29	(wrap
                                          (call (lockf-formula c)
                                                (call (lockf-formula b) a))))
    ([8 (place b) (place c)]	$ 30	(wrap
                                          (call (lockf-formula c)
                                                (let ((product (call (lockf-formula b) a)))
                                                  [product a]))))
    ([9 b (place c)]		$ 31	(if (> b (length +primitive-accessors+))
                                            (wrap
                                              (funcall (lambda (core)
                                                         (call (lockf-formula (tree-elt b core))
                                                               core))
                                                       (call (lockf-formula c) a)))
                                            (let* ((symbol (elt +primitive-accessors+ (1- b)))
                                                   (getter (fdefinition symbol))
                                                   (setter (fdefinition `(setf ,symbol))))
                                              (assert symbol)
                                              (wrap
                                                (funcall (lambda (core)
                                                           (call (the formula
                                                                      (let ((formula (funcall getter core)))
                                                                        (if (consp formula)
                                                                            (funcall setter
                                                                                     (lock-formula formula) core)
                                                                            formula)))
                                                                 core))
                                                         (call (lockf-formula c) a))))))
    ([10 [_ (place c)]
         (place d)]		$ 32	(wrap
                                          (call (lockf-formula c) a)
                                          (call (lockf-formula d) a)))
    ;; TODO jets
    ([10 _ (place c)]		$ 33	(wrap
                                          (call (lockf-formula c) a)))))
