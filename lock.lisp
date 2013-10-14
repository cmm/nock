(in-package nock)
(in-readtable impl)

(deftype noun ()
  '(or cons (integer 0)))

(deftype notom ()
  "NOck aTOM: an unsigned integer"
  '(integer 0))

(deftype nondex ()
  "NOck iIDEX: a value suitable for 0"
  '(integer 1))

(deftype formula ()
  "Nock formula, or Hoon gate: gets a noun, returns a noun.
NIL is identity because checking for it it cheaper than funcalling
#'IDENTITY, go figure."
  '(or (function (noun) (values noun)) null))

(deftype noolean ()
  "Nock bOOLEAN"
  '(member 0 1))

(declaim (inline noolify))
(defun noolify (value)
  (if value 0 1))

;;; We use the native C*R functions instead of the generic TREE-ELT
;;; when we can, because those tend to be implemented using clever
;;; tricks which are unavailable to mere users.  The only wrinkle is
;;; that they are specified to return NIL for absent elements, so we
;;; need to wrap each of them in a valid formula lest we lie to the
;;; compiler.
(defun wrap-shallow-getter (c*r)
  (funcall (compile nil
                    (let ((name (intern (format nil "NOUN-~a" c*r) 'nock)))
                      `(lambda ()
                         (declare (optimize (debug 0) (safety 0) (speed 3)))
                         (named-lambda ,name (.noun.)
                           (declare (type noun .noun.))
                           (or (,c*r .noun.)
                               (nope .noun.
                                     ,(format nil "too shallow for ~a" c*r)))))))))

(defparameter +shallow-accessors+
  (labels ((ADs (i)
             (case i
               (2	'(#\A))
               (3	'(#\D))
               (otherwise (multiple-value-bind (quotent remainder)
                              (floor i 2)
                            (append (ADs (+ 2 remainder)) (ADs quotent)))))))
    (coerce (cons nil (loop :for i :from 2 :below 32
                            :collect (let ((symbol (find-symbol
                                                    (format nil "C~{~a~}R" (ADs i))
                                                    :cl)))
                                       (cons (shallow-getter symbol)
                                             (fdefinition `(setf ,symbol))))))
            'simple-vector)))

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

;;; Wrap FUNCALL to accomodate NIL (which stands in for #'IDENTITY)
(declaim (inline call))
(defun call (function a)
  (if function
      (funcall function a)
      a))

(defun lock (term)
  "The lambda-chaining Nock evaluator."
  (let ((noun (nerm-noun term)))
    (ecase (nerm-op term)
      (*	(call (lockf-formula (cdr noun)) (car noun)))

      (?	(noolify (consp noun)))
      (+	(1+ noun))
      (=	(noolify (equal (car noun) (cdr noun))))
      (/	(tree-elt (car noun) (cdr noun))))))

(defmacro wrap (&body body &environment env)
  `(locally
       (declare (optimize (debug 0) (safety 0) (speed 3)))
     (named-lambda ,(let ((annotation (macroexpand-1 '(annotation) env)))
                      (intern (format nil "RULE-~{~a~^:~}" (reverse annotation))
                              'nock))
         (a)
       (declare (type noun a))
       ,@body)))

(declaim (ftype (function (cons) (values formula)) lock-formula))
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
                                            ((> b (length +shallow-accessors+))
                                             (wrap
                                               (tree-elt b a)))
                                            (t
                                             (car (elt +shallow-accessors+ (1- b)))))))

    ([1 b]			$ 22	(constantly b))

    ([2 (place b) (place c)]	$ 23	(wrap
                                          (call (lock-formula (call (lockf-formula c) a))
                                                (call (lockf-formula b) a))))

    ([3 (place b)]		$ 24	(wrap
                                          (noolify (consp (call (lockf-formula b) a)))))
    ([4 (place b)]		$ 25	(wrap
                                          (1+ (the notom (call (lockf-formula b) a)))))
    ([5 (place b)]		$ 26	(wrap
                                          (let ((noun (call (lockf-formula b) a)))
                                            (noolify (equal (car noun) (cdr noun))))))
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

    ([9 b (place c)]		$ 31	(if (> b (length +shallow-accessors+))

                                            (wrap
                                              (funcall (lambda (core)
                                                         (call (lockf-formula (tree-elt b core))
                                                               core))
                                                       (call (lockf-formula c) a)))

                                            (let* ((accessors (elt +shallow-accessors+ (1- b)))
                                                   (getter (the formula (car accessors)))
                                                   (setter (cdr accessors)))
                                              (wrap
                                                (funcall (lambda (core)
                                                           (call (the formula
                                                                      (let ((formula (call getter core)))
                                                                        (if (consp formula)
                                                                            (funcall setter (lock-formula formula)
                                                                                     core)
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
