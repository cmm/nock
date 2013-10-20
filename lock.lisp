(in-package nock)
(in-readtable impl)

(defconstant +inline-tree-accessor-max+ 256)

(defun generate-tree-access (idx arg)
  (case idx
    (2		`(carn ,arg))
    (3		`(cdr ,arg))
    (otherwise	(multiple-value-bind (quotent remainder)
                    (floor idx 2)
                  (generate-tree-access (+ 2 remainder)
                                        (generate-tree-access quotent arg))))))

(defun compile* (form)
  (funcall (compile nil `(lambda () ,form))))

(defparameter +shallow-tree-accessors+
  (let ((i 2)
        (res (make-array (list +inline-tree-accessor-max+))))
    (map-into res
              (lambda ()
                (prog1
                    (let ((name (intern (format nil "/~d" i) 'nock)))
                      (compile*
                       `(locally
                            (declare (optimize (debug 0) (safety 0) (speed 3)))
                          (named-lambda ,name (.noun.)
                            (declare (type noun .noun.))
                            ,(generate-tree-access i '.noun.)))))
                  (incf i))))))

(declaim (ftype (function (nondex) formula) tree-accessor))
(defun tree-accessor (idx)
  (cond
    ((= idx 1)
     nil)
    ((<= (1+ idx) +inline-tree-accessor-max+)
     (the formula-function (elt +shallow-tree-accessors+ (- idx 2))))
    (t
     (locally
         (declare (optimize (debug 0) (safety 0) (speed 3))
                  (type nondex idx))
       (labels ((tree-elt (idx tree)
                  (cond
                    ((< idx +inline-tree-accessor-max+)
                     (locally (declare (type fixnum idx))
                       (funcall (the formula-function (elt +shallow-tree-accessors+ (- idx 2)))
                                tree)))
                    (t (multiple-value-bind (quotent remainder)
                           (floor idx 2)
                         (tree-elt (+ 2 remainder)
                                   (tree-elt quotent tree)))))))
         (lambda (tree)
           (tree-elt idx tree)))))))

;;; Wrap FUNCALL to accomodate NIL (which stands in for #'IDENTITY)
(declaim (inline call))
(defun call (function a)
  (if function
      (funcall (the formula-function function) a)
      a))

(defun lock (term)
  "The lambda-chaining Nock evaluator."
  (let ((noun (nerm-noun term)))
    (ecase (nerm-op term)
      (*	(call (lockf-formula (cdr noun)) (car noun)))

      (?	(noolify (consp noun)))
      (+	(1+ noun))
      (=	(noolify (equal (carn noun) (cdr noun))))
      (/	(funcall (tree-accessor (carn noun)) (cdr noun))))))

(defun lockf-formula (noun)
  (typecase (car noun)
    (wormula	(wormula-formula (car noun)))
    (t		(let ((formula (lock-formula noun)))
                  (prog1 formula
                    (setf (car noun)
                          (make-wormula :original (car noun)
                                        :formula formula)))))))

(locally
    (declare (optimize (debug 0) (safety 0) (speed 3)))
  (defun eqn (b c)
    (or (eql b c)
        (and (consp b) (consp c)
             (eqn (carn b) (carn c))
             (eqn (cdr b) (cdr c))))))

(defun lock-formula (noun)
  (let ((code (lock-fragment noun 'a)))
    (etypecase code
      (null
       nil)
      (function
       code)
      (symbol
       (fdefinition code))
      (cons
       (compile*
        `(locally
             (declare (optimize (debug 0) (safety 0) (speed 3)))
           (lambda (a)
             ,code)))))))

(defun deworm (noun)
  (etypecase noun
    (cons	(cons (deworm (car noun)) (deworm (cdr noun))))
    (notom	noun)
    (wormula	(deworm (wormula-original noun)))))

(defun lock-call (noun arg)
  (let ((code (lock-fragment (deworm noun) arg)))
    (etypecase code
      (null
       arg)
      (function
       `(funcall ,code ,arg))
      (symbol
       `(,code ,arg))
      ((or notom cons)
       code))))

(defun lock-fragment (noun arg)
  (ematch noun
    ([b c] when (consp b)	$ 19	`(cons ,(lock-call b arg)
                                               ,(lock-call c arg)))

    ([0 b]			$ 21	(progn
                                          (check-type b nondex)
                                          (tree-accessor b)))

    ([1 b]			$ 22	`(quote ,b))

    ([2 b c]			$ 23	`(call (lockf-formula ,(lock-call c arg))
                                               ,(lock-call b arg)))

    ([3 b]			$ 24	`(noolify (consp ,(lock-call b arg))))
    ([4 b]			$ 25	`(let ((notom ,(lock-call b arg)))
                                           (check-type notom number)
                                           (1+ notom)))
    ([5 b]			$ 26	`(let ((noun ,(lock-call b arg)))
                                           (check-type noun cons)
                                           (noolify (eqn (carn noun) (cdr noun)))))
    ([6 b c d]			$ 28	`(let ((condition ,(lock-call b arg)))
                                           (check-type condition noolean)
                                           (if (zerop condition)
                                               ,(lock-call c arg)
                                               ,(lock-call d arg))))

    ([7 b c]			$ 29	(lock-call c (lock-call b arg)))

    ([8 b c]			$ 30	(lock-call c `[,(lock-call b arg) ,arg]))

    ([9 b c]			$ 31	`(funcall (lambda (core)
                                                    (call (lockf-formula
                                                           ,(lock-call [0 b] 'core))
                                                          core))
                                                  ,(lock-call c arg)))

    ([10 [_ c] d]		$ 32	`(progn
                                           ,(lock-call c arg)
                                           ,(lock-call d arg)))

    ;; TODO jets
    ([10 _ c]			$ 33	(lock-fragment c arg))))
