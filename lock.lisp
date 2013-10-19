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

(defmacro mbda ((arg) (&rest sub-formulae) &body body &environment env)
  (let ((mbda-name (let ((annotation (macroexpand-1 '(annotation) env)))
                     (intern (format nil "RULE-~{~a~^:~}" (reverse annotation))
                             'nock))))
    (labels ((bda (body)
               `(locally
                    (declare (optimize (debug 0) (safety 0) (speed 3)))
                  (named-lambda ,mbda-name (,arg)
                    (declare (type noun ,arg))
                    ,@body)))
             (all-subsets (list)
               (when list
                 (let* ((tail-subsets (all-subsets (cdr list)))
                        (tail-subsets (or tail-subsets (list tail-subsets))))
                   (append tail-subsets
                           (mapcar (lambda (s)
                                     (cons (car list) s))
                                   tail-subsets)))))
             (uncall (ids body)
               ;; a very crude code walker -- if the body uses CALL
               ;; other than as an actual function call, bad things
               ;; will happen
               (labels ((rec (body)
                          (ematch body
                            (a when (atom a) a)
                            ((list 'call f rest) when (member f ids)
                             (rec rest))
                            ((cons car cdr)
                             (cons (rec car) (rec cdr))))))
                 (rec body)))
             (sans-id (names)
               `((and ,@(loop :for name :in names
                              :collect `(eqn ,name (%I))))
                 ,(bda (uncall names body)))))
  `(let (,@(loop :for name :in sub-formulae
                 :collect `(,name (lockf-formula ,name))))
     (cond
       ,@(loop :for names :in (sort (all-subsets sub-formulae)
                                    (lambda (a b) (> (length a) (length b))))
               :collect (sans-id names)))))))

(declaim (ftype (function (cons) formula) lockf-formula))
(defun lock-formula (noun)
  (declare (optimize (debug 3) (safety 3) (speed 1))
           (type cons noun))
  (ematch noun
    ([b c] when (consp b)	$ 19	(mbda (a) (b c)
                                          (cons (call b a) (call c a))))

    ([0 b]			$ 21	(progn
                                          (check-type b nondex)
                                          (tree-accessor b)))

    ([1 b]			$ 22	(constantly b))

    ([2 b c]			$ 23	(mbda (a) (b c)
                                          (call (lock-formula (call c a))
                                                (call b a))))

    ([3 b]			$ 24	(mbda (a) (b)
                                          (noolify (consp (call b a)))))
    ([4 b]			$ 25	(mbda (a) (b)
                                          (let ((notom (call b a)))
                                            (check-type notom number)
                                            (1+ notom))))
    ([5 b]			$ 26	(mbda (a) (b)
                                          (let ((noun (call b a)))
                                            (check-type noun cons)
                                            (noolify (eqn (carn noun) (cdr noun))))))
    ([6 b c d]			$ 28	(mbda (a) (b c d)
                                          (let ((cond (call b a)))
                                            (check-type cond noolean)
                                            (if (zerop cond)
                                                (call c a)
                                                (call d a)))))

    ([7 b c]			$ 29	(mbda (a) (b c)
                                          (call c (call b a))))

    ([8 b c]			$ 30	(mbda (a) (b c)
                                          (call c [(call b a) a])))

    ([9 b c]			$ 31	(let ((accessor (tree-accessor b)))
                                          (mbda (a) (c)
                                            (funcall (lambda (core)
                                                       (call (lockf-formula
                                                              (call accessor core))
                                                             core))
                                                     (call c a)))))

    ([10 [_ c] d]		$ 32	(mbda (a) (c d)
                                          (call c a)
                                          (call d a)))

    ;; TODO jets
    ([10 _ c]			$ 33	(lockf-formula c))))
