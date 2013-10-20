(in-package nock)
(in-readtable impl)

(defconstant +inline-tree-accessors-num+ 256
  "How many tree accessor formulae to precompute.")
(defconstant +inline-tree-idx-max+
  (+ +inline-tree-accessors-num+ 2)
  "One above the largest tree index with a precomputed accessor.")
(defparameter +inline-tree-accessors+
  (make-array `(,+inline-tree-accessors-num+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tree-accessor-name (idx createp)
    (let* ((nock-package #.(find-package 'nock))
           (name (format nil "/~d" idx))
           (symbol (find-symbol name nock-package)))
      (or symbol
          (if createp
              (intern name nock-package)
              (error "no inline accessor for idx ~d" idx))))))

(defmacro define-inline-accessors ()
  (labels ((generate-tree-access (idx arg)
             (case idx
               (2		`(carn ,arg))
               (3		`(cdr ,arg))
               (otherwise	(multiple-value-bind (quotent remainder)
                                    (floor idx 2)
                                  (generate-tree-access
                                   (+ 2 remainder)
                                   (generate-tree-access quotent arg)))))))
    `(locally
         (declare ,*optimize-speed*)
       ,@(loop :for i :from 0 :below +inline-tree-accessors-num+
               :collecting
               (let* ((idx (+ i 2))
                      (name (tree-accessor-name idx t)))
                 `(progn
                    (declaim (inline ,name))
                    (defun ,name (.noun.)
                      (declare (type noun .noun.))
                      ,(generate-tree-access idx '.noun.))
                    (setf (elt +inline-tree-accessors+ ,i)
                          (function ,name))))))))
(define-inline-accessors)

(defun deep-tree-accessor (idx)
  "Make tree accessor formula for a large index."
  (locally
      (declare #.*optimize-speed*
               (type nondex idx))
    (labels ((tree-elt (idx tree)
               (if (< idx +inline-tree-idx-max+)
                   (let ((idx (the fixnum idx)))
                     (funcall (the formula (elt +inline-tree-accessors+ (- idx 2)))
                              tree))
                   (multiple-value-bind (quotent remainder)
                       (floor idx 2)
                     (tree-elt (+ 2 remainder)
                               (tree-elt quotent tree))))))
      (named-lambda deep-tree-accessor (tree)
        (tree-elt idx tree)))))

(defun tree-accessor (idx)
  "Proper tree accessor formula for IDX."
  (check-type idx nondex)
  (cond
    ((= idx 1)				#'identity)
    ((< idx +inline-tree-idx-max+)	(elt +inline-tree-accessors+ (- idx 2)))
    (t					(deep-tree-accessor idx))))

(defun tree-accessor-designator (idx)
  "Tree accessor designator for IDX."
  (check-type idx nondex)
  (cond
    ((= idx 1)				nil)
    ((< idx +inline-tree-idx-max+)	(tree-accessor-name idx nil))
    (t					(deep-tree-accessor idx))))

(defun compile* (form)
  "COMPILE any old form, not just a function name."
  (funcall (compile nil `(lambda () ,form))))

(defun lock-formula (noun)
  "Compile NOUN to a formula."
  (let ((code (lock-match noun 'a)))
    (etypecase code
      (null	#'identity)
      (function	code)
      (symbol	(fdefinition code))
      (cons	(compile*
                 `(locally
                      (declare ,*optimize-speed*)
                    (lambda (a)
                      ,code)))))))

(defun lock-call (noun arg)
  "Emit (possibly optimized) code for a formula call."
  (let ((code (lock-match (deworm noun) arg)))
    (etypecase code
      (null		arg)
      (function		`(funcall (the formula ,code) (the noun ,arg)))
      (symbol		`(,code (the noun ,arg)))
      ((or notom cons)	code))))

(defun lock-match (noun arg)
  "Emit code for NOUN, possibly applied to ARG."
  (ematch noun
    ([b c] when (consp b)	$ 19	`(cons ,(lock-call b arg)
                                               ,(lock-call c arg)))

    ([0 b]			$ 21	(progn
                                          (check-type b nondex)
                                          (tree-accessor-designator b)))

    ([1 b]			$ 22	`(quote ,b))

    ([2 b c]			$ 23	`(funcall (the formula
                                                       (lockf-formula ,(lock-call c arg)))
                                                  (the noun ,(lock-call b arg))))

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
                                                    (funcall (the formula
                                                                  (lockf-formula ,(lock-call [0 b] 'core)))
                                                             (the noun core)))
                                                  ,(lock-call c arg)))

    ([10 [_ c] d]		$ 32	`(progn
                                           ,(lock-call c arg)
                                           ,(lock-call d arg)))

    ;; TODO jets
    ([10 _ c]			$ 33	(lock-match c arg))))

(defun lockf-formula (noun)
  "Compile NOUN to a formula and cache it."
  (typecase (car noun)
    (wormula	(wormula-formula (car noun)))
    (t		(let ((formula (lock-formula noun)))
                  (prog1 formula
                    (setf (car noun)
                          (make-wormula :original (car noun)
                                        :formula formula)))))))

(defun lock (term)
  "The compiling Nock evaluator."
  (let ((noun (nerm-noun term)))
    (ecase (nerm-op term)
      (*	(funcall (lockf-formula (cdr noun)) (car noun)))

      (?	(noolify (consp noun)))
      (+	(1+ noun))
      (=	(noolify (equal (carn noun) (cdr noun))))
      (/	(funcall (tree-accessor (carn noun)) (cdr noun))))))
