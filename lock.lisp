(in-package nock)
(in-readtable base)

(defun generate-tree-access (idx arg)
  (case idx
    (2          `(carn ,arg))
    (3          `(cdr ,arg))
    (otherwise  (let ((sub (ash idx -1))
                      (there (+ (logand idx 1) 2)))
                  (generate-tree-access
                   there
                   (generate-tree-access sub arg))))))

(defun tree-accessor (idx)
  "Tree accessor for the given index."
  (declare #.*optimize-speed*
           (type nondex idx))
  (labels ((tree-elt (idx tree)
             (declare (type fixnum idx))
             (case idx
               (2         (carn tree))
               (3         (cdr tree))
               (otherwise (let ((sub (ash idx -1))
                                (there (+ (logand idx 1) 2)))
                            (tree-elt there
                                      (tree-elt sub tree)))))))
      (if (= idx 1)
          #'identity
          (named-lambda tree-accessor (tree)
            (tree-elt idx tree)))))

(defun compile* (form)
  "COMPILE any old form, not just a function name."
  (funcall (compile nil `(lambda () ,form))))

(defparameter *jets* (make-hash-table :test 'equal))

(defun lock-formula (noun)
  "Compile NOUN to a formula."
  (let ((code (lock-match noun)))
    (etypecase code
      (null  #'identity)
      (cons  (if-let (jet (gethash code *jets*))
               (ematch jet
                 (['function name]  (symbol-function name))
                 (['macro    name]  (get name 'callable)))
               (compile*
                `(locally
                     (declare ,*optimize-speed*)
                   (lambda (a)
                     ,code))))))))

(defun lock-call (noun)
  "Emit (possibly optimized) code for a formula call."
  (let ((code (lock-match noun)))
    (etypecase code
      (null  'a)
      (cons  (if-let (jet (gethash code *jets*))
               (ematch jet
                 ([_ name]  `(,name a)))
               code)))))

(defun lock-match (noun)
  "Emit code for NOUN."
  (ematch [(carn noun) (cdr noun)]
    ([b c] when (consp b)    `[,(lock-call b) ,(lock-call c)])

    ([0 b]                   (etypecase b
                               ((eql 1) nil)
                               (positive-fixnum
                                (generate-tree-access b 'a))))

    ([1 b]                   `(quote ,b))

    ([2 b c]                 `(funcall
                               (the formula
                                    (lockf-formula
                                     ,(lock-call c)))
                               (the noun ,(lock-call b))))

    ([3 b]                   `(noolify (consp ,(lock-call b))))
    ([4 b]                   `(let ((notom ,(lock-call b)))
                                (check-type notom notom)
                                (1+ notom)))
    ([5 b]                   `(let ((noun ,(lock-call b)))
                                (check-type noun cons)
                                (noolify (eqn (carn noun) (cdr noun)))))
    ([6 b c d]               `(let ((condition ,(lock-call b)))
                                (check-type condition noolean)
                                (if (zerop condition)
                                    ,(lock-call c)
                                    ,(lock-call d))))

    ([7 b c]                 `(let ((a ,(lock-call b)))
                                ,(lock-call c)))

    ([8 b c]                 `(let ((a [,(lock-call b) a]))
                                ,(lock-call c)))

    ([9 b c]                 `(funcall
                               (lambda (a)
                                 (funcall (the formula
                                               (lockf-formula
                                                ,(lock-call [0 (original b)])))
                                          (the noun a)))
                               ,(lock-call c)))

    ([10 [_ c] d]            `(progn
                                ,(lock-call c)
                                ,(lock-call d)))

    ;; We find jets by code recognition, so ignore any hints
    ([10 _ c]                (lock-match c))))

(defun lockf-formula (noun)
  "Compile NOUN to a formula and cache it."
  (typecase (car noun)
    (worm  (worm-formula (car noun)))
    (t     (let ((formula (lock-formula noun)))
             (prog1 formula
               (setf (car noun)
                     (make-worm :original (car noun)
                                :formula formula)))))))

(defun lock (term)
  "The compiling Nock evaluator."
  (let ((noun (nerm-noun term)))
    (ecase (nerm-op term)
      (*        (funcall (lockf-formula (cdr noun)) (car noun)))

      (?        (noolify (consp noun)))
      (+        (1+ noun))
      (=        (noolify (eqn (carn noun) (cdr noun))))
      (/        (funcall (tree-accessor (carn noun)) (cdr noun))))))

(defmacro define-jet (name noun (arg) &body body)
  "Define a jet NAME that does whatever NOUN does."
  `(let ((.code. (lock-match ,noun)))
     (locally
         (declare ,*optimize-speed*)
       (defun ,name (,arg)
         ,@body))
     (setf (gethash .code. *jets*) ['function ',name])))

(defmacro define-jet-macro (name noun (&rest args) &body body)
  "Define a jet macro NAME that does whatever NOUN does."
  (unless-match (list _) args
    (unless-match (list _ '&environment _) args
      (error "Malformed argument list")))
  `(progn
     (defmacro ,name (,@args)
       ,@body)
     (let ((.code. (lock-match ,noun)))
       (setf (get ',name 'callable)
             (locally
                 (declare ,*optimize-speed*)
               (flet ((,name (arg)
                        (,name arg)))
                 (function ,name))))
       (setf (gethash .code. *jets*) ['macro ',name]))))
