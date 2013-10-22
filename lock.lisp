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

(defun lock-formula (noun)
  "Compile NOUN to a formula."
  (let ((code (lock-match noun 'a)))
    (etypecase code
      (null     #'identity)
      (function code)
      (symbol   (fdefinition code))
      (cons     (compile*
                 `(locally
                      (declare ,*optimize-speed*)
                    (lambda (a)
                      ,code)))))))

(defun lock-call (noun arg)
  "Emit (possibly optimized) code for a formula call."
  (let ((code (lock-match (deworm noun) arg)))
    (etypecase code
      (null             arg)
      (function         `(funcall (the formula ,code) (the noun ,arg)))
      (symbol           `(,code (the noun ,arg)))
      ((or notom cons)  code))))

(defun lock-match (noun arg)
  "Emit code for NOUN, possibly applied to ARG."
  (ematch noun
    ([b c] when (consp b)       `(cons ,(lock-call b arg)
                                    ,(lock-call c arg)))

    ([0 b]                      (etypecase b
                                  ((eql 1) nil)
                                  (positive-fixnum
                                   (generate-tree-access b arg))))

    ([1 b]                      `(quote ,b))

    ([2 b c]                    `(funcall
                                  (the formula
                                       (lockf-formula
                                        ,(lock-call c arg)))
                                  (the noun ,(lock-call b arg))))

    ([3 b]                      `(noolify (consp ,(lock-call b arg))))
    ([4 b]                      `(let ((notom ,(lock-call b arg)))
                                   (check-type notom number)
                                   (1+ notom)))
    ([5 b]                      `(let ((noun ,(lock-call b arg)))
                                   (check-type noun cons)
                                   (noolify (eqn (carn noun) (cdr noun)))))
    ([6 b c d]                  `(let ((condition ,(lock-call b arg)))
                                   (check-type condition noolean)
                                   (if (zerop condition)
                                       ,(lock-call c arg)
                                       ,(lock-call d arg))))

    ([7 b c]                    (lock-call c (lock-call b arg)))

    ([8 b c]                    (lock-call c `[,(lock-call b arg) ,arg]))

    ([9 b c]                    `(funcall
                                  (lambda (core)
                                    (funcall (the formula
                                                  (lockf-formula
                                                   ,(lock-call [0 b] 'core)))
                                             (the noun core)))
                                  ,(lock-call c arg)))

    ([10 [_ c] d]               `(progn
                                   ,(lock-call c arg)
                                   ,(lock-call d arg)))

    ;; TODO jets
    ([10 _ c]                   (lock-match c arg))))

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
      (=        (noolify (equal (carn noun) (cdr noun))))
      (/        (funcall (tree-accessor (carn noun)) (cdr noun))))))
