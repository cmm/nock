(in-package nock)

(defvar *annotation* nil)
(defvar *sub-count*)
(defvar *inner-nerm-read-context-p* nil)

(defun nell-reader (stream char)
  "A NELL is a Nock cELL.  We represent them as CONSes, because Lisp."
  (declare (ignore char))
  (let* ((*inner-nerm-read-context-p* t)
         (list (read-delimited-list #\] stream t)))
    `(list* ,@list)))

(defun eval-reader (stream char)
  "Read a NERM.
Also eval it right away, usually."
  (declare (ignore char))
  (let* ((args (let ((*inner-nerm-read-context-p* t))
                 (read-delimited-list #\} stream t)))
         (make-form `(make-nerm :op ',(intern (string (first args)) 'nock)
                                :noun ,(second args)
                                :annotation *annotation*)))
    (if *inner-nerm-read-context-p*
        `(let ((*annotation* (cons *sub-count* *annotation*)))
           (incf *sub-count*)
           (nock ,make-form))
        `(if *tail-recursive-p*
             ,make-form
             (nock ,make-form)))))

(defun match-reader (stream char)
  "Read a NERM, or rather make a MATCH pattern that will match it."
  (declare (ignore char))
  (let ((args (read-delimited-list #\} stream t)))
    `(nerm :op ',(intern (string (first args)) 'nock) :noun ,(second args))))

(defun spel-reader (stream char)
  "Read and eval a NERM."
  (declare (ignore char))
  (let ((args (read-delimited-list #\} stream t)))
    (unless (= (length args) 2)
      (error "invalid NERM syntax"))
    `(nock (make-nerm :op ',(intern (string (first args)) 'nock)
                      :noun ,(second args)))))

(defun spec-reader (stream char)
  "Read and eval a NERM, spec style."
  (let* ((op (intern (make-string 1 :initial-element char) 'nock))
         (noun (let ((*inner-nerm-read-context-p* t))
                 (read stream t nil t))))
    `(nock (make-nerm :op ',op :noun ,noun))))

(defun dollar-reader (stream char)
  "Activate the eval readtable for the next one or two SEXPs.
The first SEXP is unevaluated, and is taken to be the *ANNOTATION* for
the second SEXP's dynamic extent."
  (declare (ignore char))
  (let ((*readtable* (find-readtable 'eval)))
    (let ((first (read stream t nil t)))
      `(macrolet ((annotation () '(,first)))
         (let ((*annotation* '(,first))
               (*sub-count* 0))
           ,(read stream t nil t))))))

(defreadtable base
  (:merge :standard)
  (:macro-char #\[ #'nell-reader)
  (:syntax-from :standard #\) #\])
  (:syntax-from :standard #\) #\}))

(defreadtable impl
  (:merge base)
  (:macro-char #\{ #'match-reader)
  (:macro-char #\$ #'dollar-reader))

(defreadtable eval
  (:merge base)
  (:macro-char #\{ #'eval-reader))

(defreadtable spel
  (:merge base)
  (:macro-char #\{ #'spel-reader))

(defreadtable spec
  (:merge spel)
  (:macro-char #\* #'spec-reader)
  (:macro-char #\? #'spec-reader)
  (:macro-char #\+ #'spec-reader)
  (:macro-char #\= #'spec-reader)
  (:macro-char #\/ #'spec-reader))
