(in-package nock)

(defvar *annotation* nil)
(defvar *inner-nerm-read-context-p* nil)

(defun nell-reader (stream char)
  "A NELL is a Nock cELL.  We represent them as CONSes, because Lisp."
  (declare (ignore char))
  (let* ((*inner-nerm-read-context-p* t)
         (list (read-delimited-list #\] stream t)))
    `(list* ,@list)))

(defun nerm-eval-reader (stream char)
  "Read a NERM.
Also eval it right away, unless tracing."
  (declare (ignore char))
  (let* ((args (let ((*inner-nerm-read-context-p* t))
                 (read-delimited-list #\} stream t)))
         (make-form `(make-nerm :op ',(first args) :noun ,(second args)
                                :annotation *annotation*)))
    (if *inner-nerm-read-context-p*
        `(nock ,make-form)
        `(if *trace*
             (nock ,make-form)
             ,make-form))))

(defun nerm-match-reader (stream char)
  "Read a NERM, or rather make a MATCH pattern that will match it."
  (declare (ignore char))
  (let ((args (read-delimited-list #\} stream t)))
    `(nerm :op ',(first args) :noun ,(second args))))

(defun nerm-user-reader (stream char)
  "Read and eval a NERM."
  (declare (ignore char))
  (let ((args (read-delimited-list #\} stream t)))
    (unless (= (length args) 2)
      (error "invalid NERM syntax"))
    `(nock (make-nerm :op ',(first args) :noun ,(second args)))))

(defun prefix-reader (stream char)
  "Read and eval a NERM, spec style."
  (let* ((op (find-symbol (make-string 1 :initial-element char) (find-package :nock)))
         (noun (let ((*inner-nerm-read-context-p* t))
                 (read stream t nil t))))
    `(nock (make-nerm :op ',op :noun ,noun))))

(defun dollar-reader (stream char)
  "Activate the eval readtable for the next one or two SEXPs.
If the first SEXP is an atom (unevaluated), it is taken to be the
*ANNOTATION* for the second SEXP's dynamic extent. Else we only read
one SEXP."
  (declare (ignore char))
  (let ((*readtable* (find-readtable 'eval)))
    (let ((first (read stream t nil t)))
      (if (consp first)
          first
          `(let ((*annotation* ',first))
             ,(read stream t nil t))))))

(defreadtable base
  (:merge :standard)
  (:macro-char #\[ #'nell-reader)
  (:syntax-from :standard #\) #\])
  (:syntax-from :standard #\) #\}))

(defreadtable impl
  (:merge base)
  (:macro-char #\{ #'nerm-match-reader)
  (:macro-char #\$ #'dollar-reader))

(defreadtable eval
  (:merge base)
  (:macro-char #\{ #'nerm-eval-reader))

(defreadtable lisp-friendly-readtable
  (:merge base)
  (:macro-char #\{ #'nerm-user-reader))

(defreadtable spec-readtable
  (:merge lisp-friendly-readtable)
  (:macro-char #\* #'prefix-reader)
  (:macro-char #\? #'prefix-reader)
  (:macro-char #\+ #'prefix-reader)
  (:macro-char #\= #'prefix-reader)
  (:macro-char #\/ #'prefix-reader))
