(in-package nock)

(defvar *nock-eval-readtable*)
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
        `(if *trace-nock-p*
             (nock ,make-form)
             ,make-form))))

(defun nerm-match-reader (stream char)
  "Read a NERM, or rather make a MATCH pattern that will match it."
  (declare (ignore char))
  (let ((args (read-delimited-list #\} stream t)))
    `(nerm :op ',(first args) :noun ,(second args))))

;;; [] is the NELL syntax, like in the Nock spec
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[ #'nell-reader)

;;; In the regular readtable, {} is the matching NERM syntax
(set-macro-character #\} (get-macro-character #\)))
(set-macro-character #\{ #'nerm-match-reader)

(defun dollar-reader (stream char)
  "Activate *NOCK-EVAL-READTABLE* for the next one or two SEXPs.
If the first SEXP is an atom (unevaluated), it is taken to be the
current *ANNOTATION* for the second SEXP's dynamic extent. Else we
only read one SEXP."
  (declare (ignore char))
  (let ((*readtable* *nock-eval-readtable*))
    (let ((first (read stream t nil t)))
      (if (consp first)
          first
          `(let ((*annotation* ',first))
             ,(read stream t nil t))))))

(set-macro-character #\$ #'dollar-reader)

(setf *nock-eval-readtable* (copy-readtable))
(set-macro-character #\{ #'nerm-eval-reader
                     nil *nock-eval-readtable*)
