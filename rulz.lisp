(in-package nock)
(in-readtable :standard)

(defvar *tracedp* t)
(defvar *max-reductions* 32
  "Just a precaution.
Precludes long boring traces before the inevitable failure.")
(defvar *tail-recursive-p* nil)
(defvar *evaluation-mode* :nock)

(defun set-evaluation-mode (mode &key (traced nil traced-p)
                                      (tail-recursive nil tail-recursive-p))
  "Determine evaluation mode."
  (unless (member mode '(:nock :lock))
    (error "Bad mode"))
  (setf *evaluation-mode* mode)
  (when traced-p
    (setf *tracedp* traced))
  (when tail-recursive-p
    (setf *tail-recursive-p* tail-recursive)))

(defvar *optimize-speed* '(optimize (debug 0) (safety 0) (speed 3)))
(defvar *annotation* nil)
(defvar *sub-count*)
