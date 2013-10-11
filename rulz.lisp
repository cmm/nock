(in-package nock)

(defvar *tracedp* t)
(defvar *max-reductions* 32
  "Just a precaution, to avoid long boring traces before a failure.")
(defvar *tail-recursive-p* nil)
(defvar *evaluation-mode* :nock)

(defun set-evaluation-mode (mode &key (traced nil traced-specified-p)
                                      (tail-recursive nil tail-recursive-specified-p))
  "Determine evaluation mode."
  (unless (member mode '(:nock :lock))
    (error "Bad mode"))
  (setf *evaluation-mode* mode)
  (when traced-specified-p
    (setf *tracedp* traced))
  (when tail-recursive-specified-p
    (setf *tail-recursive-p* tail-recursive)))
