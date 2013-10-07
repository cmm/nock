(in-package nock)

(defvar *tracedp* t)
(defvar *tail-recursive-p* nil)
(defvar *max-reductions* 32
  "Just a precaution, to avoid long boring traces before a failure.")

(defun set-evaluation-mode (&key (traced nil traced-specified-p)
                                 (tail-recursive nil tail-recursive-specified-p))
  "Determine evaluation mode.
Current knobs are tracing and tail recursion, more may come later."
  (when traced-specified-p
    (setf *tracedp* traced))
  (when tail-recursive-specified-p
    (setf *tail-recursive-p* tail-recursive)))
