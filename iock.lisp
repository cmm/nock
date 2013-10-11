;;;; A naive rule-rewriting evaluator, good for learning.

(in-package nock)
(in-readtable impl)

(defvar *depth* 0)

(defun trace-boilerplate (term)
  ;; My FORMAT-fu sucks, sorry.
  (format *trace-output* "~&")
  (when (and (nermp term))
    (let ((ann (nerm-annotation term)))
      (when ann
        (format *trace-output* "~{~a~^:~}~6t" (reverse ann)))))
  (dotimes (i *depth*)
    (format *trace-output* " ")))

(defvar *reductions*)

(defun nock-in/traced (term)
  (trace-boilerplate term)
  (format *trace-output* "~a" term)
  (let ((result (let ((*depth* (1+ *depth*)))
                  (nock-in term))))
    (trace-boilerplate term)
    (format *trace-output* "<- ~a" (nellify result))
    result))

(defun nock-in (term)
  ;; You say "loop", I say "tail recursion"
  (loop :for current = term :then (progn
                                    (when *tracedp*
                                      (incf *reductions*)
                                      (when (> *reductions* *max-reductions*)
                                        (nope current "got to *MAX-REDUCTIONS*, giving up!")))
                                    (nock-nock current))
        :while (nermp current)
        :finally (return current)))

(defun nock-nock (term)
  "Perform one Nock reduction or fail."
  ;; This is supposed to be close in readability to the Nock spec.
  ;; The annotation numbers after the dollar signs are the spec rule
  ;; numbers.
  (ematch term
    (	{? a} when (consp a)		$  4			0					)
    (	{? _}				$  5			1					)
    (	{+ a} when (atom a)		$  7			(1+ a)					)
    (	{+ _}				$  6					(nack term)		)
    (	{= [a b]} when (equal a b)	$  8			0					)
    (	{= a} when (consp a)		$  9			1					)
    (	{= _}				$ 10					(nack term)		)

    (	{/ [1 a]}			$ 12			a					)
    (	{/ [2 a _]}			$ 13			a					)
    (	{/ [3 _ b]}			$ 14			b					)
    (	{/ [_ a]} unless (consp a)	$ foo!					(nack term)		)
    (	{/ [a _]} when (consp a)	$ foo!					(nack term)		)
    (	{/ [a b]} when (oddp a)		$ 16	{/ [3 {/ [(ash a -1) b]}]}				)
    (	{/ [a b]} when (> a 0)		$ 15	{/ [2 {/ [(ash a -1) b]}]}				)
    (	{/ _}				$ 17					(nack term)		)

    (	{* [a b c]} when (consp b)	$ 19	[{* [a b]} {* [a c]}]					)

    (	{* [a 0 b]}			$ 21	{/ [b a]}						)
    (	{* [_ 1 b]}			$ 22			b					)
    (	{* [a 2 b c]}			$ 23	{* [{* [a b]} {* [a c]}]}				)
    (	{* [a 3 b]}			$ 24	{? {* [a b]}}						)
    (	{* [a 4 b]}			$ 25	{+ {* [a b]}}						)
    (	{* [a 5 b]}			$ 26	{= {* [a b]}}						)

    (	{* [a 6 b c d]}			$ 28	{* [a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]}	)
    (	{* [a 7 b c]}			$ 29	{* [a 2 b 1 c]}						)
    (	{* [a 8 b c]}			$ 30	{* [a 7 [[7 [0 1] b] 0 1] c]}				)
    (	{* [a 9 b c]}			$ 31	{* [a 7 c 2 [0 1] 0 b]}					)
    (	{* [a 10 [_ c] d]}		$ 32	{* [a 8 c 7 [0 3] d]}					)
    (	{* [a 10 _ c]}			$ 33	{* [a c]}						)

    (	{* _}				$ 35					(nack term)		)
    ))
