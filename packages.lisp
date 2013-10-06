(cl:defpackage :nock
  (:use :cl)
  (:import-from :optima
                #:ematch)
  (:import-from :named-readtables
                #:defreadtable
                #:find-readtable
                #:in-readtable)

  (:export #:set-evaluation-mode
           #:spec-readtable
           #:lisp-friendly-readtable))
