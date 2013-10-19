(cl:defpackage :nock
  (:use :cl)
  (:import-from :optima
                #:ematch
                #:place)
  (:import-from :named-readtables
                #:defreadtable
                #:find-readtable
                #:readtable-name
                #:in-readtable)
  (:import-from :alexandria
                #:when-let
                #:named-lambda)

  (:export #:set-evaluation-mode
           #:*max-reductions*
           #:spel
           #:spec

           ;; parenthesized primitives
           #:%hint
           #:%cellp
           #:%eq
           #:%inc
           #:%compose
           #:%cell-compose
           #:%core-apply
           #:%elt
           #:%S #:%K #:%I
           #:%if
           #:%dec))
