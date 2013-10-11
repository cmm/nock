(cl:defpackage :nock
  (:use :cl)
  (:import-from :optima
                #:ematch)
  (:import-from :named-readtables
                #:defreadtable
                #:find-readtable
                #:readtable-name
                #:in-readtable)

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
