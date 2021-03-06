(cl:defpackage :nock
  (:use :cl)
  (:import-from :optima
                #:ematch)
  (:import-from :optima.extra
                #:unless-match)
  (:import-from :named-readtables
                #:defreadtable
                #:find-readtable
                #:readtable-name
                #:in-readtable)
  (:import-from :alexandria
                #:when-let
                #:if-let
                #:named-lambda
                #:positive-fixnum)

  (:export #:set-evaluation-mode
           #:*max-reductions*
           #:spel
           #:spec

           #:define-jet
           #:define-jet-macro

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
