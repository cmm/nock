(cl:defpackage :nock
  (:use :cl)
  (:import-from :optima #:ematch)
  (:export #:*trace-nock-p*
           #:nock))
