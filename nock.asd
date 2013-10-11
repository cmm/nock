(asdf:defsystem :nock
  :description "Didactic Nock evaluator"
  :version "0.1"
  :author "Michael Livshin"
  :license "PD"
  :depends-on (:optima :named-readtables)
  :serial t
  :components ((:file "pack")
               (:file "rulz")
               (:file "read")
               (:file "nerm")
               (:file "nack")
               (:file "sock")
               (:file "iock")
               (:file "lock")
               (:file "nock")))
