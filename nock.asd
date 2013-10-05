(asdf:defsystem :nock
  :description "Didactic Nock evaluator"
  :version "0.1"
  :author "Michael Livshin"
  :license "PD"
  :depends-on (:optima :named-readtables)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "nerm")
               (:file "read-syntax")
               (:file "nock")))
