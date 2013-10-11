(asdf:defsystem :nock
  :description "Didactic Nock evaluator"
  :version "0.1"
  :author "Michael Livshin"
  :license "PD"
  :depends-on (:optima :named-readtables)
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "read-syntax")
               (:file "nerm")
               (:file "nack")
               (:file "symbolic-ops")
               (:file "interpreter")
               (:file "nock")))
