(defsystem "cl-maelstrom"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:access
               :alexandria
               :calispel
               :serapeum
               :yason)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "scheduler")
                 (:file "printer")
                 (:file "message")
                 (:file "node"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-maelstrom/tests"))))

(defsystem "cl-maelstrom/tests"
  :author ""
  :license ""
  :depends-on ("cl-maelstrom"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-maelstrom"
  :perform (test-op (op c) (symbol-call :rove :run c)))
