(defsystem "financial"
  :version "0.0.1"
  :author "Kishaloy Neogi"
  :mailto "nkishaloy@yahoo.com"
  :license ""
  :depends-on ("alexandria"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "financial library in Common Lisp"
  :in-order-to ((test-op (test-op "financial/tests"))))

(defsystem "financial/tests"
  :author "Kishaloy Neogi"
  :license ""
  :depends-on ("financial"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for financial"
  :perform (test-op (op c) (symbol-call :rove :run c)))
