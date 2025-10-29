(asdf:defsystem "financial"
  :version "0.0.1"
  :author "Kishaloy Neogi"
  :mailto "nkishaloy@yahoo.com"
  :license "MIT"
  :depends-on ("alexandria"
               "trivia"
               "serapeum"
               "iterate"
               "cl-csv"
               "local-time"
               "cl-containers"
               "coalton")
  :components ((:module "src"
                        :components
                        ((:file "main")
                         (:file "statements"))))
  :description "financial library in Common Lisp"
  :in-order-to ((test-op (test-op "financial/tests"))))

(asdf:defsystem "financial/tests"
  :author "Kishaloy Neogi"
  :license ""
  :depends-on ("financial"
               "rove")
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "Test system for financial"
  :perform (test-op (op c) (symbol-call :rove :run c)))


  
