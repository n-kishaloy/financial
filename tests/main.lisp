(defpackage financial/tests/main
  (:use :cl :financial :rove))
(in-package :financial/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :financial)' in your Lisp.

(deftest dates-det
  (testing ""
    (ok (= 365 (financial:days-in-year 1999)))
    (ok (= 2 2))))
