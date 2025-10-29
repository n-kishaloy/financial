(defpackage financial/tests/main
  (:use :cl :financial :rove)
  (:export #:print-loc))
(in-package :financial/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :financial)' in your Lisp.

(deftest test-date-yr (testing "" (ok (financial:test-date-yr))))
(deftest test-present-value (testing "" (ok (financial:test-present-value))))

