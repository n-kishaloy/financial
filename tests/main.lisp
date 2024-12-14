(defpackage financial/tests/main
  (:use :cl
        :financial
        :rove))
(in-package :financial/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :financial)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))
    (ok (= 2 2))))
