(defpackage financial
  (:use #:cl)
  (:local-nicknames (#:sp #:serapeum))
  (:export #:hello))
(in-package #:financial)

(defun hello (name) (format t "Hallo there, ~A ~%" name))
;; blah blah blah.

(sp:-> is-leap-year (fixnum) bool)
(defun is-leap-year (yr) t)