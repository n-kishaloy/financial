(defpackage financial
  (:use #:cl)
  (:local-nicknames (#:sp #:serapeum))
  (:export #:is-leap-year #:days-in-year))
(in-package #:financial)

(sp:-> is-leap-year (fixnum) boolean)
(defun is-leap-year (yr)
  (or (and (= 0 (mod yr 4))
           (not (= 0 (mod yr 100))))
      (= (mod yr 400) 0)))

(sp:-> days-in-year (fixnum) fixnum)
(defun days-in-year (yr)
  (if (is-leap-year yr) 366 365))