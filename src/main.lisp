(defpackage #:financial
  (:use #:cl)
  (:local-nicknames (#:sp #:serapeum))
  (:export #:test-date-yr #:test-present-value
           #:is-leap-year #:days-in-year #:yearfrac #:x-yearfrac #:date-to-float
           #:date-from-float #:period #:make-period  #:discount-factor #:x-discount-factor
           #:midnight-timestamp #:new-year-timestamp #:pv #:xpv #:fv #:xfv #:pvm #:fvm
           #:pvc #:fvc #:pmt #:pv-annuity #:approx #:npv-t0 #:npv #:xnpv #:newt-raph))
(in-package #:financial)

;; this is x
(sp:-> is-leap-year (fixnum) boolean)
(defun is-leap-year (yr)
  "Returns if a year is leap year or not"
  (or (and (= 0 (mod yr 4))
           (not (= 0 (mod yr 100))))
      (= (mod yr 400) 0)))

(sp:-> days-in-year (fixnum) fixnum)
(defun days-in-year (yr) (if (is-leap-year yr) 366 365))

(sp:-> midnight-timestamp (fixnum fixnum fixnum) fixnum)
(defun midnight-timestamp (d m y) (encode-universal-time 0 0 0 d m y 0))

(sp:-> new-year-timestamp (fixnum) fixnum)
(defun new-year-timestamp (yr) (midnight-timestamp 1 1 yr))

(sp:-> date-to-float (fixnum) float)
(defun date-to-float (dt)
  "Converts a Timestamp to a f64 float with 1(One) year represented by 1.0"
  (multiple-value-bind (s0 mn0 h0 d1 m1 yr) (decode-universal-time dt 0)
    (declare (ignore s0 mn0 h0 d1 m1))
    (let* ((ts (new-year-timestamp yr))
           (sc (if (is-leap-year yr) 31622400.0 31536000.0)))
      (+ yr (/ (- dt ts) sc)))))

(sp:-> date-from-float (float) fixnum)
(defun date-from-float (yf0)
  "Converts a float to a Timestamp with 1(One) year represented by 1.0"
  (multiple-value-bind (yr fr) (floor yf0)
    (let ((ts (new-year-timestamp yr))
          (sc (if (is-leap-year yr) 31622400.0 31536000.0)))
      (+ ts (round (* fr sc))))))

(sp:-> yearfrac (fixnum fixnum &key (:tp keyword)) float)
(defun yearfrac (dt0 dt1 &key (tp :US30360))
"Day difference (dt1 - dt0) in fraction of a year
- dt0 = start date
- dt1 = end date

Following methods are supported
- US30360 => US 30/360 or NASD 30/360
- EU30360 => EURO 30/360
- ACTACT => (Days in Leap year) / 366 + (Days in Normal year) / 365
- ACT360 => Actual nos of days / 360
- ACT365 => Actual nos of days / 365

Note that the ACTACT formula is different from MS Excel and follows the Actual/Actual
ISDA rule. For more details refer <https://en.wikipedia.org/wiki/Day_count_convention>.

The yearfrac function is also signed with the result coming as negative in case dt0 > dt1. This is different from MS Excel, where the yearfrac number return absolute difference between the dates. Use abs() at end to replicate the same."
  (cond ((eq tp :ACTACT) (- (date-to-float dt1) (date-to-float dt0)))
        ((eq tp :ACT365) (/ (- dt1 dt0) 31536000.0))
        ((eq tp :ACT360) (/ (- dt1 dt0) 31104000.0))
        (t (multiple-value-bind (s1 mn1 h1 d1 m1 y1) (decode-universal-time dt1 0)
             (declare (ignore s1 mn1 h1))
             (multiple-value-bind (s0 mn0 h0 d0 m0 y0) (decode-universal-time dt0 0)
               (declare (ignore s0 mn0 h0))
               (flet ((day-count-factor (y0 m0 d0 y1 m1 d1)
                        (/ (+ (* 360 (- y1 y0)) (* 30 (- m1 m0)) (- d1 d0))
                           360.0)))
                 (cond ((eq tp :EU30360)
                        (flet ((last-day (d) (= d 31) 30 d))
                          (day-count-factor y0 m0 (last-day d0) y1 m1 (last-day d1))))
                       ((eq tp :US30360)
                        (flet ((last-feb (d m y)
                                 (and (= m 2) (if (is-leap-year y) (= d 29) (= d 28)))))
                          (progn
                            (if (last-feb d0 m0 y0)
                                (progn (if (last-feb d1 m1 y1) (setf d1 30))
                                       (setf d0 30)))
                            (if (and (= d1 31) (>= d0 30)) (setf d1 30))
                            (if (= d0 31) (setf d0 30))
                            (day-count-factor y0 m0 d0 y1 m1 d1))))
                       (t 0.0))))))))

(defstruct period (beg 0 :type fixnum) (fin 0 :type fixnum))

(sp:-> x-yearfrac (period &key (:tp keyword)) float)
(defun x-yearfrac (pr &key (tp :US30360))
  (yearfrac (period-beg pr) (period-fin pr) :tp tp)) 

(sp:-> discount-factor (float float) float)
(defun discount-factor (r n) (/ 1.0 (expt (+ 1.0 r) n)))

(sp:-> x-discount-factor (float period &key (:tp keyword)) float)
(defun x-discount-factor (r pr &key (tp :US30360))
  (discount-factor r (x-yearfrac pr :tp tp)))

(sp:-> pv (float float float) float)
(defun pv (r n f) (/ f (expt (+ 1.0 r) n)))

(sp:-> pvm (float float float float) float)
(defun pvm (r n m f) (pv (/ r m) (* n m) f))

(sp:-> fvm (float float float float) float)
(defun fvm (r n m f) (fv (/ r m) (* n m) f))

(sp:-> fv (float float float) float)
(defun fv (r n f) (* f (expt (+ 1.0 r) n)))

(sp:-> xpv (float period float &key (:tp keyword)) float)
(defun xpv (r p f &key (tp :US30360)) (pv r (x-yearfrac p :tp tp) f))

(sp:-> xfv (float period float &key (:tp keyword)) float)
(defun xfv (r p f &key (tp :US30360)) (fv r (x-yearfrac p :tp tp) f))

(sp:-> pvc (float float float) float)
(defun pvc (r n f) (/ f (exp (* r n))))

(sp:-> fvc (float float float) float)
(defun fvc (r n f) (* f (exp (* r n))))

(sp:-> pmt (float float float float float) float)
(defun pmt (r n m p f)
  (let* ((rm (/ r m))
         (rn (expt (+ 1.0 rm) (* n m))))
    (/ (* (+ p (/ f rn)) rm)
       (- (/ 1.0 rn) 1.0))))

(sp:-> pv-annuity (float float float float float) float)
(defun pv-annuity (r n m pm f)
  (let* ((rm (/ r m))
         (rn (expt (+ 1.0 rm) (* n m))))
    (- (* (/ pm rm) (- (/ 1.0 rn) 1.0))
       (/ f rn))))

(sp:-> approx (float float) boolean)
(defun approx (x y)
  (let ((mx (max (abs x) (abs y))))
    (or (< mx 1e-5) (< (/ (abs (- x y)) mx) 1e-4))))

(sp:-> npv-t0 (float list list) float)
(defun npv-t0 (r ti cf)
  (let ((r1 (+ 1.0 r)))
    (reduce #'+ (mapcar (lambda (tim csh) (/ csh (expt r1 tim))) ti cf))))

(sp:-> npv (float list float list) float)
(defun npv (r ti t0 cf) (* (npv-t0 r ti cf) (expt (+ 1.0 r) t0)))

(sp:-> xnpv (float list fixnum list) float)
(defun xnpv (r dt d0 cf) (npv-t0 r (mapcar (lambda (di) (yearfrac d0 di)) dt) cf))

(sp:-> newt-raph (function float float) cons)
(defun newt-raph (f x0 tol)
  (labels ((itrate (xp itr)
             (let* ((x (cdr xp))
                    (dx (/ tol 10.0))
                    (fx (funcall f x))
                    (df (/ (- (funcall f (+ x dx)) fx) dx))
                    (del-x (/ fx df))
                    (x1 (- x del-x)))
               (cond ((< (abs del-x) tol) `(:Some . ,x1))
                     ((= itr 0) `(:None . nil))
                     ((< (abs df) tol) `(:None . nil))
                     (t (itrate `(:Some . ,x1) (1- itr)))))))
    (itrate `(:Some . ,x0) 100)))

(sp:-> irr (list list) cons)
(defun irr (ti cf) (newt-raph (lambda (r) (npv-t0 r ti cf)) 0.1 1e-4))

(sp:-> xirr (list list) cons)
(defun xirr (dt cf)
  (let ((d0 (car dt))) (irr (mapcar (lambda (di) (yearfrac d0 di)) dt) cf)))

;; TESTS

(defvar *cdc* ())
(setf *cdc*
      (mapcar (lambda (x y)
                (list
                 (yearfrac x y :tp :US30360)
                 (yearfrac x y :tp :ACTACT)
                 (yearfrac x y :tp :ACT360)
                 (yearfrac x y :tp :ACT365)
                 (yearfrac x y :tp :EU30360)))
              
              (list (midnight-timestamp 5 2 2018)
                    (midnight-timestamp 29 2 2020)
                    (midnight-timestamp 30 8 2015)
                    (midnight-timestamp 28 2 2016)
                    (midnight-timestamp 31 1 2014)
                    (midnight-timestamp 28 2 2014)
                    (midnight-timestamp 29 2 2016))
              
              (list (midnight-timestamp 14 5 2023)
                    (midnight-timestamp 28 2 2024)
                    (midnight-timestamp 31 3 2010)
                    (midnight-timestamp 30 10 2016)
                    (midnight-timestamp 31 8 2014)
                    (midnight-timestamp 30 9 2014)
                    (midnight-timestamp 15 6 2016))))

; (format t "~a~%" *cdc*)

(defun test-date-yr ()
  (and (= 365 (days-in-year 2011))
       (= 366 (days-in-year 2016))
       (= 365 (days-in-year 1900))
       (= 366 (days-in-year 1600))
       (equal *cdc*
              (list (list 5.275 5.2684326 5.3444443 5.271233 5.275)
                    (list 3.9944444 3.9971924 4.0555553 4.0 3.9972222)
                    (list -5.4166665 -5.416382 -5.4944444 -5.419178 -5.413889)
                    (list 0.6722222 0.6694336 0.6805556 0.6712329 0.6722222)
                    (list 0.5833333 0.58081055 0.5888889 0.58082193 0.5833333)
                    (list 0.5833333 0.5863037 0.59444445 0.5863014 0.5888889)
                    (list 0.29166666 0.29223633 0.29722223 0.2931507 0.29444444)))
       (= 1.0 1.0)))

(defun test-present-value ()
  (and (= (pv 0.09 5.0 1e+7) 6499313.0)
       (= (xpv 0.08 (make-period :beg (midnight-timestamp 29 2 2020)
                                 :fin (midnight-timestamp 28 2 2024))
               5.638)
          4.1458697)
       (= (xfv 0.08 (make-period :beg (midnight-timestamp 29 2 2020)
                                 :fin (midnight-timestamp 28 2 2024))
               5.638)
          7.667159)
       (= (fvm 0.06 4.0 12.0 10000000.0) 1.2704889e7)
       (= (pvm 0.06 4.0 12.0 1.2704889e7) 10000000.0)
       (= (fvc 0.08 2.0 10000.0) 11735.109)
       (= (pvc 0.08 2.0 11735.109) 10000.0)
       (= (pmt 0.08 30.0 12.0 -1000.0 50.0) 7.304098)
       (= (pv-annuity 0.08 30.0 12.0 7.304098 50.0) -1000.0)
       (= (npv 0.08
               (list 0.25 6.25 3.5 4.5 1.25)
               -0.45
               (list -6.25 1.2 1.25 3.6 2.5))
          0.36962163)
       (= (xnpv 0.08
                (list (midnight-timestamp 25 2 2012)
                      (midnight-timestamp 28 6 2012)
                      (midnight-timestamp 15 2 2013)
                      (midnight-timestamp 18 9 2014)
                      (midnight-timestamp 20 2 2015))
                (midnight-timestamp 10 1 2012)
                (list -15.0 5.0 25.0 -10.0 50.0))
          44.165768)
       (approx (cdr (newt-raph (lambda (x) (expt (- x 4.0) 2.0)) 2.0 1e-5)) 4.0)
       (approx (cdr (newt-raph (lambda (x) (* (- x 3.0) (- x 4.0))) 2.0 1e-5)) 3.0)
       (eql :None (car (newt-raph (lambda (x) (+ (expt (- x 4.0) 2.0) 5.0)) 2.0 1e-5)))
       (approx (cdr (irr (list 0.125 0.29760274 0.49760274 0.55239726 0.812671233)
                     (list -10.25 -2.5 3.5 9.5 1.25)))
               0.31813413)
       (approx (cdr (xirr (list (midnight-timestamp 25 2 2012)
                            (midnight-timestamp 28 6 2012)
                            (midnight-timestamp 15 2 2013)
                            (midnight-timestamp 18 9 2014)
                            (midnight-timestamp 20 2 2015))
                      (list -115.0 5.0 25.0 -10.0 200.0)))
               0.27845532)
       (eql :None (car (irr (list 0.125 0.29760274 0.49760274 0.55239726 0.812671233)
                        (list 10.25 2.5 3.5 9.5 1.25))))))

