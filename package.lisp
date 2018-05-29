;;;; package.lisp

(defpackage #:mccal
  (:use #:clim-lisp #:clim)
  (:export
   #:run-calendar))

(defpackage #:calendar
  (:use #:cl)
  (:export
   #:week-num
   #:adjust-weekday
   #:same-day
   #:same-month
   #:weeks))
