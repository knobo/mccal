
(in-package #:calendar)

(defparameter *week-starts-at* :sunday) ;; :monday Iso date starts at monday


(deftype week-day () `(member (:monday :tuesday :wednesday :thirsday :friday)))

(typep :monday 'week-day)

(defmethod adjust-weekday ((timestamp local-time:timestamp) (day number))
  "Set the day of week to day"
  (let ((current-day (local-time:timestamp-day-of-week timestamp)))
    (if (zerop current-day)
        (setf current-day 7))
    (local-time:timestamp+ timestamp
                           (- day current-day)
                           :day)))

(defmethod week-num ((timestamp local-time:timestamp))
  (multiple-value-bind (year week day) (local-time::%timestamp-decode-iso-week timestamp)
    (declare (ignore year day))
    week))

(defmethod same-day ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:with-decoded-timestamp (:day day-a :month month-a :year year-a) a
    (local-time:with-decoded-timestamp (:day day-b :month month-b :year year-b) b
      (and (equal year-a year-b)
           (equal month-a month-b)
           (equal day-a day-b)))))

(defmethod same-month ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:with-decoded-timestamp (:month month-a :year year-a) a
    (local-time:with-decoded-timestamp (:month month-b :year year-b) b
      (and (equal year-a year-b)
           (equal month-a month-b)))))

(defmethod weeks ((a local-time:timestamp) (b local-time:timestamp))
  (local-time-duration:duration-as
   (local-time-duration:timestamp-difference a b)
   :week))
