;;;; mccal.lisp

(in-package #:mccal)

(define-presentation-type date ()
  :inherit-from t)

(define-application-frame calendar ()
  ((date :initarg :date :accessor date))
  (:panes
   (prev :push-button
         :label "Prev"
         :name :prev
         :activate-callback (lambda (&rest args)
                              (declare (ignore args))
                              (com-select-date
                               (local-time:adjust-timestamp (date *application-frame*) (offset :month -1)))
                              (redisplay-frame-panes *application-frame*)))
   (month :label-pane
          :align-x  :center
          :label
          (local-time:format-timestring nil (date *application-frame*) :format '(:long-month " " (:year 4))))

   (next :push-button
         :label "Next"
         :activate-callback (lambda (&rest args)
                              (declare (ignore args))
                              (com-select-date
                               (local-time:adjust-timestamp (date *application-frame*) (offset :month 1)))
                              (redisplay-frame-panes *application-frame*)))

   (calendar :application :display-function #'display-calendar)
   (input :interactor)
   (pointer-doc :pointer-documentation))
  (:menu-bar t)
  (:layouts (default
                (vertically ()
                  (horizontally ()prev month next)
                  calendar
                  input
                  pointer-doc))))

(defgeneric display (frame pane)
  )

(defmethod display-calendar ((frame calendar) pane)
  (let* ((date (date frame))
         (start-day (local-time:timestamp-minimize-part date :day :timezone local-time:+gmt-zone+))
         (end-day   (local-time:timestamp-maximize-part date :day))
         (iterator-day (calendar:adjust-weekday start-day 1)))

    (formatting-table (pane :x-spacing '(3 :character) :y-spacing '(1 :character))

      ;; Draw table header
      (formatting-row (pane)
        (surrounding-output-with-border
            (pane :shape          :rectangle
                  :background     +light-sea-green+
                  :line-thickness 1)
          (dolist (wd '(" - "  m t w t f s s))
            (formatting-cell (pane :align-x :right)
              (with-drawing-options (pane :ink +blue+)
                (format pane "~a" wd))))))

      ;; Draw month
      (dotimes (week (1+ (calendar:weeks end-day start-day)))
        (formatting-row (pane)

          ;; First cell is week number
          (formatting-cell (pane :align-x :right)
            (with-drawing-options (pane :ink +blue+)
              (format pane "~a" (calendar:week-num iterator-day))))

          ;; Draw one week per row
          (dotimes (i 7)
            (formatting-cell (pane :align-x :right)
              (with-output-as-presentation (pane (local-time:clone-timestamp iterator-day) 'date)
                (with-drawing-options (pane :ink (cond
                                                   ((calendar:same-day date iterator-day) +magenta+)
                                                   ((calendar:same-month date iterator-day) +black+)
                                                   (t +gray+)))
                  (draw-text* pane (format nil "~a" (local-time:timestamp-day iterator-day))  0 0))))
            (local-time:adjust-timestamp! iterator-day (offset :day 1))))))))


(defmethod run-calendar ()
  (run-frame-top-level (make-application-frame 'calendar :date (local-time:today))))

(define-presentation-to-command-translator com-click-date
    (date com-select-date calendar :echo nil)
    (object)
  (list object))

(define-calendar-command (com-select-date :name t :menu t) ((d date))
  (setf (date *application-frame*) d))

(define-calendar-command (com-select-year :name t :menu t) ((d number))
  (local-time:adjust-timestamp! (date *application-frame*) (:set :year d)))

(define-calendar-command (com-select-month :name t :menu t) ((d number))
  (local-time:adjust-timestamp! (date *application-frame*) (:set :month d)))

(define-calendar-command (com-redraw :name t :menu t) ()
  )
