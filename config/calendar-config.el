(require 'calendar)
(require 'holidays)

(setq holiday-general-holidays nil
      holiday-other-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil)

(setq holiday-local-holidays
      '((holiday-fixed 1 1 "New Year")
        (holiday-fixed 3 29 "Good Friday")
        (holiday-fixed 4 1 "Easter Monday")
        (holiday-fixed 5 20 "Victoria Day")
        (holiday-fixed 6 24 "Saint-Jean-Baptiste Day (Quebec only)")
        (holiday-fixed 7 1 "Canada Day")
        (holiday-fixed 8 5 "Civic Holiday (excluding Quebec)")
        (holiday-fixed 9 2 "Labour Day")
        (holiday-fixed 9 30 "National Day for Truth and Reconciliation")
        (holiday-fixed 10 14 "Thanksgiving Day")
        (holiday-fixed 11 11 "Remembrance Day")
        (holiday-fixed 12 25 "Christmas Day")
        (holiday-fixed 12 26 "Boxing Day")
        ))

(setq calendar-holidays (append holiday-local-holidays holiday-other-holidays))

;; Function to display holidays in the calendar
(defun my-calendar-show-holidays ()
  "Show holidays in the calendar."
  (interactive)
  (calendar-mark-holidays))

(add-hook 'calendar-today-visible-hook 'my-calendar-show-holidays)
(add-hook 'calendar-today-invisible-hook 'my-calendar-show-holidays)
