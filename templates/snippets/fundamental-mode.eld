fundamental-mode

(today (format-time-string "%Y-%m-%d")
       :ann "Today's date"
       :doc "Insert today's date")
(NOW (format-time-string "<%Y-%0m-%0d %a %H:%0M>")
     :ann "Current time"
     :doc "Insert current date and time")
