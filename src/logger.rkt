#lang racket/base

(provide make-logger)

(require racket/date)

(unless (directory-exists? "logs") (make-directory "logs"))

(define (time-string)
  (define (pad-zeroes n)
    (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))
  (let ((current-time (current-date)))
    (format "(~a:~a:~a) "
            (pad-zeroes (date-hour current-time))
            (pad-zeroes (date-minute current-time))
            (pad-zeroes (date-second current-time)))))


(define (make-logger log-file-name)
  (define log-file
    (open-output-file (string-append "logs/" log-file-name) #:exists 'append))
  (define (log msg)
    (display (time-string) log-file)
    (displayln msg log-file)
    (flush-output log-file))
  log)

