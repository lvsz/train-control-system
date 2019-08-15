#lang racket/base

(require racket/tcp
         racket/date
         racket/class
         "infrabel.rkt")

(define infrabel (new infrabel%))

(define port (call-with-input-file "resources/tcp-port.txt" read))
(define listener #f)
(define in #f)
(define out #f)

(unless (directory-exists? "logs") (make-directory "logs"))
(define log-file (open-output-file "logs/infrabel-server.log" #:exists 'append))

(define (setup)
  (set! listener (tcp-listen port 4 #t))
  (let-values (((_in _out) (tcp-accept listener)))
    (set! in _in)
    (set! out _out)
    (let ((railway-setup-id (read in)))
      (send/apply infrabel initialize railway-setup-id))))

(define (stop (exn #f))
  (send infrabel stop)
  (when listener
    (tcp-close listener)
    (and in (tcp-abandon-port in))
    (and out (tcp-abandon-port out))
    (send infrabel stop))
  (cond ((not exn)
         (log "Infrabel server stopped by request"))
        ((exn:break? exn)
         (log "Infrabel server stopped by user break"))
        ((eof-object? exn)
         (log "Infrabel server stopped by client disconnection"))
        (else
         (log "Infrabel server stopped by unkown cause")))
  (exit))
#|
(define received (make-queue))
(define listener
  (thread (lambda ()
            (let loop ((msg (read in)))
              (set! received (cons msg received))
              (when (thread-try-receive)
                (set!
              (loop (read in))))))

(define (get-msg)
  (define msg #f)
  (thread-send listener (lambda (x) (set! msg x)))
  (let loop ()
    (cond (msg
            (log (format "received message: ~a" msg))
            msg
            (else
              (sleep 0.1)
              (loop))))))
|#

(define (get-msg)
  (let ((msg (read in)))
    (log (format "received message: ~a" msg))
    msg))

(define (reply response)
  (log (format "sent messgage: ~a~%" response))
  (writeln response out)
  (flush-output out))

(define (run)
  (log "Infrabel server activated")
  (let loop ((msg (get-msg)))
    (when (eof-object? msg)
      (stop))
    (let ((method (car msg))
          (args (cdr msg)))
      (case method
        ((add-loco)
         (send/apply infrabel add-loco args))
        ((remove-loco)
         (send/apply infrabel remove-loco args))
        ((get-loco-speed)
         (reply (send/apply infrabel get-loco-speed args)))
        ((set-loco-speed)
         (send/apply infrabel set-loco-speed args))
        ((change-loco-direction)
         (send/apply infrabel change-loco-direction args))
        ((get-loco-detection-block)
         (reply (send/apply infrabel get-loco-detection-block args)))
        ((get-switch-position)
         (reply (send/apply infrabel get-switch-position args)))
        ((set-switch-position)
         (send/apply infrabel set-switch-position args))
        ((get-switch-ids)
         (reply (send infrabel get-switch-ids)))
        ((get-detection-block-ids)
         (reply (send infrabel get-detection-block-ids)))
        ((get-detection-block-statuses)
         (reply (send infrabel get-detection-block-statuses)))
        ((start)
         (thread (lambda () (send infrabel start))))
        ((stop)
         (stop)))
      (loop (get-msg)))))

(define (start)
  (send infrabel start)
  (thread run))

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

(define (log msg)
  (display (time-string) log-file)
  (displayln msg log-file)
  (flush-output log-file))

(with-handlers ((exn:break? stop))
               (setup)
               (let loop ((msg (get-msg)))
                 (if (apply eq? 'start msg)
                   (void (start))
                   (begin (printf "Ignoring message: ~a~%" msg)
                          (loop (get-msg))))))
