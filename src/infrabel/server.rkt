#lang racket/base

(provide start-server)

(require racket/tcp
         racket/date
         racket/class
         "infrabel.rkt"
         "../logger.rkt")

(define infrabel (new infrabel%))

(define port (call-with-input-file "resources/tcp.txt" read))
(define listener #f)
(define in #f)
(define out #f)

(define (setup)
  (set! listener (tcp-listen port 4 #t))
  (let-values (((_in _out) (tcp-accept listener)))
    (set! in _in)
    (set! out _out))
  (let loop ()
    (let ((msg (get-msg)))
      (displayln msg)
      (case (car msg)
        ((initialize) (cadr msg))
        (else (loop))))))


(define (stop exn)
  (send infrabel stop)
  (when listener
    (tcp-close listener)
    (and in (tcp-abandon-port in))
    (and out (tcp-abandon-port out))
    (send infrabel stop))
  (cond ((eq? exn 'request)
         (log "Infrabel server stopped by request"))
        ((exn:break? exn)
         (log "Infrabel server stopped by user break"))
        ((eof-object? exn)
         (log "Infrabel server stopped by client disconnection"))
        (else
         (log (format "Infrabel server stopped by unkown cause: ~a" exn))))
  (exit))

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
      (stop msg))
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
        ((reserve-route)
         (reply (send/apply infrabel reserve-route args)))
        ((finished-route)
         (send/apply infrabel finished-route args))
        ((stop)
         (stop 'request))
        (else (displayln (cons 'unrecongized msg))))
      (loop (get-msg)))))


(define log (make-logger "infrabel-server.log"))

(define (start-server)
  (with-handlers ((exn:break? stop))
                 (send infrabel initialize (setup))
                 (begin0 (send infrabel start)
                         (sleep 0.1)
                         (thread run))))
