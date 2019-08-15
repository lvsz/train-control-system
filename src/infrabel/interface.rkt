#lang racket/base

(provide infrabel%)

(require racket/tcp
         racket/date
         racket/class)

(define port (call-with-input-file "resources/tcp-port.txt" read))

(unless (directory-exists? "logs") (make-directory "logs"))
(define log-file (open-output-file "logs/infrabel-interface.log" #:exists 'append))

(define max-attempts 10)
(define (try-connect n)
  (if (> n max-attempts)
    (error (format "tcp-connect failed after ~a attempts" n))
    (with-handlers ((exn:fail:network?
                      (lambda (exn)
                        (printf "tcp-connect attempt ~a failed~%" n)
                        (sleep 0.5)
                        (try-connect (add1 n)))))
                   (tcp-connect "localhost" port))))

(define-values (in out) (try-connect 0))

;; Struct for sending a request over tcp
;; msg contains the request
;; on-response is either #f or unary function
(struct request (msg on-response))

;; Several threads may be sending & requesting data over tcp,
;; so use this thread to avoid race conditions
(define communicator
  (thread (lambda ()
            (let loop ()
              ; blocks till thread receives a request
              (let* ((req (thread-receive))
                     (msg (request-msg req))
                     (on-response (request-on-response req)))
                (log (format "requesting ~a" msg))
                (writeln msg out)
                (flush-output out)
                (when on-response
                  (let ((response (read in)))
                    (log (format "response for ~a: ~a" msg response))
                    (on-response response)))
                (loop))))))

;; send a message over tcp
(define (put . args)
  (thread-send communicator
               (request args #f)))

;; request something over tcp
(define (get . args)
  (let ((response #f)
        (responded #f))
  (thread-send
    communicator
    (request args (lambda (x) (set! response x) (set! responded #t))))
  (let wait ()
    (if responded
      response
      (begin (sleep 0.05)
             (wait))))))

;; for interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infrabel%
  (class object%
    (super-new)

    (define/public (initialize setup-id)
      (put setup-id))

    (define/public (start)
      (put 'start))
    (define/public (stop)
      (put 'stop))

    (define/public (add-loco id prev-segment curr-segment)
      (put 'add-loco id prev-segment curr-segment))
    (define/public (remove-loco id)
      (put 'remove-loco id))
    (define/public (get-loco-speed id)
      (get 'get-loco-speed id))
    (define/public (set-loco-speed id speed)
      (put 'set-loco-speed id speed))
    (define/public (change-loco-direction id)
      (put 'change-loco-direction id))
    (define/public (get-loco-detection-block id)
      (get 'get-loco-detection-block id))


    (define/public (get-switch-position id)
      (get 'get-switch-position id))
    (define/public (set-switch-position id position)
      (put 'set-switch-position id position))
    (define/public (get-switch-ids)
      (get 'get-switch-ids))

    (define/public (get-detection-block-ids)
      (get 'get-detection-block-ids))
    (define/public (get-detection-block-statuses)
      (get 'get-detection-block-statuses))))

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
