#lang racket/base

(require racket/class
         "nmbs/nmbs.rkt"
         "infrabel.rkt"
         "gui.rkt"
         "setup.rkt")

(provide main)

(define (main (setup-id #f))
  (when (> (vector-length (current-command-line-arguments)) 0)
    (set! setup-id (string->symbol (vector-ref (current-command-line-arguments) 0))))
  ;; define infrabel
  (define infrabel
    (new infrabel%))

  ;; start infrabel
  ;;(send infrabel start)

  ;; define nmbs
  (define nmbs
    (new nmbs%
         (infrabel infrabel)))

  ;; create window & run application
  (void (cond (setup-id
                (send nmbs initialize (get-setup setup-id))
                (new window% (nmbs nmbs)
                     (atexit (lambda () (send infrabel stop)))))
              (else
                (new setup-window%
                     (nmbs nmbs)
                     (setups setups)
                     (atexit (lambda () (send infrabel stop))))))))

(main)
