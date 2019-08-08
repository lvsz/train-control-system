#lang racket/base

(require racket/class
         "nmbs.rkt"
         "infrabel.rkt"
         "gui.rkt"
         "setup.rkt")

(provide main)

(define (main (setup-id #f))

  ;; if a railway setup is specified, use it, otherwise pick oone at random
  (define setup
    (if setup-id
      (get-setup setup-id)
      (list-ref setups (random (length setup)))))

  ;(define setup-name "hardware")

  ;; initialize infrabel with the chosen setup
  (define infrabel
    (new infrabel%))

  ;; start infrabel
  ;;(send infrabel start)

  ;; initialize nmbs
  (define nmbs
    (new nmbs%
         (infrabel infrabel)))

  ;; create window & run application
  (void (new setup-window%
             (nmbs nmbs)
             (setups setups)
             (atexit (lambda () (send infrabel stop))))))

(define (straight)
  (main 'straight))

(define (straight-with-switch)
  (main 'straight-with-switch))

(define (loop)
  (main 'loop))

(define (loop-and-switches)
  (main 'loop-and-switches))

(define (hardware)
  (main 'hardware))

(hardware)
