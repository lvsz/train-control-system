#lang racket/base

(provide route%)

(require racket/class
         "../railway/railway.rkt")

(define route%
  (class object%
    (init-field railway loco to)
    (super-new)
    (define from (send loco get-current-track))

    (define route (send railway get-route from to))

    (define/public (get-route)
      route)

    (define/public (set-route new-route)
      (set! route new-route)
      (set-switches)
      this)

    (define/public (go)
      (set-switches)
      this)

    (define/public (current)
      (car route))

    (define/public (peek-next)
      (if (null? (cdr route))
        '()
        (cadr route)))

    (define/public (next)
      (begin0 (peek-next)
              (unless (null? route)
                (set-switches)
                (set! route (cdr route)))))

    (define (set-switches)
      (let loop ((next (cdr route))
                 (visited (list (send (car route) get-segment))))
        (unless (null? next)
          (let ((segment (send (car next) get-segment)))
            (when (and (switch? segment)
                       (not (memq segment visited)))
              (send segment set-current-track (car next))
              (loop (cdr next) (cons segment visited)))))))

      (define/public (reverse?)
        (if (or (null? route) (null? (cdr route)) (null? (cddr route)))
          #f
          (let ((segment-1 (send (car route) get-segment))
                (segment-2 (send (caddr route) get-segment)))
            (eq? segment-1 segment-2))))))
