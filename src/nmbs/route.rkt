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
        #f
        (cadr route)))

    (define prev #f)

    (define/public (next)
      (begin0 (peek-next)
              (unless (null? route)
                (set-switches)
                (set! prev (car route))
                (set! route (cdr route)))))

    (define (set-switches)
      (let loop ((next (cdr route))
                 (visited (list (send (car route) get-segment)))
                 (distance 0))
        (unless (null? next)
          (let ((segment (send (car next) get-segment)))
            ; don't want to mess with all switches on our route
            ; but don't want to stop early either
            (unless (or (> distance 1500)
                        (memq segment visited))
              (when (and (switch? segment)
                         (not (eq? (send segment get-current-track) (car next))))
                (send segment set-current-track (car next)))
              (loop (cdr next)
                    (cons segment visited)
                    (+ distance (send segment get-length))))))))

    (define/public (reverse?)
      (and prev
           (pair? route)
           (peek-next)
           (same-segment? prev (peek-next))))

    (define/public (finished?)
      (eq? (car route) to))))
