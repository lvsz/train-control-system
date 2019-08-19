#lang racket/base

(provide loco-controller%)

(require racket/class
         "../railway/railway.rkt")

(define loco-controller%
  (class object%
    (init-field nmbs loco route on-finish)
    (super-new)

    (define loco-id (send loco get-id))
    ;; define direction, changing it if needed
    (define direction
      (let ((dir (send loco get-direction)))
          (displayln (cons (send route peek-next)
                           (send loco get-previous-track)))
        (if (same-segment? (send route peek-next)
                           (send loco get-previous-track))
          (begin (send nmbs change-loco-direction loco-id)
                 (- dir))
          dir)))
    (define speed 70)
    (define interval 0.1)
    ;; time needed when reversing after a switch
    (define clearance (/ 100 speed))
    ;; listener function to update parameters after a speed change
    (define (loco-speed-changed new-speed)
      (set! speed new-speed)
      (unless (zero? speed)
        (set! clearance (/ 100 speed))))
    (define last-update #f)

    ;; called when route requires a reversing maneuver
    (define (reverse-loco)
      (sleep clearance)
      (send loco update-location (send route next))
      (send nmbs change-loco-direction loco-id)
      (sleep clearance)
      (set! last-update (current-milliseconds))
      (route-iter (send (send route current) get-length)))

    (define (route-iter travelled)
      (sleep interval)
      ;; actual time passed since last updates
      (define delta (let ((old-update last-update))
                      (set! last-update (current-milliseconds))
                      (/ (- last-update old-update) 1000.0)))
      (define curr (send loco get-current-track))
      (define db (send nmbs get-loco-detection-block loco-id))
      (cond ((send route finished?)
             (sleep (/ (send (send loco get-current-track) get-length) 3 speed))
             (send nmbs set-loco-speed loco-id 0)
             (on-finish))
            ((send route reverse?)
             (reverse-loco))
            (db ; if loco is on db
              (cond
                ; loco on same db as before, little happens
                ((eq? db (send curr get-id))
                 (route-iter (+ travelled (* delta speed))))
                ; loco on different db
                ((eq? db (send (send route peek-next) get-id))
                 (send loco update-location (send route next))
                 (route-iter 0))
                (else
                  (route-iter (+ travelled (* delta speed))))))
            ; last iteration, loco was on a db, but not anymore
            ((detection-block? curr)
             (send loco update-location (send route next))
             (route-iter 0))
            ; if our estimates say that we should be on the next track,
            ; which is not a detection block
            ((and (> travelled (send (send route current) get-length))
                  (not (detection-block? (send route peek-next))))
             (send loco update-location (send route next))
             (route-iter 0))
            (else
              (route-iter (+ travelled (* delta speed))))))

    (define/public (start)
      (thread (lambda ()
                (send nmbs set-loco-speed loco-id speed)
                (send nmbs add-loco-speed-listener loco-id loco-speed-changed)
                (set! last-update (current-milliseconds))
                (route-iter 0))))))

