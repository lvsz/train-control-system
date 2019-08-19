#lang racket/base

(require racket/class
         racket/list
         racket/set
         "../railway/railway.rkt"
         "../setup.rkt")

(provide nmbs%)

;; METHODS:
; initialize
; get-loco-ids
; get-switch-ids
; get-track-ids
; get-switch-position
; set-switch-position
; change-switch-posiiton
; add-loco
; get-loco-detection-block
; get-loco-speed
; set-loco-speed
; get-starting-spots
(define nmbs%
  (class object%
    (init-field infrabel)
    (super-new)

    (define railway #f)

    (define switch-listeners '())
    (define (get-switch-listeners)
      switch-listeners)
    (define/public (add-switch-listener fn)
      (set! switch-listeners (cons fn switch-listeners)))

    (define detection-block-listeners '())
    (define/public (add-detection-block-listener fn)
      (set! detection-block-listeners
            (cons fn detection-block-listeners)))

    (define/public (get-loco-ids)
      (send railway get-loco-ids))
    (define/public (get-switch-ids)
      (send railway get-switch-ids))
    (define/public (get-track-ids)
      (send railway get-track-ids))
    (define/public (get-detection-block-ids)
      (send railway get-detection-block-ids))

    (define/public (get-switch-position id)
      (send infrabel get-switch-position id))
      ;(send (send railway get-switch id) get-position))
    (define/public (set-switch-position id int)
      (send (send railway get-switch id) set-position int))
    (define/public (change-switch-position id)
      (send (send railway get-switch id) change-position))

    (define/public (get-track id)
      (send railway get-track id))

    (define loco-speed-listeners (make-hash))
    (define/public (add-loco-speed-listener loco-id fn)
      (if (hash-has-key? loco-speed-listeners loco-id)
        (hash-update! loco-speed-listeners loco-id (lambda (fns) (cons fn fns)))
        (hash-set! loco-speed-listeners loco-id (list fn))))

    (define (get-loco id)
      (send railway get-loco id))
    (define/public (get-loco-speed id)
      (abs (send infrabel get-loco-speed id)))
    (define/public (set-loco-speed id speed)
      (for ((notify (in-list (hash-ref loco-speed-listeners id))))
        (notify speed))
      (send infrabel set-loco-speed id (* (send (get-loco id) get-direction) speed)))
    (define/public (change-loco-direction id)
      (send infrabel change-loco-direction id)
      (send (get-loco id) change-direction))

    (define/public (add-loco spot-id)
      (let* ((spot (hash-ref starting-spots spot-id))
             (curr-id (starting-spot-current spot))
             (prev-id (starting-spot-previous spot))
             (id (gensym "L")))
        (send infrabel add-loco id prev-id curr-id)
        (send railway add-loco id (get-track prev-id) (get-track curr-id))
        id))

    (define/public (remove-loco loco-id)
      (send infrabel remove-loco loco-id)
      (send railway remove-loco loco-id))

    (define/public (route loco-id end-id)
      (define end (get-track end-id))
      (define loco (send railway get-loco loco-id))
      (define loco-current-track (send loco get-current-track))
      (define loco-previous-track (send loco get-previous-track))
      (define route (let* ((route (make-object route% railway loco end))
                           (route-ids (for/list ((r (in-list (send route get-route))))
                                        (send r get-id))))
                      (let loop ()
                        (let ((all-clear-or-alt (send infrabel reserve-route loco-id route-ids)))
                          (case all-clear-or-alt
                            ((#t) (send route go))
                            ((#f) (sleep 5)
                                  (loop))
                            (else (send route set-route
                                        (for/list ((id (in-list all-clear-or-alt)))
                                          (get-track id)))))))))
      (define direction (send loco get-direction))
      (when (eq? (send (send route peek-next) get-segment)
                 (send loco-previous-track get-segment))
        (set! direction (- direction))
        (change-loco-direction loco-id))
      (define speed 40)
      (define interval 0.01)
      (define clearance (/ 100 speed))
      (set-loco-speed loco-id speed)
      (define (loco-speed-changed new-speed)
        (set! speed new-speed)
        (unless (zero? speed)
          (set! clearance (/ 100 speed))))
      (add-loco-speed-listener loco-id loco-speed-changed)
      (define last-update (current-milliseconds))

      (define (reverse-loco)
        (sleep clearance)
        (send loco update-location (send route next))
        (change-loco-direction loco-id)
        (sleep clearance)
        (set! last-update (current-milliseconds))
        (route-iter (send (send route current) get-length)))

      (define (route-iter travelled)
        (sleep interval)
        (define delta (let ((old-update last-update))
                        (set! last-update (current-milliseconds))
                        (/ (- last-update old-update) 1000.0)))
        (let ((curr (send loco get-current-track))
              (db? (send infrabel get-loco-detection-block loco-id)))
          (if db?
            ; if loco is on db
            (let ((db (get-track db?)))
              (cond
                ((eq? db end)
                 (sleep (/ (send end get-length) 3 speed))
                 (set-loco-speed loco-id 0)
                 (send loco update-location db)
                 (send infrabel finished-route loco-id))
                ; loco on same db as before
                ((eq? db curr)
                 (route-iter (+ travelled (* delta speed))))
                ; loco on differne db
                ((send route reverse?)
                 (reverse-loco))
                ((eq? db (send route peek-next))
                 ;(error "took a wrong turn"))
                 (send loco update-location (send route next))
                 (route-iter 0))
                (else
                 (route-iter (+ travelled (* delta speed))))))
            (cond ((detection-block? curr)
                   ; last iteration, loco was on a db, but no longer
                   (send loco update-location (send route next))
                   (route-iter 0))
                  ((and (> travelled (send (send route current) get-length))
                        (not (detection-block? (send route peek-next))))
                   (if (send route reverse?)
                     (reverse-loco)
                     (begin (send loco update-location (send route next))
                            (route-iter 0))))
                  (else
                   (route-iter (+ travelled (* delta speed))))))))
      (define (start)
        (route-iter 0))
      (thread start))

    ;; get hash of spots where a new loco can be added,
    ;; to be a valid spot, a detection block is needed whose local id and
    ;; that of a connected segment match those imported through infrabel
    (define starting-spots #f)
    (define (find-starting-spots!)
      (let* ((db-ids (send infrabel get-detection-block-ids))
             (switch-ids (send infrabel get-switch-ids))
             (infrabel-ids (append db-ids switch-ids))
             (spots (for/list ((track-id (get-detection-block-ids)))
                      (and (memq track-id infrabel-ids)
                           (let* ((track (get-track track-id))
                                  ; get ids from tracks connected to current track
                                  (ids (map (lambda (t)
                                              (send t get-id))
                                            (send track get-connected-tracks)))
                                  ; check if any connected track can be found in infrabel
                                  (prev (findf (lambda (x)
                                                 (memq x infrabel-ids))
                                               ids)))
                             ; create & return starting-spot object if result was a match
                             (and prev
                                  (cons track-id
                                        (starting-spot prev track-id))))))))
        (set! starting-spots
              (for/hash ((spot (in-list spots))
                         #:when spot)
                (values (car spot) (cdr spot))))))

    (define/public (get-starting-spots)
      (hash-keys starting-spots))

    (define (get-updates)
      (for ((db (send infrabel get-detection-block-statuses)))
        (for ((notify (in-list detection-block-listeners)))
          (notify (car db) (cdr db))))
      (for ((loco (in-list (get-loco-ids))))
        (for ((notify (in-list (hash-ref loco-speed-listeners loco))))
          (notify (get-loco-speed loco))))
      (sleep 1)
      (get-updates))

    (define/public (initialize setup)
      (set! railway (make-object railway% setup))
      (send infrabel initialize (send setup get-id))
      (send infrabel start)
      (for ((switch (in-list (send railway get-switches))))
        (let ((id (send switch get-id)))
          ;(send switch set-position (send infrabel get-switch-position id))
          (send switch
                set-callback
                (lambda ()
                  (let ((pos (send switch get-position)))
                    (for-each (lambda (fn)
                                (fn id pos))
                              (get-switch-listeners))
                    (send infrabel set-switch-position id pos))))))
      (find-starting-spots!)
      (thread (lambda ()
                (sleep 0.5)
                (get-updates))))))


;; simple struct that defines a spot where a locomotive can be added
;; like in the simulator, it needs a track for the train to start on
;; and a connected track to determine its direction
(struct starting-spot (previous current))

(define route%
  (class object%
    (init-field railway loco to)
    (super-new)
    (define from (send loco get-current-track))

    (define route
      (send railway get-route from to))

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
      (set! route (cdr route))
      (set-switches)
      (if (null? route)
        '()
        (car route)))

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
