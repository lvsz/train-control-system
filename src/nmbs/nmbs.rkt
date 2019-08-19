#lang racket/base

(require racket/class
         racket/list
         racket/set
         "route.rkt"
         "loco-controller.rkt"
         "../railway/railway.rkt"
         "../setup.rkt")

(provide nmbs%)

;; METHODS:
; initialize
; get-loco-ids
; get-switch-ids
; get-detection-block-ids
; get-switch-position
; set-switch-position
; change-switch-posiiton
; add-loco
; get-loco-speed
; set-loco-speed
; get-starting-spots
(define nmbs%
  (class object%
    (init-field infrabel)
    (super-new)

    (define railway #f)

    ;; list of functions to be called when a switch changes
    (define switch-listeners '())
    (define/public (add-switch-listener fn)
      (set! switch-listeners (cons fn switch-listeners)))

    ;; list of functions to be called when a detection block changes
    (define detection-block-listeners '())
    (define/public (add-detection-block-listener fn)
      (set! detection-block-listeners
            (cons fn detection-block-listeners)))

    ;; hashmap of lists of functions called when a loco changes speed
    (define loco-speed-listeners (make-hash))
    (define/public (add-loco-speed-listener loco-id fn)
      (if (hash-has-key? loco-speed-listeners loco-id)
        (hash-update! loco-speed-listeners loco-id (lambda (fns) (cons fn fns)))
        (hash-set! loco-speed-listeners loco-id (list fn))))


    (define/public (get-loco-ids)
      (send railway get-loco-ids))
    (define/public (get-switch-ids)
      (send railway get-switch-ids))
    (define/public (get-detection-block-ids)
      (send railway get-detection-block-ids))

    (define/public (get-switch-position id)
      (send infrabel get-switch-position id))
    (define/public (set-switch-position id int)
      (send (send railway get-switch id) set-position int))
    (define/public (change-switch-position id)
      (send (send railway get-switch id) change-position))

    (define (get-loco id)
      (send railway get-loco id))
    (define/public (get-loco-speed id)
      (abs (send infrabel get-loco-speed id)))
    (define/public (set-loco-speed id speed)
      (for ((notify (in-list (hash-ref loco-speed-listeners id))))
        (notify speed))
      (send infrabel set-loco-speed id
            (* (send (get-loco id) get-direction) speed)))
    (define/public (change-loco-direction id)
      (send infrabel change-loco-direction id)
      (send (get-loco id) change-direction))

    ;; nmbs generates a list of viable starting spots
    ;; to add new locomotives
    (define/public (add-loco spot-id)
      (let* ((spot (hash-ref starting-spots spot-id))
             (curr-id (starting-spot-current spot))
             (prev-id (starting-spot-previous spot))
             (loco-id (gensym "L")))
        (send infrabel add-loco loco-id prev-id curr-id)
        (send railway add-loco loco-id prev-id curr-id)
        loco-id))

    (define/public (remove-loco loco-id)
      (send infrabel remove-loco loco-id)
      (send railway remove-loco loco-id)
      (thread-suspend update-thread)
      (hash-remove! loco-speed-listeners loco-id)
      (thread-resume update-thread))

    (define/public (get-loco-detection-block id)
      (send infrabel get-loco-detection-block id))

    ;; method that gets a route for a loco an creates a controller for it
    (define/public (route loco-id end-id)
      (define end (send railway get-track end-id))
      (define loco (send railway get-loco loco-id))
      (define loco-current-track (send loco get-current-track))
      (define loco-previous-track (send loco get-previous-track))
      ; get the route and check whether it's available
      ; otherwise get alternative route from infrabel or wait
      (define route
        (let* ((route (make-object route% railway loco end))
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
                              (send railway get-track id)))))))))
      (define loco-controller
        (new loco-controller%
             (nmbs this)
             (loco loco)
             (route route)
             (on-finish (lambda ()
                          (send infrabel finished-route loco-id)))))
      (send loco-controller start))

    ;; get hash of spots where a new loco can be added,
    ;; to be a valid spot, a detection block is needed whose local id and
    ;; that of a connected segment match those imported through infrabel
    (define starting-spots #f)
    (define/public (get-starting-spots)
      (if starting-spots
        (hash-keys starting-spots)
        '()))

    ;; update detection block statuses & loco speeds on regular intervals
    (define (get-updates)
      (sleep 1)
      (for ((db (send infrabel get-detection-block-statuses)))
        (for ((notify (in-list detection-block-listeners)))
          (notify (car db) (cdr db))))
      (for ((loco (in-list (get-loco-ids))))
        (for ((notify (in-list (hash-ref loco-speed-listeners loco))))
          (notify (get-loco-speed loco))))
      (get-updates))

    (define update-thread #f)

    (define/public (stop)
      (send infrabel stop)
      (kill-thread (current-thread)))

    (define/public (initialize setup)
      (set! railway (make-object railway% setup))
      (send infrabel initialize (send setup get-id))
      (send infrabel start)
      ;; add callback to switches to notify listeners & infrabel when changed
      (for ((switch (in-list (send railway get-switches))))
        (let ((id (send switch get-id)))
          (send switch
                set-callback
                (lambda ()
                  (let ((pos (send switch get-position)))
                    (for-each (lambda (fn)
                                (fn id pos))
                              switch-listeners)
                    (send infrabel set-switch-position id pos))))))
      (set! starting-spots (find-starting-spots infrabel railway))
      (sleep 0.5)
      (set! update-thread (thread get-updates)))))


;; simple struct that defines a spot where a locomotive can be added
;; like in the simulator, it needs a track for the train to start on
;; and a connected track to determine its direction
;; only detection blocks are allowed as starting tracks
(struct starting-spot (previous current))
(define (find-starting-spots infrabel railway)
  (let* ((db-ids (send infrabel get-detection-block-ids))
         (switch-ids (send infrabel get-switch-ids))
         (infrabel-ids (append db-ids switch-ids))
         (spots (for/list ((track-id (send railway get-detection-block-ids)))
                  ; first make sure there's a match
                  (and (memq track-id infrabel-ids)
                       (let* ((track (send railway get-track track-id))
                              ; get ids from segments connected to current track
                              (ids (map (lambda (s) (send s get-id))
                                        (send track get-connected-segments)))
                              ; check if any connected segment can be found in infrabel
                              (prev (findf (lambda (x)
                                             (memq x infrabel-ids))
                                           ids)))
                         ; create & return starting-spot object if result was a match
                         (and prev
                              (cons track-id (starting-spot prev track-id))))))))
    (for/hash ((spot (in-list spots))
               #:when spot)
      (values (car spot) (cdr spot)))))

