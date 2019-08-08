#lang racket/base

(require racket/class
         racket/list
         racket/set
         "railway.rkt"
         "setup.rkt")

(provide nmbs%
         starting-spot%)

(define nmbs%
  (class object%
    (init-field infrabel)
    (super-new)

    (define railway #f)
    (define starting-spots '())

    (define/public (initialize setup)
      (set! railway (make-object railway% setup))
      (for-each (lambda (switch)
                  (send switch
                        set-callback
                        (lambda ()
                          (send infrabel
                                set-switch-position
                                (send switch get-id)
                                (send switch get-position)))))
                (send railway get-switches))
      (send infrabel initialize (send setup get-id))
      (send infrabel start)
      (set! starting-spots (find-starting-spots)))

    (define/public (get-loco-ids)
      (send railway get-loco-ids))
    (define/public (get-switch-ids)
      (send railway get-switch-ids))
    (define/public (get-track-ids)
      (send railway get-track-ids))

    (define/public (get-switch-position id)
      (cons (send infrabel get-switch-position id)
            (send (send railway get-switch id) get-position)))
    (define/public (set-switch-position id int)
      (send (send railway get-switch id) set-position int))
    (define/public (change-switch-position id)
      (send (send railway get-switch id) change-position))

    (define/public (get-track id)
      (send railway get-track id))

    (define/public (get-loco-speed id)
      (send (send railway get-loco id) get-speed))
    (define/public (set-loco-speed id speed)
      (send infrabel set-loco-speed id speed)
      (send (send railway get-loco id) set-speed speed))

    (define/public (add-loco id starting-spot)
      (let ((curr-id (get-field current-track starting-spot))
            (prev-id (get-field previous-track starting-spot)))
      (send infrabel add-loco id prev-id curr-id)
      (send railway add-loco id (get-track prev-id) (get-track curr-id))))

    ;; get list of spots where a new loco can be added
    ;; to be a valid spot, 2 connected tracks are needed whose
    ;; local ids match those imported through infrabel
    (define (find-starting-spots)
      (let ((infrabel-ids (send infrabel get-detection-block-ids)))
        (filter-map
          ; lambda creates a starting-spot from a valid track-id
          ; invalid ones return #f which gets filtered by filter-map
          (lambda (track-id)
            ; first check if current track id is also found in infrabel
            (and (memq track-id infrabel-ids)
                 (let* ((track (get-track track-id))
                        ; get ids from tracks connected to current track
                        (ids (map (lambda (t)
                                    (send t get-id))
                                  (send track get-connected-tracks)))
                        ; check if any connected track can be found in infrabel
                        (result (findf (lambda (x) (memq x infrabel-ids)) ids)))
                   ; create & return starting-spot object if result was a match
                   (and result
                        (make-object starting-spot% track-id result)))))
          (get-track-ids))))

    (define/public (get-starting-spots)
      starting-spots)))


;; simple class that defines a spot where a locomotive can be added
;; like in the simulator, it needs a track for the train to start on
;; and a connected track to determine its direction
(define starting-spot%
  (class object%
    (init-field current-track previous-track)
    (super-new)
    (define/public (get-id)
      current-track)))

