#lang racket/base

(require racket/class
         racket/list
         "../railway/railway.rkt"
         (prefix-in z21: "../interface/interface.rkt")
         (prefix-in sim: "../gui_simulator/interface.rkt"))

(provide infrabel%)

;; methods
; initialize
; start
; stop
; get-detection-block-ids
; get-detection-block-statuses
; get-switch-ids
; get-switch-position
; set-switch-position
; add-loco
; remove-loco
; get-loco-speed
; set-loco-speed
; change-loco-direction
; get-loco-detection-block
; reserve-route
; finish-route
(define infrabel%
  (class object%
    (super-new)

    (define railway #f)
    (define segment-reservations (make-hash))

    (define ext:start-simulator sim:start)
    (define ext:stop-simulator sim:stop)
    (define ext:get-loco-detection-block sim:get-loco-detection-block)
    (define ext:get-loco-speed sim:get-loco-speed)
    (define ext:set-loco-speed! sim:set-loco-speed!)
    (define ext:set-switch-position! sim:set-switch-position!)
    (define ext:get-switch-position sim:get-switch-position)
    (define ext:add-loco sim:add-loco)
    (define ext:remove-loco sim:remove-loco)
    (define ext:get-detection-block-ids sim:get-detection-block-ids)
    (define ext:get-switch-ids sim:get-switch-ids)

    (define (z21-mode)
      (set! ext:start-simulator z21:start-simulator)
      (set! ext:stop-simulator z21:stop-simulator)
      (set! ext:get-switch-position z21:get-switch-position)
      (set! ext:set-switch-position! z21:set-switch-position!)
      (set! ext:get-loco-speed z21:get-loco-speed)
      (set! ext:set-loco-speed! z21:set-loco-speed!)
      (set! ext:get-loco-detection-block z21:get-loco-detection-block)
      (set! ext:add-loco z21:add-loco)
      (set! ext:remove-loco void)
      (set! ext:get-switch-ids (send railway get-switch-ids))
      (set! ext:get-detection-block-ids (send railway get-detection-block-ids)))

    (define/public (initialize setup-id)
      (set! railway (make-object railway% setup-id))
      (if (eq? setup-id 'z21)
        (z21-mode)
        (case setup-id
          ((hardware)             (sim:setup-hardware))
          ((straight)             (sim:setup-straight))
          ((straight-with-switch) (sim:setup-straight-with-switch))
          ((loop)                 (sim:setup-loop))
          ((loop-and-switches)    (sim:setup-loop-and-switches))
          (else                   (error (format "Setup ~a not found" setup-id)))))
      (for ((segment-id (in-list (send railway get-segment-ids))))
        (hash-set! segment-reservations segment-id #f))
      (ext:start-simulator)
      'initialized)

    (define/public (start)
      (for ((switch (in-list (send railway get-switches))))
        (send switch set-position (ext:get-switch-position (send switch get-id))))
      (thread (lambda ()
                (let loop ()
                  (for ((loco (in-list (send railway get-loco-ids))))
                    (get-loco-detection-block loco))
                  (sleep 1)
                  (loop)))))

    (define/public (stop)
      (ext:stop-simulator))

    (define/public (add-loco id prev-segment-id curr-segment-id)
      (define prev-segment (get-track prev-segment-id))
      (define curr-segment (get-track curr-segment-id))
      (ext:add-loco id prev-segment-id curr-segment-id)
      (send railway add-loco id prev-segment-id curr-segment-id)
      (hash-set! segment-reservations curr-segment-id id)
      (send curr-segment occupy))

    (define/public (remove-loco id)
      (ext:remove-loco id)
      (send railway remove-loco id)
      (for (((segment loco) (in-hash segment-reservations))
            #:when (eq? id loco))
        (hash-set! segment-reservations segment #f)
        (when (detection-block? (get-track segment))
          (send (get-track segment) clear))))

    (define/public (get-loco-speed id)
      (ext:get-loco-speed id))

    (define/public (set-loco-speed id speed)
      (send (get-loco id) set-speed speed)
      (ext:set-loco-speed! id speed))

    (define/public (change-loco-direction id)
      (set-loco-speed id (- (ext:get-loco-speed id))))

    (define/public (get-loco-detection-block loco-id)
      (let* ((new-db-id (ext:get-loco-detection-block loco-id))
             (new-db (and new-db-id (get-track new-db-id)))
             (loco (send railway get-loco loco-id))
             (old-db (send loco detection-block)))
        (cond
          ; nothing changed
          ((eq? new-db old-db)
           (void))
          ; loco is on a detection block but wasn't before
          ((and new-db (not old-db))
           (send new-db occupy)
           (send loco update-location new-db)
           (hash-set! segment-reservations new-db-id loco-id))
          ; loco is on a new detection block
          ((and new-db old-db)
           (send old-db clear)
           (send new-db occupy)
           (send loco update-location new-db)
           (hash-set! segment-reservations new-db-id loco-id))
          ; else loco left a detection block
          (else
           (send old-db clear)
           (send loco left-detection-block)))
        new-db-id))

    (define (get-loco id)
      (send railway get-loco id))

    (define (get-track id)
      (send railway get-track id))

    (define/public (reserve-route loco-id route)
      (define segments
        (for/list ((track-id (in-list route)))
          (send (get-track track-id) get-segment-id)))
      (define non-clear
        (for/list ((segment-id (in-list segments))
                   #:when (let ((reserved (hash-ref segment-reservations segment-id)))
                            (and reserved
                                (not (eq? reserved loco-id)))))
          segment-id))
      (define (alt-route)
        (send railway get-alt-route
              (get-track (first route))
              (get-track (last route))
              (map get-track non-clear)))
      (if (empty? non-clear)
        (begin (for ((segment-id (in-list segments)))
                 (hash-set! segment-reservations segment-id loco-id))
               #t)
        (let ((alt (alt-route)))
          (if alt
            (map (lambda (x) (send x get-id)) alt)
            #f))))

    (define/public (end-route loco-id)
      (let ((current-db (get-loco-detection-block loco-id)))
        (for (((segment-id loco?) (in-hash segment-reservations))
              #:when (and (eq? loco-id loco?)
                          (not (eq? segment-id current-db))))
          (hash-set! segment-reservations segment-id #f))))

    (define/public (get-switch-position id)
      (ext:get-switch-position id))
    (define/public (set-switch-position id position)
      (send (get-track id) set-position position)
      (ext:set-switch-position! id position))
    (define/public (get-switch-ids)
      (ext:get-switch-ids))

    (define/public (get-detection-block-ids)
      (ext:get-detection-block-ids))

    (define/public (get-detection-block-statuses)
      (for/list ((db (in-list (send railway get-detection-blocks))))
        (cons (send db get-id) (send db get-status))))))
