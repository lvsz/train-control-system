#lang racket/base

(require racket/class
         racket/list
         racket/set
         "../railway.rkt"
         "../setup.rkt"
         "../adts.rkt"
         "../priority-queue.rkt")

(provide nmbs%
         starting-spot%)

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

    (define/public (get-loco-detection-block id)
      (send infrabel get-loco-detection-block id))
    (define/public (get-loco-speed id)
      (send (send railway get-loco id) get-speed))
    (define/public (set-loco-speed id speed)
      (send infrabel set-loco-speed id speed)
      (send (send railway get-loco id) set-speed speed))

    (define/public (add-loco id starting-spot)
      (let ((curr-id (get-field current-track starting-spot))
            (next-id (get-field next-track starting-spot)))
        (send infrabel add-loco id next-id curr-id)
        (send railway add-loco id (get-track next-id) (get-track curr-id))))


      ;(define start-track (send railway get-track start))
      ;(define end-track (send railway get-track end))
      ;(define-values ((start-node-1 start-node-2) (send start-track get-nodes)))
      ;(define-values ((end-node-1 end-node-2) (send end-track get-nodes)))
      ;(define nodes (send railway get-nodes))
      ;(define distances (make-hash (map (lambda (n) (cons n +inf.0)) nodes)))
      ;(define prevs (make-hash (map (lambda (n) (cons n #f)) nodes)))

      ;(define (for-each-track from fn)
      ;  (let ((tracks (send from get-tracks)))
      ;    ()))

      ;(hash-set! distances start-node-1 0)
      ;(hash-set! distances start-node-2 0)

      ;void)

    ;; get list of spots where a new loco can be added
    ;; to be a valid spot, 2 connected detection blocks are needed whose
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
    (init-field current-track next-track)
    (super-new)
    (define/public (get-id)
      current-track)))
      ;(string->symbol (format "~a → ~a" current-track next-track)))))

(define route%
  (class object%
    (init-field railway start)
    (super-new)
    (define tracks (filter (lambda (t) (not (is-a? t switch%)))
                           (send railway get-tracks)))
    ;(printf "evaluating tracks ~a~%" (map (lambda (x) (send x get-id)) tracks))
    (define distances (make-hash (map (lambda (t) (cons t +inf.0)) tracks)))
    (define how-to-reach (make-hash (map (lambda (t) (cons t '())) tracks)))
    (define pq-ids (make-hash (map (lambda (t) (cons t '())) tracks)))
    (define (track-ids pq-id track distance)
      (hash-set! pq-ids track pq-id))
    (define (pq-id-of track)
      (hash-ref pq-ids track))
    (define pq (priority-queue < ))
    (define (relax! from to)
      (let* ((tos (if (is-a? to switch%)
                    (send to options-from-track from)
                    (list to)))
             (weights (map (lambda (t) (send t get-length)) tos)))
        (for-each (lambda (a-to weight)
                    (when (< (+ (hash-ref distances from) weight)
                             (hash-ref distances a-to))
                      (hash-set! distances a-to (+ (hash-ref distances from) weight))
                      (hash-set! how-to-reach a-to from))
                    (unless (queue-empty? pq)
                      (reschedule! pq (pq-id-of a-to) (hash-ref distances a-to) track-ids)))
                  tos
                  weights)))
    (hash-set! distances start 0)
    (for-each (lambda (track)
                (enqueue! pq track +inf.0 track-ids))
              tracks)
    (reschedule! pq (pq-id-of start) 0 track-ids)
    (let loop ()
      ;(printf "distance from start: ~a~%" (hash-ref distances start))
      (let-values (((from distance) (serve! pq track-ids)))
        ;(printf "loop with ~a~%" (send from get-id))
        (for-each (lambda (to)
                    (relax! from to))
                  (send from get-connected-tracks)))
      (unless (queue-empty? pq)
        (loop)))
    (define route
      (cons (map (lambda (x) (cons (send (or (send (car x) get-master-switch) (car x)) get-id) (if (null? (cdr x)) '() (send (or (send (cdr x) get-master-switch) (cdr x)) get-id)))) (hash->list how-to-reach))
            (map (lambda (x) (cons (send (car x) get-id) (cdr x))) (hash->list distances))))
    (define/public (get-route)
      route)))

