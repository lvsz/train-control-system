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
    (define/public (get-detection-block-ids)
      (send railway get-detection-block-ids))

    (define/public (get-switch-position id)
      (cons (send infrabel get-switch-position id)
            (send (send railway get-switch id) get-position)))
    (define/public (set-switch-position id int)
      (send (send railway get-switch id) set-position int))
    (define/public (change-switch-position id)
      (send (send railway get-switch id) change-position))

    (define/public (get-track id)
      (send railway get-track id))

    (define (get-loco id)
      (send railway get-loco id))
    (define/public (get-loco-detection-block id)
      (send infrabel get-loco-detection-block id))
    (define/public (get-loco-speed id)
      (send (get-loco id) get-speed))
    (define/public (set-loco-speed id speed)
      (send infrabel set-loco-speed id (* (send (get-loco id) get-direction) speed))
      (send (get-loco id) set-speed speed))
    (define/public (change-loco-direction id)
      (send infrabel change-loco-direction id)
      (send (get-loco id) change-direction))

    (define/public (add-loco id starting-spot)
      (let ((curr-id (get-field current-track starting-spot))
            (next-id (get-field next-track starting-spot)))
        (send infrabel add-loco id next-id curr-id)
        (send railway add-loco id (get-track next-id) (get-track curr-id))))

    (define/public (route loco-id end-id)
      (define end (get-track end-id))
      (define loco (send railway get-loco loco-id))
      (define loco-current-track (send loco get-current-track))
      (define loco-previous-track (send loco get-previous-track))
      (define route (make-object route% railway loco end))
      (define direction (send loco get-direction))
      (when (eq? (get-field segment (send route peek-next))
                 (get-field segment loco-previous-track))
        (set! direction (- direction))
        (change-loco-direction loco-id))
      (define speed 100)
      (define interval 0.1)
      (define clearance (/ 100 (abs speed)))
      (set-loco-speed loco-id speed)
      (define (reverse-loco speed)
        (sleep clearance)
        (send loco update-location (send route next))
        (change-loco-direction loco-id)
        (route-iter (- (send (send route current) get-length)
                       (* clearance speed))
                    speed))
      (define (route-iter travelled speed)
        (sleep interval)
        (let ((curr (send loco get-current-track))
              ;(prev (send loco get-previous-track))
              (db? (get-loco-detection-block loco-id)))
          (if db?
            ; if loco is on db
            (let ((db (get-track db?)))
              (cond
                ((eq? db end)
                 (sleep (/ (send end get-length) 3 speed))
                 (set-loco-speed loco-id 0)
                 (send loco update-location db))
                ; loco on same db as before
                ((eq? db curr)
                 (route-iter (+ travelled (* interval speed)) speed))
                ; loco on differne db
                ((send route reverse?)
                 (reverse-loco speed))
                ((eq? db (send route peek-next))
                 ;(error "took a wrong turn"))
                 (send loco update-location db)
                 (route-iter 0 ;(* interval (abs speed))
                             speed))
                (else (route-iter (+ travelled (* interval speed)) speed))))
            (cond ((is-a? curr block%)
                   ; last iteration, loco was on a db, but no longer
                   (send loco update-location (send route next))
                   (route-iter 0 speed))
                  ((and (> travelled (send (send route current) get-length))
                        (not (is-a? (send route peek-next) block%)))
                   (if (send route reverse?)
                     (reverse-loco speed)
                     (begin (send loco update-location (send route next))
                            (route-iter 0 speed))))
                  (else
                   (route-iter (+ travelled (* interval speed))
                               speed))))))
      (define (start)
        (route-iter 0 speed))
      (thread start))

;
;                 ; loco is on db so time & travelled don't matter
;                (else
;                 (sleep interval)
;
;        (let ((curr )
;              (prev (send loco get-previous-track))
;              (db (let ((db? (send infrabel get-loco-detection-block loco-id)))
;                    (if db?
;                      (get-track db?)
;                      #f)))
;              (speed 100)
;              (pos-in-curr 0))
;          (when (eq? (cadr route) 
;
;          (when d
;            (set! db (get-track db))
;            (send loco update-location db curr))
;          (cond ((send curr same-segment? end)
;                 (set-loco-speed loco-id 0))
;                ((send 



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
    (init-field railway loco to)
    (super-new)
    (define start (send loco get-current-track))
    (define tracks (filter (lambda (t) (not (is-a? t switch%)))
                           (send railway get-tracks)))
    (define distances (make-hash (map (lambda (t) (cons t +inf.0)) tracks)))
    (define how-to-reach (make-hash (map (lambda (t) (cons t '())) tracks)))
    (define pq-ids (make-hash (map (lambda (t) (cons t '())) tracks)))
    (define (track-ids pq-id track distance)
      (hash-set! pq-ids track pq-id))
    (define (pq-id-of track)
      (hash-ref pq-ids track))
    (define pq (priority-queue < ))
    (define (relax! from to)
      (let ((weight (send to get-length)))
        (when (< (+ (hash-ref distances from) weight)
                 (hash-ref distances to))
          (hash-set! distances to (+ (hash-ref distances from) weight))
          (hash-set! how-to-reach to from)
          (unless (queue-empty? pq)
            (reschedule! pq (pq-id-of to) (hash-ref distances to) track-ids)))))
    (hash-set! distances start 0)
    (for-each (lambda (track)
                (enqueue! pq track +inf.0 track-ids))
              tracks)
    (reschedule! pq (pq-id-of start) 0 track-ids)
    (let-values (((from distance) (serve! pq track-ids)))
      (for-each (lambda (to) (relax! from to))
                (send start get-connected-tracks*)))
    (let loop ()
      (let-values (((from distance) (serve! pq track-ids)))
        (for-each (lambda (to) (relax! from to))
                  (send from from* (hash-ref how-to-reach from))))
      (unless (queue-empty? pq)
        (loop)))

    (define route
      (do ((rt (list to) (cons (hash-ref how-to-reach (car rt)) rt)))
        ((eq? (car rt) start) rt)))

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
                 (visited (list (get-field segment (car route)))))
        (unless (null? next)
          (let ((segment (get-field segment (car next))))
            (when (and (is-a? segment switch%)
                       (not (memq segment visited)))
              (send segment set-current-track (car next))
              (loop (cdr next) (cons segment visited)))))))

      (define/public (reverse?)
        (if (or (null? route) (null? (cdr route)) (null? (cddr route)))
          #f
          (let ((segment-1 (get-field segment (car route)))
                (segment-2 (get-field segment (caddr route))))
            (eq? segment-1 segment-2))))

        (set-switches)))

