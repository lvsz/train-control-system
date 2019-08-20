#lang racket/base

(require racket/class
         racket/set
         racket/list
         "adts.rkt"
         "priority-queue.rkt"
         "setup.rkt")

(provide railway% track? switch? detection-block? same-segment?)

(define railway%
  (class object%
    (init-field setup-id)
    (super-new)

    (define setup (get-setup setup-id))

    (define nodes (make-hash))
    (define tracks (make-hash))
    (define switches (make-hash))
    (define blocks (make-hash))
    (define locos (make-hash))
    (define segments #f)

    (define/public (get-track id)
      (hash-ref tracks id))
    (define/public (get-tracks)
      (hash-values tracks))
    (define/public (get-track-ids)
      (hash-keys tracks))

    (define/public (get-switch id)
      (hash-ref switches id))
    (define/public (get-switches)
      (hash-values switches))
    (define/public (get-switch-ids)
      (hash-keys switches))

    (define/public (get-detection-block id)
      (hash-ref blocks id))
    (define/public (get-detection-blocks)
      (hash-values blocks))
    (define/public (get-detection-block-ids)
      (hash-keys blocks))

    (define/public (get-segments)
      (hash-values segments))
    (define/public (get-segment-ids)
      (hash-keys segments))

    (define/public (get-loco id)
      (hash-ref locos id))
    (define/public (get-loco-ids)
      (hash-keys locos))
    (define/public (get-locos)
      (hash-values locos))
    (define (public-add-loco id prev-segment-id curr-segment-id)
      (let* ((prev-segment (get-track prev-segment-id))
             (curr-segment (get-track curr-segment-id))
             (new-loco (make-object loco% id prev-segment curr-segment)))
        (hash-set! locos id new-loco)))
    (public (public-add-loco add-loco))

    (define/public (remove-loco id)
      (hash-remove! locos id))

    (define (add-node id)
      (hash-set! nodes id (make-object node% id)))
    (define (get-or-add-node id)
      (if (hash-has-key? nodes id)
        (hash-ref nodes id)
        (let ((node (make-object node% id)))
          (hash-set! nodes id node)
          node)))

    (define (add-track id n1 n2 length)
      (hash-set! tracks id (make-object track% id n1 n2 length)))

    (define (add-switch id current alternative)
      (let ((switch (make-object switch% id current alternative)))
        (hash-set! tracks id switch)
        (hash-set! switches id switch)))

    (define (add-block id n1 n2 length)
      (let ((block (make-object detection-block% id n1 n2 length)))
        (hash-set! tracks id block)
        (hash-set! blocks id block)))

    (define (add-loco id prev-track curr-track)
      (hash-set! locos id (make-object loco% id prev-track curr-track)))

    (define read-id string->symbol)
    (define read-number string->number)
    (define (read-node s)
      (get-or-add-node (string->symbol s)))
    (define (read-track s)
      (get-track (read-id s)))
    (define (add adder fns params)
      (apply adder (map (lambda (fn x) (fn x)) fns params)))
    (for ((input (in-list (send setup get-contents))))
      (let ((type (string->symbol (car input)))
            (params (cdr input)))
        (case type
          ((T) (add add-track
                    (list read-id read-node read-node read-number)
                    params))
          ((D) (add add-block
                    (list read-id read-node read-node read-number)
                    params))
          ((S) (add add-switch
                    (list read-id read-track read-track)
                    params))
          ((L) (add add-loco
                    (list read-id read-track read-track)
                    params)))))

    (define no-switches
      (filter (lambda (t) (not (is-a? t switch%))) (get-tracks)))
    (define routes&distances (make-hash))

    ;; this function's main purpose is to make sure that if a train has to
    ;: make a reverse maneuver after a switch on another switch, that the
    ;: second switch is to be set on its longer track for its maneuver
    ;; instead of of the default shortest track given by the pathfinder
    ;; this makes it less likely for the train to overshoot it trajectory
    ;; during the maneuver without affecting actual distance
    (define (improve-route route)
      (define improved-route '())
      (let loop ((route route))
        (if (< (length route) 3)
          (append (reverse improved-route) route)
          (let ((t1 (car route))
                (t2 (cadr route))
                (t3 (caddr route)))
            (if (and (same-segment? t1 t3)
                     (switch? (send t2 get-segment)))
              (let ((better-t2 (remq t2 (send (send t2 get-segment) get-positions))))
                (set! improved-route (cons t1 improved-route))
                (if (null? better-t2)
                  (set! improved-route (cons t2 improved-route))
                  (set! improved-route (cons (car better-t2) improved-route)))
                (loop (cddr route)))
              (begin (set! improved-route (cons t1 improved-route))
                     (loop (cdr route))))))))

    (set! segments
          (for/hash (((id track) (in-hash tracks))
                     #:when (eq? track (send track get-segment)))
            (values id track)))

    (define (unfold-route to route&distance)
      (define (unfold curr route)
        (let ((prev (car (hash-ref route&distance curr))))
          (if (null? prev)
            (improve-route (cons curr route))
            (unfold prev (cons curr route)))))
      (unfold to '()))

    (define/public (get-route&distance from to)
      (when (switch? from)
        (set! from (send from get-current-track)))
      (unless (hash-has-key? routes&distances from)
        (hash-set! routes&distances from (dijkstra from no-switches)))
      (let ((r&d (hash-ref routes&distances from)))
        (values (unfold-route to r&d) (cdr (hash-ref r&d to)))))

    (define/public (get-route from to)
      (let-values (((route dist) (get-route&distance from to)))
        route))

    (define/public (get-distance from to)
      (unless (hash-has-key? routes&distances from)
        (hash-set! routes&distances from (dijkstra from no-switches)))
      (cdr (hash-ref (hash-ref routes&distances from) to)))

    (define/public (get-alt-route&distance from to avoid)
      (when (switch? from)
        (set! from (send from get-current-track)))
      (let* ((avoid* (flatten (for/list ((t (in-list avoid)))
                                (let ((s (send t get-segment)))
                                  (if (switch? s)
                                    (send s get-positions)
                                    s)))))
             (alt (dijkstra from no-switches avoid*)))
        (if (null? (car (hash-ref alt to)))
          (values #f +inf.0)
          (values (unfold-route to alt) (cdr (hash-ref alt to))))))

    (define/public (get-alt-route from to avoid)
      (let-values (((route dist) (get-alt-route&distance from to avoid)))
        route))))

(define (dijkstra start tracks (avoid '()))
  (define distances (make-hash (map (lambda (t) (cons t +inf.0)) tracks)))
  (define how-to-reach (make-hash (map (lambda (t) (cons t '())) tracks)))
  (define pq-ids (make-hash (map (lambda (t) (cons t '())) tracks)))
  (define (notify pq-id track distance)
    (hash-set! pq-ids track pq-id))
  (define (pq-id-of track)
    (hash-ref pq-ids track))
  (define pq (priority-queue <))
  (define (relax! from to)
    (let ((weight (send to get-length)))
      (when (< (+ (hash-ref distances from) weight)
               (hash-ref distances to))
        (hash-set! distances to (+ (hash-ref distances from) weight))
        (hash-set! how-to-reach to from)
        (unless (queue-empty? pq)
          (reschedule! pq (pq-id-of to) (hash-ref distances to) notify)))))
  (hash-set! distances start 0)
  (for-each (lambda (track)
              (enqueue! pq track +inf.0 notify))
            tracks)
  (reschedule! pq (pq-id-of start) 0 notify)
  (let-values (((from distance) (serve! pq notify)))
    (for ((to (in-list (send start get-connected-tracks)))
          #:unless (memq to avoid))
      (relax! from to)))
  (let loop ()
    (let*-values (((from distance) (serve! pq notify))
                  ((from-from) (hash-ref how-to-reach from)))
      (unless (null? from-from)
        (for ((to (in-list (send from from* from-from)))
              #:unless (memq to avoid))
          (relax! from to))))
    (unless (queue-empty? pq)
      (loop)))
  (for/hash (((curr prev) (in-hash how-to-reach)))
    (values curr (cons prev (hash-ref distances curr)))))

