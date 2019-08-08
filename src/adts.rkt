#lang racket/base

(require racket/class
         racket/list
         racket/function
         racket/set)

(provide node% track% block% switch% loco%)


(define node%
  (class object%
    (init-field id)
    (super-new)
    (define/public (get-id) id)
    (define tracks '())
    (define/public (get-tracks)
      tracks)
    (define/public (add-track track)
      (set! tracks (set-union (list track) tracks)))
    (define/public (remove-track track)
      (set! tracks (remq track tracks)))
    (define/public (from track)
      (if (and (pair? tracks) (pair? (cdr tracks)))
        (car (remq track tracks))
        #f))))

(define track%
  (class object%
    (init-field id node-1 node-2 length)
    (super-new)
    (send node-1 add-track this)
    (send node-2 add-track this)
    (define/public (get-id) id)
    (define/public (get-length)
      length)
    (define/public (get-nodes)
      (values node-1 node-2))
    (define/public (from node)
      (cond ((eq? node node-1) node-2)
            ((eq? node node-2) node-1)
            (else #f)))
    (define/public (get-connected-tracks)
      (filter identity
              (map (lambda (n)
                     (send n from this))
                   (list node-1 node-2))))))

(define block%
  (class track%
    (init id node-1 node-2 length)
    (field (status 'green))
    (super-make-object id node-1 node-2 length)
    (define connected-blocks
      (for/list ((track (in-list (list (send node-1 from this)
                                       (send node-2 from this))))
                 #:when (is-a? track block%))
        (send track connect-block this)
        track))
    (define/public (connect-block block)
      (set! connected-blocks (cons block connected-blocks)))
    (define/public (get-status)
      status)
    (define/public (set-status new-status)
      (case new-status
        ((red) (set! status 'red)
               (for-each (lambda (block) (send block set-status 'orange))
                         connected-blocks))
        ((orange) (set! status 'orange))
        ((green) (when (eq? status 'red)
                   (for-each (lambda (block)
                               (when (eq? (send block get-status) 'orange)
                                 (send block set-status 'green)))
                             connected-blocks))
                 (set! status 'green))))))


(define switch%
  (class track%
    (init id)
    (init-field position-1 position-2)
    (super-make-object id
                       (get-field node-1 position-1)
                       (get-field node-2 position-1)
                       (get-field length position-1))
    ;TODO this can probably be done better node's in the remove-track method.
    (send (get-field node-1 position-1) remove-track position-1)
    (send (get-field node-2 position-1) remove-track position-1)
    (define n3 (get-field node-1 position-2))
    (define n4 (get-field node-2 position-2))
    (send n3 remove-track position-2)
    (send n4 remove-track position-2)
    (send n3 add-track this)
    (send n4 add-track this)

    (define/public (remove-from-nodes)
      (for-each (lambda (n)
                  (send n remove-track this))
                (list (get-field node-1 position-1)
                      (get-field node-2 position-1)
                      (get-field node-1 position-2)
                      (get-field node-2 position-2))))

    (when (is-a? position-1 switch%)
      (send position-1
            set-internal-callback
            (lambda () (send this set-position 1)))
      (send position-1 remove-from-nodes))
    (when (is-a? position-2 switch%)
      (send position-2
            set-internal-callback
            (lambda () (send this set-position 2)))
      (send position-2 remove-from-nodes))

    (define position 1)

    (define/override (from node)
      (send (current) from node))

    (define/override (get-length)
      (send (current) get-length))

    (define/override (get-nodes)
      (send (current) get-nodes))

    (define/override (get-connected-tracks)
      (send (current) get-connected-tracks))

    (define/public (options-from node)
      (filter identity
              (flatten (map (lambda (track)
                              (if (is-a? track switch%)
                                (send track options-from node)
                                (send track from node)))
                            (list position-1 position-2)))))

    ;(define/public (has? track)
    ;  (or (eq? position-1 track)
    ;      (eq? position-2 track)
    ;      (and (is-a? position-1 switch%) (send position-1 has? track))
    ;      (and (is-a? position-2 switch%) (send position-2 has? track))))

    (define/public (get-position)
      position)

    (define internal-callback void)
    (define/public (set-internal-callback f)
      (set! internal-callback f))
    (define callback void)
    (define/public (set-callback f)
      (set! callback f))
    (define/public (set-position int)
      (internal-callback)
      (set! position int)
      (callback))

    (define (current)
      (if (= position 1)
        position-1
        position-2))

    (define/public (get-current-track)
      (if (is-a? (current) switch%)
        (send (current) get-current-track)
        (current)))

    (define/public (change-position)
      (if (= position 1)
        (set-position 2)
        (set-position 1)))))


(define loco%
  (class object%
    (init-field id previous-segment starting-segment (speed 0))
    (super-new)
    (define location starting-segment)
    (define/public (get-id)
      id)
    (define/public (get-location)
      location)
    (define/public (set-location new-location)
      (when (is-a? location block%)
        (send location set-status 'green))
      (when (is-a? new-location block%)
        (send new-location set-status 'red))
      (set! location new-location))
    (define/public (get-speed)
      speed)
    (define/public (set-speed x)
      (set! speed x))))

