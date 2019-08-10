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
    ; list of tracks connected to this node
    (define tracks '())

    (define/public (get-id)
      id)

    (define/public (get-tracks)
      tracks)

    ; add a track (but prevent duplicates)
    (define/public (add-track track)
      (set! tracks (set-union (list track) tracks)))

    ; remove a track
    (define/public (remove-track track)
      (set! tracks (remq track tracks)))

    ; given a track, return the other track if it exists
    ; only used by track%
    (define/public (from track)
      (cond ((and (not (memq track tracks)))
             (let ((switch (get-field master-switch track)))
               (if switch
                 (from switch)
                 (error (format "node ~a does not connect to track ~a" id (send track get-id))))))
            ((and (pair? tracks) (pair? (cdr tracks)))
             (car (remq track tracks)))
            (else #f)))

    ))

(define track%
  (class object%
    (init-field id node-1 node-2 length)
    (field (master-switch #f))
    (super-new)
    (send node-1 add-track this)
    (send node-2 add-track this)

    (define/public (get-id)
      id)

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
                   (list node-1 node-2))))

    (define/public (connected? track)
      (let ((node-3 (get-field node-1 track))
            (node-4 (get-field node-2 track)))
        (or (eq? node-1 node-3)
            (eq? node-1 node-4)
            (eq? node-2 node-3)
            (eq? node-2 node-4))))


    (define/public (get-master-switch)
      master-switch)

    (define/public (switch-override switch)
      (send node-1 remove-track (or master-switch this))
      (send node-1 add-track switch)
      (send node-2 remove-track (or master-switch this))
      (send node-2 add-track switch)
      (set! master-switch switch))))

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
    (inherit-field master-switch)
    (super-make-object id
                       (get-field node-1 position-1)
                       (get-field node-2 position-1)
                       (get-field length position-1))

    (send position-1 switch-override this)
    (send position-2 switch-override this)

    (define/override (switch-override switch)
      (set! master-switch switch)
      (send position-1 switch-override switch)
      (send position-2 switch-override switch)
      (set! adjust-master-switch
           (if (eq? (get-field position-1 switch) this)
             (lambda () (send switch set-position 1))
             (lambda () (send switch set-position 2)))))

    (define position 1)

    (define/override (from node)
      (send (current) from node))

    (define/override (get-length)
      (send (current) get-length))

    (define/override (get-nodes)
      (send (current) get-nodes))

    (define/override (get-connected-tracks)
      (send (current) get-connected-tracks))

    (define/public (get-positions)
      (flatten (list (if (is-a? position-1 switch%)
                       (send position-1 get-positions)
                       position-1)
                     (if (is-a? position-2 switch%)
                       (send position-2 get-positions)
                       position-2))))

    (define/public (options-from node)
      (filter identity
              (flatten (map (lambda (track)
                              (if (is-a? track switch%)
                                (send track options-from node)
                                (send track from node)))
                            (list position-1 position-2)))))

    (define/public (options-from-track track)
      (filter (lambda (t) (send t connected? track)) (get-positions)))

    (define/public (has? track)
      (or (eq? position-1 track)
          (eq? position-2 track)
          (and (is-a? position-1 switch%) (send position-1 has? track))
          (and (is-a? position-2 switch%) (send position-2 has? track))))

    (define/public (get-position)
      position)

    (define adjust-master-switch void)

    (define callback void)
    (define/public (set-callback f)
      (set! callback f))

    (define/public (set-position int)
      (adjust-master-switch)
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

    (define/public (set-current-track track)
      (cond ((eq? position-1 track)
             (set-position 1))
            ((eq? position-2 track)
             (set-position 2))
            ((and (is-a? position-1 switch%)
                  (send position-1 has? track))
             ; will automatically change position of this track
             ; through internal-callback
             (send position-1 set-current-track track))
            ((and (is-a? position-2 switch%)
                  (send position-2 has? track))
             (send position-2 set-current-track track))
            (else
              (error "Switch doesn't contain track: ") (send track get-id))))

    (define/public (change-position)
      (if (= position 1)
        (set-position 2)
        (set-position 1)))))


(define loco%
  (class object%
    (init-field id previous-segment starting-segment (speed 0))
    (super-new)
    (define location starting-segment)
    ;(displayln (cons (send previous-segment get-id) (send starting-segment get-id)))
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

