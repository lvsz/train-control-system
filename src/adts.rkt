#lang racket/base

(require racket/class
         racket/list
         racket/function
         racket/set)

(provide node%
         track%
         detection-block%
         switch%
         loco%
         track?
         switch?
         detection-block?
         same-segment?)

(define (connected? track-1 track-2)
  (let ((node-1-1 (get-field node-1 track-1))
        (node-1-2 (get-field node-2 track-1))
        (node-2-1 (get-field node-1 track-2))
        (node-2-2 (get-field node-2 track-2)))
    (or (eq? node-1-1 node-2-1)
        (eq? node-1-1 node-2-2)
        (eq? node-1-2 node-2-1)
        (eq? node-1-2 node-2-2))))

(define (track? segment)
  (is-a? segment track%))

(define (switch? segment)
  (is-a? segment switch%))

(define (detection-block? segment)
  (is-a? segment detection-block%))

(define (same-segment? a b)
  (eq? (send a get-segment)
       (send b get-segment)))

(define (nub lst)
  (set->list (list->set lst)))

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
  (class* object% (writable<%>)
    (init-field id node-1 node-2 length)
    (field (master-switch #f) (segment this))
    (super-new)
    (send node-1 add-track this)
    (send node-2 add-track this)

    (define/public (get-id)
      id)

    (define/public (get-length)
      length)

    (define/public (get-segment)
      segment)

    (define/public (get-nodes)
      (values node-1 node-2))

    (define/public (get-current-track)
      this)

    (define/public (from track)
      (let ((segment (send track get-segment))
            (connected (get-connected-tracks)))
        (if (or (null? connected)
                (null? (cdr connected))
                (not (memq segment connected)))
          #f
          (car (remq segment connected)))))

    (define/public (from* track (include-rev #t))
      (let (; options going forwards
            (fwd (from track))
            ; options when reversing on this track
            (rev (if (and include-rev
                          (switch? (send track get-segment)))
                   (filter (lambda (p) (connected? this p))
                           (remq track (send (send track get-segment)
                                             get-positions)))
                   '())))
        (cond ((not fwd)
               rev)
              ((is-a? fwd switch%)
               (append (filter (lambda (p) (connected? this p))
                              (send fwd get-positions))
                      rev))
              (else
               (cons fwd rev)))))

    (define/public (get-connected-tracks)
      (filter identity
              (map (lambda (n)
                     (send n from this))
                   (list node-1 node-2))))

    (define/public (get-connected-tracks*)
      (flatten (map (lambda (t)
                      (if (is-a? t switch%)
                        (filter (lambda (p) (connected? this p))
                                (send t get-positions))
                        t))
                    (get-connected-tracks))))

    (define/public (get-master-switch)
      master-switch)

    (define/public (switch-override switch)
      (send node-1 remove-track (or master-switch this))
      (send node-1 add-track switch)
      (send node-2 remove-track (or master-switch this))
      (send node-2 add-track switch)
      (set! master-switch switch)
      (set! segment switch))

    (define/public (same-segment? track)
      (eq? segment (send track get-segment)))

    (define/public (custom-write port)
      (write (cons 'track% id) port))
    (define/public (custom-display port)
      (display (send segment get-id) port))))

(define detection-block%
  (class track%
    (init ((_id id))
          ((_node-1 node-1))
          ((_node-2 node-2))
          ((_length length)))
    (field (status 'green))
    (super-make-object _id _node-1 _node-2 _length)
    (inherit-field id node-1 node-2 length)

    (define connected-blocks
      (for/list ((track (in-list (list (send node-1 from this)
                                       (send node-2 from this))))
                 #:when (is-a? track detection-block%))
        (send track connect-block this)
        track))

    (define/public (connect-block block)
      (set! connected-blocks (cons block connected-blocks)))

    (define/public (get-status)
      status)

    (define/public (occupy)
      (set! status 'red)
      (for ((block (in-list connected-blocks))
            #:when (eq? (send block get-status) 'green))
        (send block set-status 'orange)))

    (define/public (clear)
      (if (for/or ((block (in-list connected-blocks)))
            (eq? (send block get-status) 'red))
        (set! status 'orange)
        (begin (set! status 'green)
               (for ((block (in-list connected-blocks))
                     #:unless (eq? (send block get-status) 'green))
                 (send block clear)))))

    (define/public (set-status new-status)
      (set! status new-status))

    (define/override (custom-write port)
      (write (cons 'detection-block% id) port))))


(define switch%
  (class track%
    (init ((_id id)))
    (init-field position-1 position-2)
    (inherit-field master-switch segment)
    (super-make-object _id
                       (get-field node-1 position-1)
                       (get-field node-2 position-1)
                       (get-field length position-1))
    (inherit-field id)

    (send position-1 switch-override this)
    (send position-2 switch-override this)

    (define/override (switch-override switch)
      (set! master-switch switch)
      (set! segment switch)
      (send position-1 switch-override switch)
      (send position-2 switch-override switch)
      (cond ((eq? (get-field position-1 switch) this)
             (set! adjust-master-switch
               (lambda () (send switch set-position 1))))
             ((eq? (get-field position-2 switch) this)
              (set! adjust-master-switch
                (lambda () (send switch set-position 2))))))

    (define position 1)

    (define/override (from track)
      (send (current) from track))

    (define/override (from* track (include-rev #t))
      (flatten (list (send position-1 from* track include-rev)
                     (send position-2 from* track include-rev))))

    (define/override (get-length)
      (send (current) get-length))

    (define/override (get-nodes)
      (send (current) get-nodes))

    (define/override (get-connected-tracks)
      (nub (append (send position-1 get-connected-tracks)
                   (send position-2 get-connected-tracks))))

    (define/public (get-positions (from #f))
      (flatten (list (if (switch? position-1)
                       (send position-1 get-positions)
                       position-1)
                     (if (switch? position-2)
                       (send position-2 get-positions)
                       position-2))))

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

    (define/override (get-current-track)
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
        (set-position 1)))

    (define/override (custom-write port)
      (write (cons 'switch% id) port))))


(define loco%
  (class object%
    (init-field id previous-track current-track)
    (field (speed 0)
           (direction 1))
    (super-new)
    ;(displayln (cons (send previous-segment get-id) (send starting-segment get-id)))
    (define/public (get-id)
      id)
    (define/public (get-current-track)
      current-track)
    (define/public (get-previous-track)
      previous-track)
    (define/public (update-location new-current-track (new-previous-track current-track))
      (set! current-track new-current-track)
      (set! previous-track new-previous-track))

    ;(define/public (set-location new-location)
      ;(when (is-a? location detection-block%)
        ;(send location set-status 'green))
      ;(when (is-a? new-location detection-block%)
        ;(send new-location set-status 'red))
      ;(set! location new-location))
    (define/public (get-speed)
      speed)
    (define/public (set-speed x)
      (set! speed x))

    (define/public (get-direction)
      direction)
    (define/public (change-direction)
      (set! direction (- direction)))))

