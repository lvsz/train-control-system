#lang racket/base

(require racket/class
         racket/list
         "../railway/railway.rkt"
         "../setup.rkt"
         (prefix-in z21: "../interface/interface.rkt")
         (prefix-in sim: "../gui_simulator/interface.rkt"))

(provide infrabel%)

(define infrabel%
  (class object%
    (super-new)

    (define railway #f)
    (define locos (make-hash))
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
      (set! railway (make-object railway% (get-setup setup-id)))
      (if (eq? setup-id 'real-hardware)
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
      (for ((loco (in-list (send railway get-loco-ids))))
        (hash-set! locos loco #f)
        (get-loco-detection-block loco)))
    (define/public (stop)
      (ext:stop-simulator))
      ;(sim:stop))

    (define loco-updates (make-hash))

    (define/public (add-loco id prev-segment-id curr-segment-id)
      (define prev-segment (get-track prev-segment-id))
      (define curr-segment (get-track curr-segment-id))
      (ext:add-loco id prev-segment-id curr-segment-id)
      (send railway add-loco id prev-segment-id curr-segment-id)
      (hash-set! locos id curr-segment-id)
      (hash-set! segment-reservations curr-segment-id id)
      (hash-set! loco-updates
                 (send railway get-loco id)
                 (update (current-milliseconds)
                         curr-segment
                         (send curr-segment from prev-segment)
                         (get-loco-speed id)
                         0
                         #f))
      (send curr-segment occupy))
    (define/public (remove-loco id)
      (ext:remove-loco id)
      (send railway remove-loco id)
      (hash-remove! locos id)
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

    (define/public (get-loco-detection-block id)
      (let ((sim-db (ext:get-loco-detection-block id))
            (loco-db (hash-ref locos id)))
        (cond
          ; nothing changed
          ((eq? sim-db loco-db)
           (void))
          ; loco is on a detection block but wasn't before
          ((and sim-db (not loco-db))
           (send (send railway get-detection-block sim-db) occupy)
           (hash-set! locos id sim-db)
           (hash-set! segment-reservations sim-db id))
           ;(send (get-loco id) update-location (get-track sim-db))
          ; loco is on a new detection block
          ((and sim-db loco-db)
           (send (send railway get-detection-block loco-db) clear)
           (send (send railway get-detection-block sim-db) occupy)
           ;(send (get-loco id) update-location (get-track sim-db))
           (hash-set! locos id sim-db)
           (hash-set! segment-reservations sim-db id))
          ; else loco left a detection block
          (else
           (send (send railway get-detection-block loco-db) clear)
           (hash-set! locos id #f)))
        sim-db))

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

    (define/public (finished-route loco-id)
      (let ((current-db (get-loco-detection-block loco-id)))
        (for (((segment-id loco?) (in-hash segment-reservations))
              #:when (and (eq? loco-id loco?)
                          (not (eq? segment-id current-db))))
          (hash-set! segment-reservations segment-id #f))))

    (struct update (time location next-segment speed travelled route) #:transparent)
    (define (loco-tracker)
      (define (update! loco time loc next speed travelled route)
        (hash-set! loco-updates loco
                   (update time loc next speed travelled route)))
      (define (next-from track from)
        (if from
          (send track from from)
          (let ((connected (send track get-connected-tracks)))
            (car connected))))
      (for (((loco last-update) (in-hash loco-updates)))
        (printf "loco on ~a?~%~%" (update-location last-update))
        (let* ((prev-time (update-time last-update))
               (curr-time (current-milliseconds))
               (prev-speed (update-speed last-update))
               (curr-speed (send loco get-speed))
               (prev-route (update-route last-update))
               (curr-route #f) ;(hash-ref routes loco-id))
               (db? (get-loco-detection-block (send loco get-id)))
               (db (and db? (get-track db?)))
               (prev-location (update-location last-update))
               (prev-segment (send prev-location get-segment))
               (prev-travelled (update-travelled last-update))
               (curr-travelled (* (abs curr-speed) (/ (- curr-time prev-time) 1000.0)))
               (prev-travelled (update-travelled last-update))
               (next-segment (update-next-segment last-update)))
          (cond ((zero? prev-speed)
                 (update! loco curr-time prev-location next-segment 1 prev-travelled curr-route))
                ((zero? curr-speed)
                 (update! loco curr-time prev-location next-segment prev-speed prev-travelled curr-route))
                ((and db (not (eq? db prev-location)))
                 (when (switch? prev-segment)
                   (set! prev-location (send prev-segment get-current-track)))
                 (send loco update-location db prev-location)
                 (let ((next (next-from db prev-location)))
                   (update! loco curr-time db next curr-speed 0 curr-route)))
                ((and db (eq? db prev-location))
                 (if (negative? (* curr-speed prev-speed))
                   (update! loco curr-time db (next-from db next-segment)
                            curr-speed
                            (+ curr-travelled (- (send db get-length) prev-travelled))
                            curr-route)
                   (update! loco curr-time db next-segment curr-speed
                            (+ curr-travelled prev-travelled)
                            curr-route)))
                ((negative? (* curr-speed prev-speed))
                 (update! loco
                          curr-time
                          prev-location
                          (next-from prev-location next-segment)
                          curr-speed
                          (+ curr-travelled (- (send prev-location get-length) prev-travelled))
                          curr-route))
                ((detection-block? prev-location)
                 ; just left detection block
                 (if (< (+ curr-travelled prev-travelled) (/ (send prev-location get-length) 2))
                   ; probably did not traverse detection-block
                   (let* ((curr-segment (next-from prev-location next-segment))
                          (curr-track (send curr-segment get-current-track)))
                     (send loco update-location curr-track prev-location)
                     (update! loco
                              curr-time
                              curr-track
                              (next-from curr-track prev-location)
                              curr-speed
                              0
                              curr-route))
                   (let ((curr-track (send next-segment get-current-track)))
                     (send loco update-location curr-track prev-location)
                     (update! loco
                              curr-time
                              curr-track
                              (next-from curr-track prev-location)
                              curr-speed
                              0
                              curr-route))))
                ((and (not (detection-block? next-segment))
                      (> (+ prev-travelled curr-travelled) (send prev-location get-length)))
                 (let* ((curr-track (send next-segment get-current-track))
                        (next-track (next-from curr-track prev-location)))
                   (send loco update-location next-track prev-location)
                   (update! loco curr-time curr-track next-track
                            curr-speed
                            (- (+ prev-travelled curr-travelled) (send prev-location get-length))
                            curr-route)))
                (else
                 (update! loco curr-time prev-location next-segment curr-speed
                          (+ prev-travelled curr-travelled)
                          curr-route)))))
      (sleep 0.1)
      (loco-tracker))

    ;(thread loco-tracker)


    (thread (lambda ()
              (let loop ()
                (for ((loco (in-list (hash-keys locos))))
                  (get-loco-detection-block loco))
                (sleep 0.5)
                (loop))))

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
        (cons (send db get-id) (send db get-status))))

    (define (lock-loco . args)
      (displayln (cons 'lock-loco args)))
    (define (alter-route . args)
      (displayln (cons 'alter args)))
    (define (ootw . args)
      (displayln (cons 'ootw args)))

    (define (resolve-routes loco-1 route-1 loco-2 route-2 conflict avoid)
      ;; IDEA
      ; get distance to next db not in loco-1's route
      ; get alternative route avoiding conflict
      ; choose shortest (min (* 2 (- dist-to-db (length db))) (abs (- og-route alt-route)))
      (define (get-overlap route-1 route-2)
        (let loop ((r1 route-1))
          (if (null? r1)
            (eprintf "No overlap found in ~a & ~a~%" route-1 route-2)
            (let ((found? (memf (lambda (x) (same-segment? (car r1) x)) route-2)))
              (if found?
                (for/list ((a (in-list found?))
                           (b (in-list r1))
                           #:break (not (same-segment? a b)))
                  a)
                (loop (cdr r1)))))))
      (define (get-conflict route-1 route-2)
        (get-overlap route-1 (reverse route-2)))
      (define (alt-route route conflict)
        (send railway get-alt-route&distance
              (car route)
              (last route)
              (append conflict avoid)))
      (define (nearest-db from avoid)
        (let ((best (cons #f +inf.0)))
          (for ((db (in-list (send railway get-detection-blocks)))
                #:when (and (not (memq db avoid))
                            (eq? (send db get-status 'green))))
            (let-values (((route dist)
                          (send railway get-alt-route&distance from db avoid)))
              (when (< dist (cdr best))
                (set! best (cons route dist)))))
          (values (car best) (cdr best))))
      (define (alt-route-diff route alt-dist)
        (- alt-dist (send railway get-distance (car route) (last route))))
      (define (ootw-diff alt-route alt-dist)
        (* 2.5 (- alt-dist (send (last alt-route) get-length))))
      (let ((conflict (get-conflict route-1 route-2)))
        (if (= (length conflict) 1)
          (let ((dist-1 (send railway get-distance (car route-1) (car conflict)))
                (dist-2 (send railway get-distance (car route-2) (car conflict))))
            (if (> dist-1 dist-2)
              (lock-loco loco-1)
              (lock-loco loco-2)))
          (let-values
            (((opt-1 dist-1) (alt-route route-1 conflict))
             ((opt-2 dist-2) (alt-route route-2 conflict))
             ((opt-3 dist-3) (nearest-db (car route-1) (remq (car route-1) route-2)))
             ((opt-4 dist-4) (nearest-db (car route-2) (remq (car route-2) route-2))))
            (let ((weighted-1 (alt-route-diff route-1 dist-1))
                  (weighted-2 (alt-route-diff route-2 dist-2))
                  (weighted-3 (ootw-diff opt-3 dist-3))
                  (weighted-4 (ootw-diff opt-4 dist-4)))
              (cond ((< (min weighted-1 weighted-2) (min weighted-3 weighted-4))
                     (if (< weighted-1 weighted-2)
                       (alter-route loco-1 opt-1)
                       (alter-route loco-2 opt-2)))
                    (else
                     (if (< weighted-3 weighted-4)
                       (ootw loco-1 opt-3 loco-2 conflict)
                       (ootw loco-2 opt-4 loco-1 conflict)))))))))))

    ;(thread (lambda ()
    ;          (let loop ()
    ;            (for-each (lambda (loco)
    ;                        (let

