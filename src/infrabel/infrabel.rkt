#lang racket/base

(require racket/class
         "../railway.rkt"
         "../adts.rkt"
         "../setup.rkt"
         (prefix-in sim: "../gui_simulator/interface.rkt"))

(provide infrabel%)

(define infrabel%
  (class object%
    (super-new)

    (define railway #f)
    (define detection-blocks #f)
    (define locos (make-hash))

    (define/public (initialize setup-id)
      (case setup-id
        ((hardware)             (sim:setup-hardware))
        ((straight)             (sim:setup-straight))
        ((straight-with-switch) (sim:setup-straight-with-switch))
        ((loop)                 (sim:setup-loop))
        ((loop-and-switches)    (sim:setup-loop-and-switches))
        (else                   (error (format "Setup ~a not found" setup-id))))
      (set! railway (make-object railway% (get-setup setup-id)))
      (set! detection-blocks
            (for/hash ((db (in-list (send railway get-detection-blocks))))
              (values (send db get-id) db))))

    (define/public (start)
      (sim:start))
    (define/public (stop)
      (sim:stop))

    (define/public (add-loco id prev-segment curr-segment)
      (hash-set! locos id curr-segment)
      (send (hash-ref detection-blocks curr-segment) occupy)
      (sim:add-loco id prev-segment curr-segment))
    (define/public (remove-loco id)
      (sim:remove-loco id))
    (define/public (get-loco-speed id)
      (sim:get-loco-speed id))
    (define/public (set-loco-speed id speed)
      (sim:set-loco-speed! id speed))
    (define/public (change-loco-direction id)
      (sim:set-loco-speed! id (- (sim:get-loco-speed id))))
    (define/public (get-loco-detection-block id)
      (let ((sim-db (sim:get-loco-detection-block id))
            (loco-db (hash-ref locos id)))
        (cond
          ; nothing changed
          ((eq? sim-db loco-db)
           (void))
          ; loco is on a detection block but wasn't before
          ((and sim-db (not loco-db))
           (send (hash-ref detection-blocks sim-db) occupy)
           (hash-set! locos id sim-db))
          ; loco is on a new detection block
          ((and sim-db loco-db)
           (send (hash-ref detection-blocks loco-db) clear)
           (send (hash-ref detection-blocks sim-db) occupy)
           (hash-set! locos id sim-db))
          ; else loco left a detection block
          (else
           (send (hash-ref detection-blocks loco-db) clear)
           (hash-set! locos id #f)))
        sim-db))

    (thread (lambda ()
              (let loop ()
                ;(displayln (cons 'getting-locos loco-list))
                (for ((loco (in-list (hash-keys locos))))
                  (get-loco-detection-block loco))
                (sleep 1)
                (loop))))

    (define/public (get-switch-position id)
      (sim:get-switch-position id))
    (define/public (set-switch-position id position)
      (sim:set-switch-position! id position))

    (define/public (get-detection-block-ids)
      (sim:get-detection-block-ids))
    (define/public (get-detection-block-statuses)
      (for/list ((db (in-list (hash-values detection-blocks))))
        (cons (send db get-id) (send db get-status))))))

    ;(thread (lambda ()
    ;          (let loop ()
    ;            (for-each (lambda (loco)
    ;                        (let

