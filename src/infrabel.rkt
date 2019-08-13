#lang racket/base

(require racket/class
         (prefix-in sim: "gui_simulator/interface.rkt"))

(provide infrabel%)

(define infrabel%
  (class object%
    (super-new)

    (define/public (initialize setup-id)
      (case setup-id
        ((hardware)             (sim:setup-hardware))
        ((straight)             (sim:setup-straight))
        ((straight-with-switch) (sim:setup-straight-with-switch))
        ((loop)                 (sim:setup-loop))
        ((loop-and-switches)    (sim:setup-loop-and-switches))
        (else                   (error (format "Setup ~a not found" setup-id)))))

    (define/public (start)
      (sim:start))
    (define/public (stop)
      (sim:stop))

    (define locos '())

    (define/public (add-loco id prev-segment curr-segment)
      (set! locos (cons id locos))
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
      (sim:get-loco-detection-block id))

    (define/public (get-switch-position id)
      (sim:get-switch-position id))
    (define/public (set-switch-position id position)
      (sim:set-switch-position! id position))

    (define/public (get-detection-block-ids)
      (sim:get-detection-block-ids))))

    ;(thread (lambda ()
    ;          (let loop ()
    ;            (for-each (lambda (loco)
    ;                        (let

