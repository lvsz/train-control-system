#lang racket/base

(require racket/class
         "adts.rkt")

(provide railway%)

(define railway%
  (class object%
    (init-field setup)
    (super-new)

    (define nodes (make-hash))
    (define tracks (make-hash))
    (define switches (make-hash))
    (define blocks (make-hash))
    (define locos (make-hash))

    (define/public (get-node id)
      (hash-ref nodes id))
    (define/public (get-nodes)
      (hash-values nodes))

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

    (define/public (get-loco id)
      (hash-ref locos id))
    (define/public (get-loco-ids)
      (hash-keys locos))
    (define (public-add-loco id prev-segment curr-segment)
      (let ((new-loco (make-object loco% id prev-segment curr-segment)))
        (hash-set! locos id new-loco)
        new-loco))
    (public (public-add-loco add-loco))

    (define (add-node id)
      (hash-set! nodes id (make-object node% id)))
    (define (get-or-add-node id)
      (if (hash-has-key? nodes id)
        (get-node id)
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
      (let ((block (make-object block% id n1 n2 length)))
        (hash-set! tracks id block)
        (hash-set! blocks id block)))

    (define (add-loco id track)
      (hash-set! locos id (make-object loco% id track)))

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
                    (list read-id read-track)
                    params)))))))

