#lang racket/base

(provide priority-queue
         queue-empty?
         enqueue!
         serve!
         reschedule!
         queue-peek
         queue-peek-at
         priority-of)

(require racket/class
         data/gvector)

(define modifiable-heap%
  (class* object% (writable<%>)
    (init-field <<?)
    (field (storage (make-gvector)))
    (super-new)

    (define notify void)

    (define (get i)
      (gvector-ref storage (sub1 i)))

    (define (store! i a)
      (gvector-set! storage (sub1 i) a)
      (notify i a))

    (define (sift-up idx)
      (let sift-iter ((child idx) (element (get idx)))
        (let ((parent (quotient child 2)))
          (cond ((zero? parent)
                 (store! child element))
                ((<<? element (get parent))
                 (store! child (get parent))
                 (sift-iter parent element))
                (else (store! child element))))))

    (define (sift-down idx)
      (let ((size (gvector-count storage)))
        (let sift-iter ((parent idx) (element (get idx)))
          (let* ((childL (* 2 parent))
                 (childR (add1 childL))
                 (smallest (cond ((< childL size)
                                  (if (<<? (get childL) (get childR))
                                    (if (<<? element (get childL))
                                      parent
                                      childL)
                                    (if (<<? element (get childR))
                                      parent
                                      childR)))
                                 ((= childL size)
                                  (if (<<? element (get childL))
                                    parent
                                    childL))
                                 (else parent))))
            (if (= smallest parent)
              (store! parent element)
              (begin (store! parent (get smallest))
                     (sift-iter smallest element)))))))

    (define/public (empty?)
      (zero? (gvector-count storage)))

    (define/public (insert! item fn)
      (set! notify fn)
      (gvector-add! storage item)
      (let ((size (gvector-count storage)))
        (if (> size 1)
          (sift-up size)
          (notify 1 item))))

    (define/public (delete! fn)
      (when (empty?)
        (error "Heap empty" this))
      (set! notify fn)
      (let* ((first (get 1))
             (last (gvector-remove-last! storage)))
        (if (empty?)
          (notify 1 last)
          (begin (store! 1 last)
                 (sift-down 1)))
        first))

    (define/public (peek)
      (when (empty?)
        (error "Heap empty" this))
      (get 1))

    (define/public (peek-at idx)
      (get idx))

    (define/public (touch-at! idx fn)
      (set! notify fn)
      (let ((parent (quotient idx 2))
            (size (gvector-count storage)))
        (cond ((= idx 1)
               (sift-down idx))
              ((= idx size)
               (sift-up idx))
              ((<<? (get parent) (get idx))
               (sift-down idx))
              (else
               (sift-up idx)))))

    (define/public (custom-write port)
      (write "#modifiable-heap" port)
      (write (gvector->list storage) port))
    (define/public (custom-display port)
      (custom-write port))))

(struct item (value (priority #:mutable)) #:transparent)

(define (pair->item x)
  (item (car x) (cdr x)))

(define (on accessor cmp)
  (lambda (x y)
    (cmp (accessor x)
         (accessor y))))

(define (pq-notify notify)
  (lambda (idx item)
    (notify idx (item-value item) (item-priority item))))

(define (priority-queue <<?)
  (make-object modifiable-heap% (on item-priority <<?)))

(define (queue-empty? pq)
  (send pq empty?))

(define (enqueue! pq value priority notify)
  (send pq insert! (item value priority) (pq-notify notify)))

(define (serve! pq notify)
  (when (send pq empty?)
    (error "serve!: empty priority queue" pq))
  (let ((item (send pq delete! (pq-notify notify))))
    (values (item-value item) (item-priority item))))

(define (reschedule! pq idx new-priority notify)
  (let ((pq-item (send pq peek-at idx)))
    (set-item-priority! pq-item new-priority)
    (send pq touch-at! idx (pq-notify notify))))

(define (queue-peek pq)
  (when (send pq empty?)
    (error "queue-peek: empty priority queue" pq))
  (item-value (send pq peek)))

(define (queue-peek-at pq idx)
  (when (send pq empty?)
    (error "queue-peek-at: empty priority queue" pq))
  (item-value (send pq peek-at idx)))

(define (priority-of pq idx)
  (when (send pq empty?)
    (error "priority-of: empty priority queue" pq))
  (item-priority (send pq peek-at idx)))
