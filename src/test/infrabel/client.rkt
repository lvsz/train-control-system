#lang racket/base

(provide run)

(require racket/class
         rackunit
         rackunit/text-ui
         "../../infrabel/server.rkt"
         (prefix-in local: "../../infrabel/infrabel.rkt")
         (prefix-in tcp: "../../infrabel/client.rkt"))

(void (thread (lambda () (start-server #f))))

(define infrabel (new local:infrabel%))
(define client (new tcp:infrabel% (log? #f)))

(define tcp-tests
  (test-suite
    "Check whether the infrabel client returns the same result as infrabel itself"
    (send client initialize 'hardware)
    (send infrabel initialize 'hardware)
    (send client start)
    (send infrabel start)
    (check-equal? (send infrabel get-detection-block-ids)
                  (send client get-detection-block-ids))
    (check-equal? (send infrabel get-detection-block-statuses)
                  (send client get-detection-block-statuses))
    (send infrabel add-loco 'loco '1-4 '1-5)
    (send client add-loco 'loco '1-4 '1-5)
    (check-eq? (send infrabel get-loco-detection-block 'loco)
               (send client get-loco-detection-block 'loco))
    (check-equal? (send infrabel get-detection-block-statuses)
                  (send client get-detection-block-statuses))
    (send infrabel stop)
    (send client stop)))

(define (run)
  (run-tests tcp-tests))

