#lang racket/base

(provide run)

(require racket/class
         racket/list
         racket/set
         racket/random
         rackunit
         rackunit/text-ui
         "../../railway/setup.rkt"
         "../../railway/railway.rkt"
         "../../railway/adts.rkt")

(define railway (make-object railway% 'hardware))

(define basic-tests
  (test-suite
    "Check basic methods of railway%"
    (test-case
      "Tracks are valid & accessible"
      (check-pred pair? (send railway get-tracks))
      (check-true (andmap track? (send railway get-tracks)))
      (check-true (andmap symbol? (send railway get-track-ids))))
    (test-case
      "Detection blocks are valid & accessible"
      (check-pred pair? (send railway get-detection-blocks))
      (check-true (andmap detection-block? (send railway get-detection-blocks)))
      (check-true (andmap symbol? (send railway get-detection-block-ids))))
    (test-case
      "Switches are valid & accessible"
      (check-pred pair? (send railway get-switches))
      (check-true (andmap switch? (send railway get-switches)))
      (check-true (andmap symbol? (send railway get-switch-ids))))))

(define d-1-3 (send railway get-track '1-3))
(define d-1-4 (send railway get-track '1-4))
(define d-1-5 (send railway get-track '1-5))
(define d-1-8 (send railway get-track '1-8))
(define d-2-4 (send railway get-track '2-4))
(define d-2-6 (send railway get-track '2-6))
(define d-2-7 (send railway get-track '2-7))
(define d-2-8 (send railway get-track '2-8))
(define s-20 (send railway get-track 'S-20))
(define s-2 (send railway get-track 'S-2))
(define s-3 (send railway get-track 'S-3))
(define u-5 (send railway get-track 'U-5))
(define u-6 (send railway get-track 'U-6))

(define component-tests
  (test-suite
    "Check interdependency of components"
    (check-equal? (set d-1-4 s-20)
                  (list->set (send d-1-5 get-connected-segments)))
    (send d-1-5 occupy)
    (check-eq? (send d-1-4 get-status)
               'orange
               "A detection block should turn orange if a neighbor turns red")
    (check-eq? (send s-2 get-position)
               1)
    (send s-3 set-position 1)
    (check-eq? (send s-2 get-position)
               2
               "Changing switch S-3 should set switch S-2 to S-3")))

(define (segment-ids lst)
  (map (lambda (t) (send (send t get-segment) get-id)) lst))

(define route-tests
  (test-suite
    "Route calculation tests"
    (test-case
      "Check correctness of some routes"
      (check-equal?  (segment-ids (send railway get-route d-1-3 d-1-4))
                     '(1-3 S-27 S-26 1-4)
                     "Simple route")
      (check-equal? (segment-ids (send railway get-route d-2-7 d-2-6))
                    '(2-7 S-4 S-8 S-4 2-6)
                    "Route that includes reversing direction")
      (check-equal? (segment-ids (send railway get-alt-route d-1-8 d-2-4 (list u-5)))
                    '(1-8 S-25 S-1 U-6 S-2 S-7 S-5 S-6 S-20 2-4)
                    "Alternative route")
      (check-false (send railway get-alt-route d-1-8 d-2-4 (list u-5 u-6))
                   "Impossible route"))
    (test-suite
      "Random tests"
      (let ((no-switches (filter (lambda (x) (not (switch? x))) (send railway get-tracks))))
        (for ((_ (in-range 50)))
          (let ((from (random-ref no-switches))
                (to (random-ref no-switches))
                (avoid (random-sample no-switches (random 1 10))))
            (let-values
              (((route-1 dist-1) (send railway get-route&distance from to))
               ((route-2 dist-2) (send railway get-alt-route&distance from to avoid)))
              (check <= dist-1 dist-2
                     (format "regular: ~a, alt: ~a, avoid: ~a" route-1 route-2 avoid)))))))))

(define (run)
  (run-tests basic-tests)
  (run-tests component-tests)
  (run-tests route-tests))
