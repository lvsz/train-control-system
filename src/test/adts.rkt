#lang racket/base

(require racket/class
         rackunit
         rackunit/text-ui
         "../adts.rkt")

(define n1 (make-object node% 'n1))
(define n2 (make-object node% 'n2))
(define n3 (make-object node% 'n3))
(define n4 (make-object node% 'n4))
(define n5 (make-object node% 'n5))
(define n6 (make-object node% 'n6))
(define n7 (make-object node% 'n7))
(define nodes (list n1 n2 n3 n4 n5 n6 n7))

(define t-n1-n2 (make-object track% 't-n1-n2 n1 n2 100))
(define b-n2-n3 (make-object block% 'b-n2-n3 n2 n3 200))
(define t-n3-n4 (make-object track% 't-n3-n4 n3 n4 100))
(define t-n4-n1 (make-object track% 't-n4-n1 n1 n4 300)) ; order of given nodes shouldn't matter
(define t-n4-n5 (make-object track% 't-n4-n5 n4 n5 50))
(define b-n5-n6 (make-object block% 'b-n5-n6 n5 n6 50))
(define b-n6-n7 (make-object block% 'b-n6-n7 n6 n7 50))
(define s-n4<n1-n5 (make-object switch% 's-n4<n1-n5 t-n4-n1 t-n4-n5))
(define t-n4-n7 (make-object track% 't-n4-n7 n4 n7 50))
(define s-n4<n1-n5-n7 (make-object switch% 's-n4<n1-n5-n7 s-n4<n1-n5 t-n4-n7))
(define tracks (list t-n1-n2 b-n2-n3 t-n3-n4 t-n4-n1 t-n4-n5 b-n5-n6 b-n6-n7 s-n4<n1-n5 t-n4-n7 s-n4<n1-n5-n7))

(define node-tests
  (test-suite
    "Tests for node% in tracks.rkt"
    (test-case
      "No node should have more than 2 tracks connected to it"
      (for ((node (in-list nodes)))
        (check >= 2 (length (send node get-tracks)))))
    (test-case
      "Coming from a track, a node can give the next track"
      (check-eq? b-n2-n3 (send n2 from t-n1-n2))
      (check-eq? t-n1-n2 (send n2 from b-n2-n3))
      (check-eq? (send n5 from t-n4-n5) (send n5 from s-n4<n1-n5)
                 "Coming from a track or a switch that includes that track returns the same result")
      (check-eq? (send n5 from s-n4<n1-n5) (send n5 from s-n4<n1-n5-n7)
                 "Coming from a switch or a switch that includes that switch returns the same result")
      )))

(define track-tests
  (test-suite
    "Tests for track% in tracks.rkt"
    (test-case
      "When given a connecting node, a track should be able to return the other one"
      (check-eq? n1 (send t-n1-n2 from n2))
      (check-eq? n2 (send t-n1-n2 from n1))
      (check-eq? n2 (send b-n2-n3 from n3))
      (check-eq? n3 (send b-n2-n3 from n2))
      (check-eq? n1 (send s-n4<n1-n5 from n4))
      (check-eq? n4 (send s-n4<n1-n5 from n1)))))

(define block-tests
  (test-suite
    "Tests for block% in tracks.rkt"
    (test-case
      "Detection blocks can update their status and that of any connected block"
      (check-eq? 'green (send b-n2-n3 get-status))
      (send b-n2-n3 set-status 'red)
      (check-eq? 'red (send b-n2-n3 get-status))
      (send b-n5-n6 set-status 'red)
      (check-eq? 'red (send b-n5-n6 get-status))
      (check-eq? 'orange (send b-n6-n7 get-status)
                 "Block b-n6-n7 should turn orange when connected block b-n5-n6 turns red")
      (send b-n5-n6 set-status 'green)
      (check-eq? 'green (send b-n6-n7 get-status)
                 "Block b-n6-n7 should turn green when connected block b-n5-n6 turns green"))))

(define switch-tests
  (test-suite
    "Tests for switch% in tracks.rkt"
    (test-case
      "A switch's from method correctly return connecting node"
      (check-eq? 'n4 (send (send s-n4<n1-n5 from n1) get-id))
      (check-eq? 'n1 (send (send s-n4<n1-n5 from n4) get-id))
      (check-eq? #f (send s-n4<n1-n5 from n5))
      (send s-n4<n1-n5 change-position)
      (check-eq? #f (send s-n4<n1-n5 from n1))
      (check-eq? 'n5 (send (send s-n4<n1-n5 from n4) get-id))
      (check-eq? 'n4 (send (send s-n4<n1-n5 from n5) get-id)))
    (test-case
      "A switch that embeds other switches can access those switches"
      (check = 3 (length (send s-n4<n1-n5-n7 options-from n4))
             "Switch s-n4<n1-n5-n7 embeds switch s-n4<n1-n5, so it has 3 options from node n4")
      (check = 1 (length (send s-n4<n1-n5-n7 options-from n7))
             "Switch s-n4<n1-n5-n7 only has one option when not coming from the shared node")
      (send s-n4<n1-n5-n7 set-current-track t-n4-n1)
      (check-eq? 'n1 (send (send s-n4<n1-n5-n7 from n4) get-id))
      (send s-n4<n1-n5-n7 set-current-track t-n4-n5)
      (check-eq? 'n5 (send (send s-n4<n1-n5-n7 from n4) get-id))
      (send s-n4<n1-n5-n7 set-current-track t-n4-n7)
      (check-eq? 'n7 (send (send s-n4<n1-n5-n7 from n4) get-id))
      (send s-n4<n1-n5-n7 set-current-track s-n4<n1-n5)
      (check-eq? (send s-n4<n1-n5 from n4) (send s-n4<n1-n5-n7 from n4)
                 "When switched to embedded switch, both will return same value on 'from' call")
      (check-exn exn:fail? (lambda () (send s-n4<n1-n5-n7 set-current-track t-n1-n2))
                 "Cannot be switched to non-connecting track"))
    (test-case
      "A switch embedded by another switch can change the position of that switch when needed"
      (send s-n4<n1-n5-n7 set-position 2)
      (check = 2 (send s-n4<n1-n5-n7 get-position))
      (send s-n4<n1-n5 set-position 1)
      (check = 1 (send s-n4<n1-n5 get-position))
      (check = 1 (send s-n4<n1-n5-n7 get-position)))
    (test-case
      "Given a track, a switch can return its connected tracks"
      (check-eq? null (send s-n4<n1-n5 options-from-track b-n2-n3))
      (check-eq? t-n4-n1 (car (send s-n4<n1-n5 options-from-track t-n1-n2)))
      (check = 3 (length (send s-n4<n1-n5-n7 options-from-track t-n3-n4))))))



(for-each run-tests (list node-tests
                          track-tests
                          block-tests
                          switch-tests))

