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

(define t1 (make-object track% 't1 n1 n2 100))
(define b1 (make-object block% 'b1 n2 n3 200))
(define t2 (make-object track% 't2 n3 n4 100))
(define t3 (make-object track% 't3 n1 n4 300)) ; order of given nodes shouldn't matter
(define t4 (make-object track% 't4 n4 n5 50))
(define t5 (make-object track% 't5 n4 n7 50))
(define b2 (make-object block% 'b2 n5 n6 50))
(define b3 (make-object block% 'b3 n6 n7 50))
(define s1 (make-object switch% 's1 t3 t4))
(define s2 (make-object switch% 's2 s1 t5))
(define tracks (list t1 b1 t2 t3 t4 t5 b2 b3 s1 s2))

(define node-tests
  (test-suite
    "Tests for node% in tracks.rkt"
    (test-case
      "No node should have more than 2 tracks connected to it"
      (for ((node (in-list nodes)))
        (check >= 2 (length (send node get-tracks)))))))

(define track-tests
  (test-suite
    "Tests for track% in tracks.rkt"
    (test-case
      "When given a connecting node, a track should be able to return the other one"
      (check-eq? n1 (send t1 from n2))
      (check-eq? n2 (send t1 from n1))
      (check-eq? n2 (send b1 from n3))
      (check-eq? n3 (send b1 from n2))
      (check-eq? n1 (send s1 from n4))
      (check-eq? n4 (send s1 from n1)))))

(define block-tests
  (test-suite
    "Tests for block% in tracks.rkt"
    (test-case
      "Detection blocks can update their status and that of any connected block"
      (check-eq? 'green (send b1 get-status))
      (send b1 set-status 'red)
      (check-eq? 'red (send b1 get-status))
      (send b2 set-status 'red)
      (check-eq? 'red (send b2 get-status))
      (check-eq? 'orange (send b3 get-status) "Block b3 should turn orange when connected block b2 turns red")
      (send b2 set-status 'green)
      (check-eq? 'green (send b3 get-status) "Block b3 should turn green when connected block b2 turns green"))))

(define switch-tests
  (test-suite
    "Tests for switch% in tracks.rkt"
    (test-case
      "A switch's from method correctly return connecting node"
      (check-eq? 'n4 (send (send s1 from n1) get-id))
      (check-eq? 'n1 (send (send s1 from n4) get-id))
      (check-eq? #f (send s1 from n5))
      (send s1 change-position)
      (check-eq? #f (send s1 from n1))
      (check-eq? 'n5 (send (send s1 from n4) get-id))
      (check-eq? 'n4 (send (send s1 from n5) get-id)))
    (test-case
      "A switch that embeds other switches can access and control those switches"
      (check = 3 (length (send s2 options-from n4)) "Switch s2 embeds switch s1, so it has 3 options from node n4")
      (check = 1 (length (send s2 options-from n7)) "Switch s2 only has one option when not coming from the shared node")
      (send s2 set-current-track t3)
      (check-eq? 'n1 (send (send s2 from n4) get-id))
      (send s2 set-current-track t4)
      (check-eq? 'n5 (send (send s2 from n4) get-id))
      (send s2 set-current-track t5)
      (check-eq? 'n7 (send (send s2 from n4) get-id))
      (send s2 set-current-track s1)
      (check-eq? (send s1 from n4) (send s2 from n4) "When switched to embedded switch, both will return same value on 'from' call")
      (check-exn exn:fail? (lambda () (send s2 set-current-track t1)) "Cannot be switched to non-connecting track"))))


(for-each run-tests (list node-tests
                          track-tests
                          block-tests
                          switch-tests))

