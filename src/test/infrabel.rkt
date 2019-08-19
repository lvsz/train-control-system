#lang racket/base

(provide run)

(require (prefix-in client: "infrabel/client.rkt"))

(define (run)
  (client:run))

