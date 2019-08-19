#lang racket/base

(require (prefix-in railway: "test/railway.rkt")
         (prefix-in infrabel: "test/infrabel.rkt"))

(railway:run)
(infrabel:run)
