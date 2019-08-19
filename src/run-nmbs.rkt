#lang racket/base

(require racket/class
         "nmbs/nmbs.rkt"
         "gui.rkt"
         "setup.rkt"
         (prefix-in tcp: "infrabel/interface.rkt")
         (prefix-in local: "infrabel/infrabel.rkt"))

(provide main)

(define (main (setup-id #f) (remote #t))
  (let loop ((args (vector->list (current-command-line-arguments))))
    (unless (null? args)
      (case (car args)
        (("--setup")
          (set! setup-id (string->symbol (cadr args)))
          (loop (cddr args)))
        (("--local")
         (set! remote #f)
         (loop (cdr args)))
        (else
         (eprintf "Unrecognized argument: " (car args))
         (loop (cdr args))))))

  ;; define infrabel, locally or over TCP
  (define infrabel
    (if remote
      (new tcp:infrabel%)
      (new local:infrabel%)))

  ;; define nmbs
  (define nmbs
    (new nmbs%
         (infrabel infrabel)))

  ;; create window & run application
  (void
    (cond
      (setup-id
        (send nmbs initialize (get-setup setup-id))
        (new window% (nmbs nmbs)
             (atexit (lambda () (send nmbs stop)))))
      (else
        (new setup-window%
             (setups setups)
             (callback (lambda (setup)
                         (send nmbs initialize setup)
                         (new window% (nmbs nmbs)
                              (atexit (lambda () (send nmbs stop)))))))))))

(main)
