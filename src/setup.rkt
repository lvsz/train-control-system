#lang racket/base

(require racket/class
         racket/list
         racket/sequence
         racket/string)

(provide setup%
         setups
         get-setup)


(define setup-location "resources/setups")

(define setup%
  (class object%
    (init-field id contents)
    (super-new)
    (define/public (get-id) id)
    (define/public (get-contents) (contents))))

(define setup-files (directory-list setup-location #:build? #t))

(define setup-names (map path->string (directory-list setup-location)))

(define (make-setup)
  (filter pair? (map string-split (sequence->list (in-lines)))))

(define setups
  (map (lambda (name path)
         (make-object setup%
                      (string->symbol name)
                      (lambda ()
                        (with-input-from-file path make-setup #:mode 'text))))
       setup-names
       setup-files))

(define (get-setup setup-id)
  (let ((setup (for/or ((a-setup (in-list setups))
                        #:when (eq? setup-id (send a-setup get-id)))
                 a-setup)))
    (if setup
      setup
      (error (format "No setup named ~a found" setup-id)))))
