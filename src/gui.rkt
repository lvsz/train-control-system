#lang racket/gui

(require "nmbs.rkt")

(provide setup-window%)

;; maximum loco speed allowed by controller
(define max-speed 300)

;; function used to sort identity symbols in a logical way
;: first on the first character defining their type
;; then on any numerical value in the remainder of the symbol
(define (id<? a b)
  (define (get-nums id)
    (string->number
      (list->string
        (filter char-numeric?
                (string->list
                  (symbol->string id))))))
  (define (get-first-char id)
    (string-ref (symbol->string id) 0))
  (let ((a-nums (get-nums a))
        (b-nums (get-nums b)))
    (if (and a b (char=? (get-first-char a) (get-first-char b)))
      (< a-nums b-nums)
      (symbol<? a b))))

(define pick-setup-panel%
  (class panel%
    (init-field setups nmbs callback)
    (super-new (enabled #t)
               (min-height 50)
               (alignment '(center top)))

    (define setup-select-menu
      (new choice%
           (label "Current setup")
           (choices (map (lambda (x) (symbol->string (send x get-id))) setups))
           (parent this)
           (vert-margin 20)
           (callback
             (lambda (choice evt)
               (thread (lambda ()
                         (let ((idx (send choice get-selection)))
                           (when idx
                             (send nmbs initialize
                                   (list-ref setups idx))
                             (callback)))))))))))

;; subpanel used to add new locomotives
;; uses a list of tracks that have the same id in both nmbs & simulator
(define add-loco-panel%
  (class panel%
    (init-field add-loco starting-spots callback)
    (super-new (enabled #t)
               (min-height 50)
               (alignment '(center top)))

    (new choice%
         (label "Add new locomotive to track")
         (choices (map (lambda (starting-spot)
                         (symbol->string (send starting-spot get-id)))
                       starting-spots))
         (parent this)
         (vert-margin 20)
         (callback
           (lambda (choice evt)
             (let ((idx (send choice get-selection)))
               (when idx
                 (let ((id (gensym "L")))
                   (add-loco id (list-ref starting-spots idx))
                   (callback id)))))))))


;; subpanel used to select a locomotive
;; doesn't show when there are no locomotives
(define loco-select-panel%
  (class panel%
    (init-field locos callback)
    (super-new (enabled (not (null? locos)))
               (min-height 50)
               (vert-margin 50)
               (alignment '(center top)))

    ; function to fully initialize this panel
    (define (enable-loco-select-menu!)
      (set! loco-select-menu (make-loco-select-menu))
      (send this enable #t))

    ; no loco-select-menu when there are no locos
    (define loco-select-menu #f)
    ; if there are loco, enable loco-select-menu
    (unless (null? locos)
      (enable-loco-select-menu!))

    (define (make-loco-select-menu)
      (new choice%
           (label "Active loco")
           (choices (map symbol->string locos))
           (parent this)
           (vert-margin 20)
           (callback
             (lambda (choice evt)
               (thread (lambda ()
               (let ((idx (send choice get-selection)))
                 (when idx
                   (callback (list-ref locos idx))))))))))

    (define/public (add-loco new-loco)
      (cond ((null? locos)
             ; case for first loco added
             (set! locos (list new-loco))
             (enable-loco-select-menu!))
            (else
             ; case for additional loco added
             (set! locos (append locos (list new-loco)))
             (send loco-select-menu append (symbol->string new-loco)))))))


;; main panel for the user to control locomotives
(define loco-panel%
  (class panel%
    (init-field nmbs)
    (super-new (enabled #t)
               (style '(border))
               (alignment '(center top)))

    ; list that keeps all loco ids
    (define loco-list '())

    ; no active loco when there are no locos
    ; otherwise default to first one in list
    (define active-loco
      (if (null?  loco-list)
        #f
        (car loco-list)))

    ; initialize creation menu
    (define add-loco-menu
      (new add-loco-panel%
           (parent this)
           (add-loco (lambda (id track)
                       (send nmbs add-loco id track)))
           (starting-spots (send nmbs get-starting-spots))
           (callback (lambda (loco-id)
                       ; new loco is added, get updated list first
                       (set! loco-list (send nmbs get-loco-ids))
                       ; get selection menu to include new loco
                       (send loco-select-menu add-loco loco-id)
                       ; if first loco, set to active loco
                       (unless active-loco
                         (set! active-loco loco-id))))))

    ; initialize selection menu
    (define loco-select-menu
      (new loco-select-panel%
           (parent add-loco-menu)
           (locos loco-list)
           (callback (lambda (loco-id)
                       ; set active loco to selected loco
                       (set! active-loco loco-id)
                       ; update speed controller to show selected loco's speed
                       (send speed-control set-value
                             (send nmbs get-loco-speed active-loco))))))

    ; initialize slider to control the active loco's speed
    (define speed-control
      (new slider%
           (label "Speed")
           (min-value (- max-speed))
           (max-value max-speed)
           (parent this)
           (vert-margin 100)
           (style '(horizontal vertical-label))
           (init-value 0)
           (enabled #t)
           (callback
             (lambda (slider evt)
               (when active-loco
                 (send nmbs set-loco-speed active-loco
                       (send slider get-value)))))))))


;; main panel to control the railway's switches
(define switch-panel%
  (class vertical-panel%
    (init-field nmbs)
    (super-new (enabled #t)
               (alignment '(center top)))
    (define buttons
        (map (lambda (id)
               (new button%
                    (label (symbol->string id))
                    (parent this)
                    (enabled #t)
                    (vert-margin 0)
                    (horiz-margin 0)
                    (callback
                      (lambda (button evt)
                        (send nmbs change-switch-position id)))))
             (sort (send nmbs get-switch-ids) id<?)))))

(define setup-window%
  (class frame%
    (init-field nmbs setups (atexit void))
    (init (width 100) (height 100))
    (super-new (label "Pick a setup"))

    (define parent-panel
      (new vertical-panel% (parent this)))

    (define setup-panel
      (new pick-setup-panel%
           (setups setups)
           (nmbs nmbs)
           (parent parent-panel)
           (callback (lambda ()
                       (new window%
                            (nmbs nmbs)
                            (setups setups)
                            (atexit atexit))
                       (show #f)))))
    (inherit show)
    (show #t)))

(define window%
  (class frame%
    (init-field nmbs setups (atexit void))
    (init (width 400) (height 400))
    (super-new (label "NMBS")
               (width width)
               (height (+ height 22)))
    (define (key-callback)
      (void))
    (define update-callback void)
    (define buffer-bmp (make-object bitmap% width height))
    (define buffer-bmp-dc (make-object bitmap-dc% buffer-bmp))
    (define layers '())
    (define timer #f)
    (define closed #f)

    (define/augment (on-close)
      (set! closed #t)
      (atexit))

    (define parent-panel
      (new vertical-panel% (parent this)))

    ;(define setups
    ;  (new choice%
    ;       (parent parent-panel)
    ;       (label "setups")
    ;       (choices '("a" "b" "c"))))

    (define bottom-pane
      (new horizontal-pane%
           (parent parent-panel)))

    (define loco-panel
      (new loco-panel%
           (nmbs nmbs)
           (parent bottom-pane)))

    (define switch-panel
      (new switch-panel%
           (nmbs nmbs)
           (parent bottom-pane)))

    (inherit show)
    (show #t)
    (send parent-panel show #t)
    (send loco-panel show #t)
    ))

