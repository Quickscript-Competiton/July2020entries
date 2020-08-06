#lang racket/base

#|
    My first submitted Racket Quickscript

    (mostly a proof-of-concept, to try my hand at writing these)

    The highlighted text will be displayed in a World window in
    randomly-colored text, flashing 5 times, then "CLOSE MEEEE"
    will be displayed to warn user to close that World window

    by: Sharon Tuttle, smtuttle@alumni.rice.edu, sharon.tuttle@humboldt.edu
    last modified: 2020-07-12

    License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
             [MIT license](http://opensource.org/licenses/MIT) at your option.
    From: https://github.com/Quickscript-Competiton/July2020entries/issues/9
|#

(require quickscript)

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(script-help-string "The highlighted text will be displayed in a World window in
randomly-colored text, flashing 5 times, then \"CLOSE MEEEE\"
will be displayed to warn user to close that World window")

#|=====
    a Msg-world is a structure:
        (make-msg-world string integer)
    ...representing a message-world with:
        a "main" message it will display, and
        the "lifetime" number of clock ticks it is to be displayed
=====|#

(define-struct msg-world (msg lifetime))

(define MSG-FONT-SIZE 25)
(define CLOSE-ME-SIZE 10)
(define CLOSE-ME-COLOR "black")
(define CLOSE-ME-IMG (text "CLOSE MEEEEE" CLOSE-ME-SIZE CLOSE-ME-COLOR))

#|========
    signature: msg-world-image: Msg-world -> image
    purpose: expects a message-world, and returns an image
        depicting its message in randomly-colored text
========|#

(check-random (msg-world-image (make-msg-world "moo" 100))
              (text "moo" MSG-FONT-SIZE (make-color (random 256)(random 256)
                                                        (random 256))))

(check-random (msg-world-image (make-msg-world "ARRGH" 15))
              (text "ARRGH" MSG-FONT-SIZE (make-color (random 256)(random 256)
                                                          (random 256))))
                
(define (msg-world-image a-msg-world)
    (text (msg-world-msg a-msg-world)
          MSG-FONT-SIZE
          (make-color (random 256)(random 256)(random 256)))    
)

(msg-world-image (make-msg-world "moo" 100))

#|========
    signature: msg-world-scene: Msg-world -> scene
    purpose: expects a message-world, and:
        if the message-world's remaining lifetime is more than 0,
            returns a scene of that message in randomly-colored text
            centered within;
        otherwise it returns a scene of "CLOSE MEEEEEEE" in smaller black text
            centered within
========|#

(check-random (msg-world-scene (make-msg-world "moo" 100))
              (place-image (circle 0.5 "outline" "white")
                           (image-width 
                                  (msg-world-image (make-msg-world "moo" 101)))
                           (image-height
                                  (msg-world-image (make-msg-world "moo" 101)))
                           (empty-scene
                              (* 2 (image-width 
                                       (msg-world-image (make-msg-world "moo" 100))))
                              (* 2 (image-height
                                       (msg-world-image
                                           (make-msg-world "moo" 100)))))))

(check-random (msg-world-scene (make-msg-world "moo" 101))
              (place-image (msg-world-image (make-msg-world "moo" 101))
                           (image-width 
                                  (msg-world-image (make-msg-world "moo" 101)))
                           (image-height
                                  (msg-world-image (make-msg-world "moo" 101)))
                           (empty-scene
                              (* 2 (image-width 
                                       (msg-world-image (make-msg-world "moo" 101))))
                              (* 2 (image-height
                                       (msg-world-image
                                           (make-msg-world "moo" 101)))))))

(check-random (msg-world-scene (make-msg-world "baa" 0))
              (place-image CLOSE-ME-IMG
                           (image-width 
                                  (msg-world-image (make-msg-world "baa" 0)))
                           (image-height
                                  (msg-world-image (make-msg-world "baa" 0)))
                           (empty-scene
                              (* 2 (image-width 
                                       (msg-world-image (make-msg-world "baa" 0))))
                              (* 2 (image-height
                                       (msg-world-image (make-msg-world "baa" 0)))))))

(check-random (msg-world-scene (make-msg-world "la la la" -1))
              (place-image CLOSE-ME-IMG
                           (image-width 
                                  (msg-world-image (make-msg-world "la la la" -1)))
                           (image-height
                                  (msg-world-image (make-msg-world "la la la" -1)))
                           (empty-scene
                              (* 2 (image-width 
                                       (msg-world-image
                                           (make-msg-world "la la la" -1))))
                              (* 2 (image-height
                                       (msg-world-image
                                           (make-msg-world "la la la" -1)))))))

(define (msg-world-scene a-msg-world)
    (define msg (msg-world-msg a-msg-world))
    (define msg-img (msg-world-image a-msg-world))
    (define msg-img-width (image-width msg-img))
    (define msg-img-ht (image-height msg-img))
    (place-image
        (cond
            [(> (msg-world-lifetime a-msg-world) 0)
                (cond [(even? (msg-world-lifetime a-msg-world))
                           (circle 0.5 "outline" "white")]
                      [else msg-img])]
            [else CLOSE-ME-IMG])
        msg-img-width
        msg-img-ht
        (empty-scene
         (* 2 msg-img-width)
         (* 2 msg-img-ht))))

(msg-world-scene (make-msg-world "ARRGH" 15))
(msg-world-scene (make-msg-world "what, me worry?" 20))

#|========
    signature: reduce-lifetime: Msg-world -> Msg-world
    purpose: expects a message-world, and reduces its current
        lifetime value by 1
========|#

(check-expect (msg-world-lifetime (reduce-lifetime (make-msg-world "ARRGH" 15)))
              14)
(check-expect (msg-world-lifetime (reduce-lifetime
                                      (make-msg-world "what, me worry?" 20)))
              19)

(check-expect (msg-world-msg (reduce-lifetime (make-msg-world "ARRGH" 15)))
              "ARRGH")
(check-expect (msg-world-msg (reduce-lifetime
                                      (make-msg-world "what, me worry?" 20)))
              "what, me worry?")

(define (reduce-lifetime a-msg-world)
  (make-msg-world (msg-world-msg a-msg-world)
                  (- (msg-world-lifetime a-msg-world) 1)))

#|========
    signature: lifetime-over?: Msg-world -> boolean
    purpose: expects a message-world, and returns whether its
        current lifetime is 0 or more
========|#

(check-expect (lifetime-over? (make-msg-world "ARRGH" 15))
              #false)

(check-expect (lifetime-over? (make-msg-world "ARRGH" 0))
              #false)

(check-expect (lifetime-over? (make-msg-world "ARRGH" -1))
              #true)

(define (lifetime-over? a-msg-world)
  (cond
     [(< (msg-world-lifetime a-msg-world) 0) #true]
     [else #false]))

(define-script show-highlighted
  #:label "Show-highlighted"
  #:menu-path ("Sele&ction")
  (λ (selection)
      (big-bang (make-msg-world selection 10)
          (to-draw msg-world-scene)
          (on-tick reduce-lifetime 0.5)
          (stop-when lifetime-over?))))

(module+ main
  (show-highlighted "Lookit MEEE!"))

(test)

