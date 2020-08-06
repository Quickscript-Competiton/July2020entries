#lang racket/base

#|
Data: 06/07/202

Author: Lambduli
github: @lambduli

This script is intended for teachers and students.
If you are a teacher and frequently ask your students to write some Racket code in front of the class,
you can thank them / give them compliment with this script when they are done.

It produces simple asciiart robo-head saying encouraging words - like this one:

;__________
;| _    _  |    ______________
;|(^)  (^) |   /              |
;|    o    |  /  Nicely done! |
;|  \___/  |  \_______________|
;+---------+

License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
         [MIT license](http://opensource.org/licenses/MIT) at your option.
From: https://github.com/Quickscript-Competiton/July2020entries/issues/2
|#

(require quickscript)

(script-help-string "Produces simple asciiart robo-head saying encouraging words")


(define (robot-say message)
  (string-append
   ";__________"
   "\n;| _    _  |    _"  (build-string (+ 1 (string-length message)) (lambda (i) #\_))
   "\n;|(^)  (^) |   / "  (build-string (+ 1 (string-length message)) (lambda (i) #\ )) "|"
   "\n;|    o    |  /  "   message " |"
   "\n;|  \\___/  |  \\__" (build-string (+ 1 (string-length message)) (lambda (i) #\_)) "|"
   "\n;+---------+"
    ))

; you are free to add more compliments
(define (compliments)
  `("Good job!"
    "Nice one!"
    "Outstanding move!"
    "Amazing form!"
    "Perfection!"
    "Nicely done!"
    "Just perfect!"
    ))


(define (give-compliment n)
  (letrec ([lst (compliments)]
           [index (modulo n (length lst))])
    (list-ref lst index)))


(define-script robopat
  #:label "robopat"
  #:menu-path ("Sele&ction")
  (λ (selection)
    (string-append (robot-say (give-compliment (string-length selection))) "\n" selection)))
