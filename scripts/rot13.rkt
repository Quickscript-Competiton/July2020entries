#lang racket/base

#|
Date: 02/07/2020

Author: Francesco "Karrq" Dainese
github: @Karrq

This is a rot13 cipher implementation for quickscript.

License: MIT/Apache 2
From: https://github.com/Quickscript-Competiton/July2020entries/issues/1

Made for July 2020 Quickscript Competition
|#

(require quickscript)

(script-help-string "Rot13 cipher of the selected text")

;rot13 alphabet
(define alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;find the index in the alphabet of a given character
(define (find-pos char)
  (for/first ([c (in-string alphabet)]
              [i (in-range (string-length alphabet))]
              #:when (eq? (char-upcase char) c))
    i))

;retrieve a character from the alphabet given the index
;and if the character was uppercase
(define (get-letter idx upcase)
  (define c (string-ref alphabet idx))
  (if upcase
      c
      (char-downcase c)))

;apply rot13 to a single character
(define (rot13-impl c)
  (define pos (find-pos c))
  (if pos
     (get-letter (remainder (+ 13 pos) 26) (char-upper-case? c))
     c))

;apply rot13 to selection
(define-script rot13
  #:label "Rot13 Encode/Decode"
  #:menu-path ("Sele&ction")
  #:help-string "Encodes or decodes a string using rot13 cipher"
  (λ (selection)
    (list->string (map rot13-impl (string->list selection)))))
