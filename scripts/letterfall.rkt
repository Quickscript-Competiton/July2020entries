#lang racket/base

;;; Author: Laurent Orseau https://github.com/Metaxal
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/6
;;;
;;; Stare at your code falling like rocks.

(require quickscript
         framework/preferences ; to disable syncheck
         racket/class
         racket/string)

(script-help-string "Stare at your code falling like rocks.")


(struct vector2d (vec n-rows n-cols))
(define (make-vector2d n-rows n-cols [v #f])
  (vector2d (make-vector (* n-rows n-cols) v) n-rows n-cols))
(define (vector2d-ref v row col)
  (vector-ref (vector2d-vec v) (+ (* row (vector2d-n-cols v)) col)))
(define (vector2d-set! v row col val)
  (vector-set! (vector2d-vec v) (+ (* row (vector2d-n-cols v)) col) val))

(define-script letterfall
  #:label "Letterfall"
  #:menu-path ("&Games and fun")
  (λ (selection #:definitions ed)
    (define full-text (send ed get-text))
    (define start (box #f))
    (define end (box #f))
    (send ed get-visible-position-range start end)
    (define txt (send ed get-text (unbox start) (unbox end)))
    (define lines (string-split txt "\n"))
    (define n-rows (length lines))
    (define n-cols (apply max (map string-length lines)))
    (define mat (make-vector2d n-rows n-cols #\space))
    (for ([line (in-list lines)]
          [row (in-naturals)]
          #:when #t
          [c (in-string line)]
          [col (in-naturals)])
      (vector2d-set! mat row col c))

    ;; Save syncheck state and deactivate it (temporarily).
    (preferences:set-default 'drracket:online-compilation-default-on #t boolean?)
    (define syncheck? (preferences:get 'drracket:online-compilation-default-on))
    (preferences:set 'drracket:online-compilation-default-on #f)
    
    (for ([i (in-range n-rows)])
      ;; Fall once.
      (for* ([row (in-range (- n-rows 2) -1 -1)]
             [col (in-range n-cols)])
        (when (eqv? #\space (vector2d-ref mat (+ row 1) col))
          (vector2d-set! mat (+ row 1) col (vector2d-ref mat row col))
          (vector2d-set! mat row col #\space)))

      (define new-txt
        (string-join
         (for/list ([row (in-range n-rows)])
           (apply string
                  (for/list ([col (in-range n-cols)])
                    (vector2d-ref mat row col))))
         "\n"))
      (send ed begin-edit-sequence)
      (send ed erase)
      (send ed insert new-txt)
      (send ed end-edit-sequence)
      (sleep #;(/ 1. (+ i 2.)) 0.01))

    (sleep 0.5)
    ;; Restore buffer state.
    #;(send ed begin-edit-sequence #f)
    (for ([i (in-range n-rows)]) ; as many undos as edit sequences before
      (send ed undo)
      (sleep 0.01))
    #;(send ed end-edit-sequence)

    ; Reactivate syncheck.
    (preferences:set 'drracket:online-compilation-default-on syncheck?)
    
    #f))
