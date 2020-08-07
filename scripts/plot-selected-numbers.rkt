#lang racket/base

;;; Author: hasn0life https://github.com/hasn0life
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/21

(require quickscript
         plot
         racket/string
         racket/list
         racket/class
         data/gvector)

(script-help-string "Takes a selection of numbers which are separated by whitespaces and plots them")

(define selections-vec (make-gvector))

(define-script plot
  #:label "Plot (reuse frame)"
  #:menu-path ("Sele&ction")
  #:persistent
  #:output-to #f
  (λ (selection)
    (make-plot selection)))

(define-script clear-plot
  #:label "Plot (new frame)"
  #:menu-path ("Sele&ction")
  #:persistent
  #:output-to #f
  (λ (selection)
    (set! fr #f) ; but don't close the old frame
    (set! selections-vec (make-gvector))
    (make-plot selection)))

(define fr #f)

(define (make-plot str)
  ;; split the string into a list of numbers
  ;; this will drop strings that cant be converted to numbers
  (let* ([num-lst (filter-map string->number (string-split str))]
         ;; make a list of x axis values
         [x-lst (build-list (length num-lst)(λ (x) x))]
         ;; combine the x values with the y values
         [plot-lst (map vector x-lst num-lst)])
    ;; add to persistent collection
    (gvector-add! selections-vec plot-lst)
    ;; graph them
    (when fr (send fr show #f))
    (set! fr (plot-frame
                (for/list ([i selections-vec]
                           [j (in-range (gvector-count selections-vec))])
                  (lines i #:color j))
                #:x-label "" #:y-label "" ))
    (send fr show #t)))

; 1 3 3 6 6 3 3 1
; 1 2 4 4 2 1 1 2 4 8 8 4 2 1 


