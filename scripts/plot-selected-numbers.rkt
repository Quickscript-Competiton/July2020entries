#lang racket/base

;;; Author: hasn0life https://github.com/hasn0life
;;; License: Apache2.0/MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/21

(require quickscript
         plot
         racket/string
         racket/list
         racket/class)

(script-help-string "Takes a selection of numbers which are separated by whitespaces and plots them")

(define-script plot
  #:label "Plot"
  #:menu-path ("Sele&ction")
  #:output-to #f
  (λ (selection)
    (make-plot selection)))

(define (make-plot str)
  ;; split the string into a list of numbers
  ;; this will drop strings that cant be converted to numbers
  (let* ([num-lst (filter-map string->number (string-split str))]
         ;; make a list of x axis values
         [x-lst (build-list (length num-lst)(λ (x) x))]
         ;; combine the x values with the y values
         [plot-lst (map vector x-lst num-lst)])
    ;; graph them
    (define fr (plot-frame (lines plot-lst) #:x-label "" #:y-label "" ))
    (send fr show #t)))
