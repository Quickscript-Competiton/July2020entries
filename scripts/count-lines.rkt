#lang racket/base

;; Count lines
;; Use the selection, if any, otherwise count lines across all definitions.
;;
;; Other ideas:
;; - if the file is read-able, use `syntax-sloc`
;; - find way to count types, contracts, etc.

;;; Author: Ben Greenman https://github.com/bennn
;;; License: Apache2.0/MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/20

(require quickscript
         (only-in racket/class send))

(script-help-string "Count lines in the current selection, or for all definitions.")

(define count-linebreaks
  (let ((newline-char (integer->char 10)))
    (lambda (str)
      (+ 1 ;; for the final line, because it's hard to select the newline at the end of it
         (for/sum ((c (in-string str))
                   #:when (eq? c newline-char))
           1)))))

(define-script line-count
  #:label "line-count"
  #:help-string "Count lines in the current selection, or for all definitions."
  #:output-to message-box
  (λ (selection #:definitions def)
    (define-values [kind num-lines]
      (if (< 0 (string-length selection))
          (values "selection" (count-linebreaks selection))
          (values "definitions" (count-linebreaks (send def get-flattened-text)))))
    (format "Lines in ~a : ~a" kind num-lines)))
