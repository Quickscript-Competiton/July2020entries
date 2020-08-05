#lang racket/base
(require quickscript racket/string)

;;; Author: Alex Harsányi https://github.com/alex-hhh
;;; License: MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/3

(script-help-string "Formats the selected text to wrap around a 78 character limit.
It is able to detect comments.
[demo](https://gist.github.com/alex-hhh/9577db5c936161546c1a730028491145#gistcomment-3366757)")

(define prefix-rx #px"^\\s*;+\\s*")
(define fill-column 78)

(define (determine-prefix line)
  (define m (regexp-match prefix-rx line))
  (if m (car m) ""))

(define (strip-prefix line)
  (string-trim (regexp-replace prefix-rx line "")))

(define (fill-paragraph lines prefix limit)
  (define words
    (apply append
           (for/list ([line (in-list lines)])
             (string-split line))))
  (define assembled
    (let loop ([completed '()]
               [current ""]
               [remaining words])
      (if (null? remaining)
          (reverse (cons current completed))
          (let ([next (car remaining)])
            (if (> (+ (string-length current) 1 (string-length next)) limit)
                (loop (cons current completed) "" remaining)
                (loop completed (string-append current next " ") (cdr remaining)))))))
  (define prefixed
    (for/list ([line assembled])
      (string-append prefix line)))
  (string-join prefixed "\n"))

(define (fill text)
  (define lines (string-split text #px"[\r\n]"))
  (define prefix (for/first ([line (in-list lines)]
                             #:when (> (string-length (string-trim line)) 0))
                   (determine-prefix line)))
  (define limit (- fill-column (if prefix (string-length prefix) 0)))
  (define slines (for/list ([line (in-list lines)])
                   (strip-prefix line)))
  (define paragraphs
    (let loop ([result '()]
               [current '()]
               [remaining slines])
      (cond ((null? remaining)
             (reverse (if (null? current)
                          result
                          (cons (reverse current) result))))
            ((= 0 (string-length (car remaining)))
             (if (null? current)
                 (loop result '() (cdr remaining))
                 (loop (cons (reverse current) result) '() (cdr remaining))))
            (#t
             (loop result (cons (car remaining) current) (cdr remaining))))))
  (define filled (for/list ([paragraph (in-list paragraphs)]) (fill-paragraph paragraph prefix limit)))
  (string-append (string-join filled (string-append "\n" prefix "\n")) "\n"))

(define-script format-selection
  #:label "Format"
  #:menu-path ("Sele&ction")
  (λ (selection)
    (fill selection)))
