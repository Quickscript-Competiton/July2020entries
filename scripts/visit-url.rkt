#lang racket

(require quickscript racket/gui/base browser/external)
(module+ test (require rackunit))

;; Author: Robby Findler https://github.com/rfindler
;; http://racket-lang.org/
;; License: [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/)
;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/16

(script-help-string "visit url at insertion point")

(define-script visit-url-at-insertion-point/macos
  #:label "visit url at insertion point"
  #:shortcut #\u
  #:shortcut-prefix (ctl option)
  #:os-types (macosx)
  (λ (selection #:editor txt)
    (visit-url-at-insertion-point txt)))

(define-script visit-url-at-insertion-point/windows-unix
  #:label "visit url at insertion point"
  #:shortcut #\u
  #:shortcut-prefix (ctl alt)
  #:os-types (windows unix)
  (λ (selection #:editor txt)
    (visit-url-at-insertion-point txt)))

(define (visit-url-at-insertion-point txt)
  (define url (fetch-url-at-insertion-point txt))
  (cond
    [url (send-url url)]
    [else (bell)]))

(define (fetch-url-at-insertion-point txt)
  (define sp (send txt get-start-position))
  (let/ec k
    (when (= sp (send txt get-end-position))
      (define before-url (scan txt sp -1 char-whitespace?))
      (when before-url
        (define url-start (+ before-url 1))
        (when (matches? txt url-start "http")
          (define url-end (scan txt url-start 1 char-whitespace?))
          (when url-end
            (k (send txt get-text url-start url-end))))))
    #f))

(define (matches? txt start str)
  (for/and ([char (in-string str)]
            [pos (in-naturals start)])
    (equal? (send txt get-character pos) char)))
(module+ test
  (let ()
    (define txt (new text%))
    (send txt insert "abcdef")
    (check-true (matches? txt 1 ""))
    (check-true (matches? txt 1 "b"))
    (check-true (matches? txt 1 "bc"))
    (check-true (matches? txt 1 "bcd"))
    (check-true (matches? txt 1 "bcdef"))
    (check-false (matches? txt 1 "a"))
    (check-false (matches? txt 1 "bcdeg"))
    (check-false (matches? txt 1 "bcdefg"))))

(define (scan txt pos dir matching-char?)
  (define lp (send txt last-position))
  (let loop ([pos pos])
    (cond
      [(<= 0 pos (- lp 1))
       (cond
         [(matching-char? (send txt get-character pos))
          pos]
         [else (loop (+ pos dir))])]
      [else #f])))

(module+ test
  (let ()
    (define t (new text%))
    (send t insert "abcd")
    (check-equal? (scan t 2 -1 (λ (x) (member x '(#\a)))) 0)
    (check-equal? (scan t 2 -1 (λ (x) (member x '(#\b)))) 1)
    (check-equal? (scan t 2 -1 (λ (x) (member x '(#\c)))) 2)
    (check-equal? (scan t 2 -1 (λ (x) (member x '(#\d)))) #f)
    (check-equal? (scan t 2 1 (λ (x) (member x '(#\a)))) #f)
    (check-equal? (scan t 2 1 (λ (x) (member x '(#\b)))) #f)
    (check-equal? (scan t 2 1 (λ (x) (member x '(#\c)))) 2)
    (check-equal? (scan t 2 1 (λ (x) (member x '(#\d)))) 3)))
