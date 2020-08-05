#lang at-exp racket/base

(require racket/format
         racket/string
         syntax-color/racket-lexer
         quickscript)

; COPY SELECTION AS HTML
; Author: Andre Alves Garzia (contact@andregarzia.com)
; Date: 07-2020
; License: MIT
; From: https://github.com/Quickscript-Competiton/July2020entries/issues/14
;
; This quickscript copies the selection to the clipboard as
; HTML. It uses a pretty <style> and the clever placement of
; <spans> to colorize parens using rainbow colors.
;
; The lexinate function and the CSS associated with the rainbow parens,
; or as I like to describe: "all that is actually interesting and useful
; in this script" is originally made by Erkin Batu Altunbaş, copied from:
;
; https://erkin.party/syntax/
;
; PS: I've did a minor change to lexinate to read from a string port
; instead of the current-input-port.

(script-help-string "Copy selection as HTML with rainbow parens")

(define-script copy-selection-as-html
  #:label "Copy selection as HTML"
  #:menu-path ("Sele&ction")
  #:help-string "Copy selection as HTML with rainbow parens"
  #:output-to clipboard
  (λ (selection) 
    (code->html selection)))

(define selection-as-port "")

(define (lexinate results depth)
  (define (make-span depth str)
    (format "<span class=\"paren-~a\">~a</span>" depth str))
  (define-values (str type paren start end) (racket-lexer selection-as-port))
  (if (not (non-empty-string? str))
      (string-join (reverse results) "")
      (case paren
        ((\( \[ \{)
         (lexinate (cons (make-span depth str) results) (add1 depth)))
        ((\) \] \})
         (lexinate (cons (make-span (sub1 depth) str) results) (sub1 depth)))
        (else
         (lexinate (cons str results) depth)))))

(define style @string-append{
<style type="text/css">
code { white-space: pre; hyphens: none; }

.paren-0  { color: #ef2929; }
.paren-1  { color: #ffaf5f; }
.paren-2  { color: #fce94f; }
.paren-3  { color: #afff00; }
.paren-4  { color: #87ffff; }
.paren-5  { color: #5fafd7; }
.paren-6  { color: #d18aff; }
.paren-7  { color: #ff7bbb; }

.paren-8  { color: #dd0000; }
.paren-9  { color: #ff8700; }
.paren-10 { color: #ffd700; }
.paren-11 { color: #a1db00; }
.paren-12 { color: #87d7af; }
.paren-13 { color: #1f5bff; }
.paren-14 { color: #af5fff; }
.paren-15 { color: #ff4ea3; }

.paren-16 { color: #a40000; }
.paren-17 { color: #ff5d17; }
.paren-18 { color: #c4a000; }
.paren-19 { color: #5faf00; }
.paren-20 { color: #00d7af; }
.paren-21 { color: #005f87; }
.paren-22 { color: #9a08ff; }
.paren-23 { color: #ff1f8b; }
</style>
                             })

(define (code->html source)
  (set! selection-as-port (open-input-string source))
  (string-append style "<code>" (lexinate '() 0) "</code>"))
