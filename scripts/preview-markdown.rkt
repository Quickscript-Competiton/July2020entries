#lang racket/base

;;; Author: Atharva Raykar https://github.com/tfidfwastaken
;;; License: Apache2.0/MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/19

(require racket/gui/base
         racket/class
         racket/path
         net/sendurl
         markdown
         quickscript)

(script-help-string "Preview current markdown file in a web browser")

(define-script render-markdown
  #:label "Preview Markdown"
  #:help-string "Preview current markdown file in a web browser"
  #:output-to #f
  (λ (selection #:definitions defs)
    (define md-xexprs
      (parse-markdown (send defs get-text)))
    (send-url/contents (xexpr->string `(html
                                        (head [title "Markdown Preview"])
                                        (body ,@md-xexprs))))))
