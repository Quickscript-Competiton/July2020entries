#lang racket/base

;;; Author: Laurent Orseau https://github.com/Metaxal
;;; License: Apache2.0
;;; From:https://github.com/Quickscript-Competiton/July2020entries/issues/10

(require quickscript
         racket/gui/base
         racket/class
         racket/list
         racket/match
         racket/string
         racket/format
         search-list-box
         syntax/modread)

(script-help-string "List, search, and go to the top level definitions")

(define (file/module->value f)
  (with-input-from-file f
    (λ ()
      (port-count-lines! (current-input-port))
      (with-module-reading-parameterization
        read-syntax))))

(define (file/module-defs f)
  (define mod (file/module->value f))
  (define inmod
    (syntax-case mod ()
    [(_ name lang (_ top-levels ...))
     (syntax-e #'(top-levels ...))]
    [else #f]))
  (and
   inmod
   (filter-map
    (λ (s)
      (match (syntax->datum s)
        [`(define ,head . ,rst)
         (list head (syntax-line s))]
        [else #f]))
    inmod)))

(define-script get-defines
  #:label "Defines"
  #:shortcut f6
  #:shortcut-prefix (ctl)
  (λ (selection #:file f #:frame drr #:definitions ed)
    (define defs (file/module-defs f))
    (when defs
      (set! defs (sort (map (λ (d) (list (~a (first d))
                                         (- (second d) 1))) ; off by 1 syntax/editor
                            defs)
                       string<=? #:key first)))
    (unless defs
      (if f
        (message-box "Defines: Error" "Could not parse file." drr)
        (message-box "Defines: Error" "File must be saved on disk." drr)))

    (define (goto-line sel str content)
      (define line (second content))
      (when line
        (if ed
          (send ed set-position (send ed line-start-position line))
          (message-box "Defines" (format "I want to go to line ~a" line)))))

    (define fr #f)
    (set! fr (new search-list-box-frame% [parent drr]
                  [label "Defines"]
                  [message "Type part of a function name. Enter to go there."]
                  [contents defs]
                  [key first]
                  [callback goto-line]
                  [filter
                   ; Using a regexp filter.
                   ; There's a little trick to avoid calculating the regexp for each lbl,
                   ; but it's not ideal.
                   (let ([str-prev #f]
                         [px #f])
                     (λ (str lbl)
                       (unless (eq? str str-prev)
                         ; This avoids calculating the px for every lbl
                         (set! str-prev str)
                         #;(when fr (send fr set-status ""))
                         (set! px (pregexp str (λ (err)
                                                 (when fr (send fr set-status err))
                                                 px))))
                       (or (not px)
                           (regexp-match px lbl))))]
                  [width 600] [height 400]))

    (define slb (send fr get-search-list-box))
    (send slb set-text selection)
    
    #f))

(module+ drracket
  (file/module-defs "defines.rkt")
  (displayln (get-defines "" #:file "defines.rkt" #:frame #f #:definitions #f)))
