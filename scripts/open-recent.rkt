#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         quickscript
         quickscript/utils
         framework
         search-list-box)

;;; Author: Laurent Orseau https://github.com/Metaxal
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/15

;;; NOTICE: The package `search-list-box` must be installed first!

(script-help-string "Open a recent file (dialog uses a search-list-box)")

(define-script open-recent
  #:label "&Open recent"
  (λ (selection #:frame drfr)
    (define recent (preferences:get 'framework:recently-opened-files/pos))
    (define fr
      (new search-list-box-frame%
           [parent drfr]
           [label "Open recent"]
           [width 600] [height 400]
           [contents recent]
           [key (λ (c) (path->string (first c)))]
           [filter word-filter]
           [callback
            (λ (sel str content)
              (when content
                (smart-open-file drfr (first content))
                (when (send cb-close get-value)
                  (send fr show #f))))]))
    
    (define cb-close (new check-box% [parent fr]
                          [label "&Close dialog after opening a file?"]
                          [value #t]))
    #f))
