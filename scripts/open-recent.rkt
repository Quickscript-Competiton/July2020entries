#lang racket/base

(require racket/gui/base
         racket/class
         racket/list
         quickscript
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
           [callback
            (λ (sel str content)
              (when content
                (define f (first content))
                (cond
                  [(file-exists? f)
                   (send drfr open-in-new-tab f)
                   (send fr show #f)]
                  [else
                   (message-box "Error"
                                (format "File not found: ~a" f)
                                drfr
                                '(ok stop))])))]))
    #f))
