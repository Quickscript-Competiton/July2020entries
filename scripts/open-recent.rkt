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

;; Opens a file in a new tab and returns whether opening was successful.
;; Checks if the file exists and displays a message box otherwise and returns #f.
;; Opens the file in the first tab if drracket is still-untouched?
;; Changes to the corresponding tab if the file is already open.
(define (smart-open-file drfr f)
  (cond
    [(not (file-exists? f))
     (message-box "Error"
                  (format "File not found: ~a" f)
                  drfr
                  '(ok stop))
     #f]
    [(send drfr still-untouched?)
     (send drfr change-to-file f)
     #t]
    [(send drfr find-matching-tab f)
     =>
     (λ (tab)
       (send drfr change-to-tab tab)
       #t)]
    [else
     (send drfr open-in-new-tab f)
     #t]))

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
                (smart-open-file drfr (first content))
                (when (send cb-close get-value)
                  (send fr show #f))))]))
    
    (define cb-close (new check-box% [parent fr]
                          [label "&Close dialog after opening a file?"]
                          [value #t]))
    #f))
