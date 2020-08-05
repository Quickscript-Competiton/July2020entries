#lang at-exp racket

(require
  framework
  framework/preferences
  racket/gui
  quickscript)

; -----------------------------------------------------------------------
; Extract colorscheme to package.
;
; Creates a new "info.rkt" in the directory of your choosing with the
; current colorscheme as a new package.
;
; Made for the quickscript competition 2020.
;
; Author: Andre Alves Garzia (contact@andregarzia.com)
; Date: 07-2020
; License: MIT
; From: https://github.com/Quickscript-Competiton/July2020entries/issues/13
; -----------------------------------------------------------------------


(script-help-string "Save the current colorscheme as a new package")

(define-script show-theme
  #:label "Extract colorscheme to package"
  #:menu-path ("&Utils")
  (λ (str) 
    (show-gui)
    #f))

(color-prefs:register-info-based-color-schemes)

(define style-keys
  '(framework:syntax-color:scheme:comment
    framework:syntax-color:scheme:constant
    framework:syntax-color:scheme:error
    framework:syntax-color:scheme:hash-colon-keyword
    framework:syntax-color:scheme:keyword
    framework:syntax-color:scheme:other
    framework:syntax-color:scheme:parenthesis
    framework:syntax-color:scheme:string
    framework:syntax-color:scheme:symbol
    framework:syntax-color:scheme:text))

(define color-keys
  '(framework:basic-canvas-background
    framework:default-text-color
    framework:disabled-background-color
    framework:failed-search-background-color
    framework:line-numbers
    framework:line-numbers-current-line-number-background
    framework:line-numbers-current-line-number-foreground
    framework:line-numbers-when-word-wrapping
    framework:misspelled-text-color
    framework:paren-match-color
    framework:program-contour-current-location-bar
    framework:warning-background-color))

(define (color->vector color)
  (define r (send color red))
  (define g (send color green))
  (define b (send color blue))
  (format "#(~a ~a ~a)" r g b))

(define (color-add->vector color)
  (define r (send color get-r))
  (define g (send color get-g))
  (define b (send color get-b))
  (format "#(~a ~a ~a)" r g b))

(define (style-to-string key)
  (define style (color-prefs:lookup-in-color-scheme key))
  (define color (color-add->vector (send style get-foreground-add)))
  ;(define background (string-append "#s(background " (color-add->vector (send style get-background-add)) ")" ))
  (define weight (format "~a" (send style get-weight-on)))
  (define font-style (format "~a" (send style get-style-on)))
  (define underline (if (send style get-underlined-on) "underline" ""))
  (define style-string (string-join (filter (not/c "normal") (filter (not/c "base")(list color weight font-style underline )))))
  (format
   "(~a ~a)~%"
   key style-string))

(define (color-to-string key)
  (define color (color-prefs:lookup-in-color-scheme key))
  (format
   "(~a ~a)~%"
   key (color->vector color)))

(define styles-as-string
  (string-join (map style-to-string style-keys)))

(define colors-as-string
  (string-join (map color-to-string color-keys)))



(define (template colorscheme-name) @string-append{
 #lang info
 
 (define deps '("base"))
 
 (define framework:color-schemes
 '(#hash(
 (name . "@|colorscheme-name|")
 (white-on-black-base? . @(format "~a" (preferences:get 'framework:white-on-black?)))
 (colors
 .
 (@|colors-as-string|
 @|styles-as-string|)))))
 })

; UI for quickscript

(define frame (new frame%
                   [label "Extract Color Scheme"]
                   [width 400]))

(define v-panel (new vertical-panel%
                     [parent frame]))

(define colorscheme-name-field (new text-field%
                                    (label "Color Scheme Name")
                                    (parent v-panel)
                                    (init-value "My Color Scheme")))

(define h-panel (new horizontal-pane%
                     [parent v-panel]))

(define location (new text-field%
                      (label "Where to save")
                      (parent h-panel)
                      (init-value "")))

(define browse-button (new button% [parent h-panel]
     [label "Browse"]
     [callback (lambda (button event)
                 (define path (get-directory))
                 (send location set-value (path->string path)))]))

(define extract-button (new button% [parent v-panel]
     [label "Extract Color Scheme"]
     [callback (lambda (button event)
                 (define colorscheme-name (send colorscheme-name-field get-value))
                 (define path-string (send location get-value))
                 (define path (if (non-empty-string? path-string) (string->path path-string) #f))
                 (cond
                   [(equal? path #f) (message-box "Error" "Please select folder" #f (list 'caution 'ok))]
                   [(directory-exists? path) (begin0
                                               (with-output-to-file (build-path path "info.rkt")
                                                 (lambda () (display (template colorscheme-name)))
                                                 #:mode 'text
                                                 #:exists 'replace)
                                               (send frame show #f))]
                   [else (message-box "Error" (format "Can't find directory: ~a" path) #f (list 'caution 'ok))]))]))

(define (show-gui)
  (send frame show #t))
