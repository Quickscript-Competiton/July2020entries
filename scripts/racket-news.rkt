#lang racket/base

;;; Author: Stephen De Gabrielle https://github.com/spdegabrielle
;;; License: MIT/Apache2.0
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/8

(require browser/external
         quickscript)

(script-help-string "Racket news and events.")

; Launch https://racket-news.com in browser
(define-script racket-news
  #:label "Racket News (browser)"
  #:menu-path ("&News")
  #:help-string "Opens racket-news.com"
  (λ (str) 
    (send-url "https://racket-news.com")
    #f))

; Launch https://racket-stories.com in browser
(define-script racket-stories
  #:label "Racket Stories (browser)"
  #:menu-path ("&News")
  #:help-string "Opens racket-stories.com"
  (λ (str) 
    (send-url "https://racket-stories.com")
    #f))

; Launch https://blog.racket-lang.org in browser
(define-script racket-blog
  #:label "Racket Blog (browser)"
  #:menu-path ("&News")
  #:help-string "Opens blog.racket-lang.org"
  (λ (str) 
    (send-url "https://blog.racket-lang.org")
    #f))



;----------------------------------------------


; Launch https://school.racket-lang.org/ in browser
(define-script racket-school
  #:label "Racket School (browser)"
  #:menu-path ("&Events")
  #:help-string "Opens school.racket-lang.org"
  (λ (str) 
    (send-url "https://school.racket-lang.org/")
    #f))


; Launch https://racketfest.com/ in browser
(define-script racket-fest
  #:label "RacketFest (browser)"
  #:menu-path ("&Events")
  #:help-string "Opens racketfest.com"
  (λ (str) 
    (send-url "https://racketfest.com/")
    #f))


; Launch https://con.racket-lang.org/ in browser
(define-script racketcon
  #:label "RacketCon (browser)"
  #:menu-path ("&Events")
  #:help-string "Opens con.racket-lang.org"
  (λ (str) 
    (send-url "https://con.racket-lang.org/")
    #f))


; Launch https://www.youtube.com/c/racketlang in browser
(define-script racket-youtube
  #:label "Racket Youtube Channel (browser)"
  #:menu-path ("&News")
  #:help-string "Opens www.youtube.com/c/racketlang"
  (λ (str) 
    (send-url "https://www.youtube.com/c/racketlang")
    #f))
