#lang racket/base

;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/22

#|
    My third submitted Racket Quickscript

    It is blatantly adapted from/inspired from Stephen De Gabrielle's
    Racket Survey script, because that looked like such a neat capability
    (to go to a specific URL).
    
    And then it grew into look, one can make a QuickScript submenu of a specific
    course's related links... including a mailto: link, just to see if it would
    work!

    F20 SPECIFIC - will need to update for future semesters of CS 111

    by: Sharon Tuttle, smtuttle@alumni.rice.edu, sharon.tuttle@humboldt.edu
    last modified: 2020-08-03
|#

(require browser/external
         quickscript)

(script-help-string "CS111 links")

(define-script in-class-examples
  #:label "In-class-examples"
  #:menu-path ("&CS111")
  #:help-string "Link to F20 CS 111 in-class examples" 
  (λ (selection)
    (send-url "http://nrs-projects.humboldt.edu/~st10/f20cs111/111ex-list.php")
    #f))

(define-script public-course-site
  #:label "Public-course-site"
  #:menu-path ("&CS111")
  #:help-string "Link to F20 CS 111 public course web site" 
  (λ (selection)
    (send-url "http://nrs-projects.humboldt.edu/~st10/f20cs111/index.php")
    #f))

(define-script canvas-course-site
  #:label "Canvas-course-site"
  #:menu-path ("&CS111")
  #:help-string "Link to F20 CS 111 Canvas course web site" 
  (λ (selection)
    (send-url "https://canvas.humboldt.edu/courses/45726")
    #f))

(define-script email-instrutor
  #:label "Email-instructor"
  #:menu-path ("&CS111")
  #:help-string "Email F20 CS 111 instructor"
  (λ (selection)
    (send-url "mailto:st10@humboldt.edu")
    #f))
