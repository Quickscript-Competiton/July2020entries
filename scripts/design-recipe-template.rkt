#lang racket/base

(require quickscript)

#|
    My second submitted Racket Quickscript

    it isn't using any additional Quickscript capabilities --
    but wanted to see if could I take the highlighted text, assume it
    is a function name, and build an HSU CS-111-compliant design recipe
    template for a function with that name

    The highlighted text is assumed to be the name of a function
    to be designed and written, and is replaced with an opening
    comment block containing an empty signature and purposes statement
    for that function, the beginnings of two check-expects for that
    function, and the beginnings of a functions header for that function,
    followed by an empty-template function body

    by: Sharon Tuttle, smtuttle@alumni.rice.edu, sharon.tuttle@humboldt.edu
    last modified: 2020-07-18

    License: Apache 2.0/MIT
    From: https://github.com/Quickscript-Competiton/July2020entries/issues/11
|#

(script-help-string  "Apply a Design Recipe template to the selected text")

(define-script design-recipe-template
  #:label "Design-recipe-template"
  #:menu-path ("Sele&ction")
  (λ (selection)
      (string-append
          "\n"
          ";========\n"
          "; signature: "
          selection
          ": ... -> ...\n"
          "; purpose: expects ...\n"
          ";     and returns ...\n"
          "\n"
          "(check-expect ("
          selection
          " ...) ...)\n\n"
          "(check-expect ("
          selection
          " ...) ...)\n\n"
          "(define ("
          selection
          " ...)\n"
          "    ...\n"
          ")\n"
          "\n")))
