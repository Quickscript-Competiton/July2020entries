#lang racket/base

;; I sometimes include a data file in my program source, and
;; find it useful to sort the data *in the source*
;; (e.g., to make it easier to find something, or just to
;; eliminate the cost of sorting each time the program runs).

;; This file provides four scripts. Each one accepts a *selection*
;; of lines. It does not "parse" the lines: e.g., if you have a
;; '( at the front of one of the lines, that will not be ignored
;; (so make sure all the lines start with the same prefix).

;; The main difficulty is that `read-line` does not distinguish
;; between trailing line separators and their absence. Therefore,
;; if the selection includes a final line-end, that ending will be
;; lost in the result. Thus, if you have something an input file
;; that looks like
#;
'(
  "Joe"
  "Shriram"
  "Ben"
  "Kathi"
  )
;; then selecting through to the end "Kathi" leaves the newline
;; integrity intact, but selecting that whole line will result in
;; that line ending removed, moving the closing paren a line up.

;;; Author: Shriram Krishnamurthi https://github.com/shriram
;;; License: Apache2.0/MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/18


(require quickscript
         racket/port
         racket/list)

(script-help-string "Sort line numerically or alphanumerically, ascending or descending")

(define (sort-selection-by comparator)
  (λ (selection)
    (define text
      (with-input-from-string selection
        (λ ()
          (let reader ()
            (let ([r (read-line)])
              (if (eof-object? r)
                  empty
                  (cons r (reader))))))))
    (define sorted
      (sort text comparator))
    (define interleaved
      (let interleave ([lines sorted])
        (if (empty? lines)
            empty
            (if (empty? (rest lines))
                lines
                (cons (first lines)
                      (cons "\n"
                            (interleave (rest lines))))))))
    (apply string-append interleaved)))

(define-script sort-lines-an-asc
  #:label "sort as alphanumeric, ascending order"
  #:menu-path ("Sort Selected Lines")
  (sort-selection-by string<=?))

(define-script sort-lines-an-desc
  #:label "sort as alphanumeric, descending order"
  #:menu-path ("Sort Selected Lines")
  (sort-selection-by string>=?))

(define-script sort-lines-num-asc
  #:label "sort as numeric, ascending order"
  #:menu-path ("Sort Selected Lines")
  (sort-selection-by
   (lambda (s1 s2)
     (<= (string->number s1) (string->number s2)))))

(define-script sort-lines-num-desc
  #:label "sort as numeric, descending order"
  #:menu-path ("Sort Selected Lines")
  (sort-selection-by
   (lambda (s1 s2)
     (>= (string->number s1) (string->number s2)))))

(module+ main
  (require rackunit)
  (check-equal?
   (sort-lines-an-asc "a
c
b
d")
   "a
b
c
d")

  (check-equal?
   (sort-lines-an-desc "Joe
Ben
Shriram
Kathi")
   "Shriram
Kathi
Joe
Ben")

  (define lines-of-numbers "1/3
02
-5
1
2.3")

  (check-equal?
   (sort-lines-num-asc lines-of-numbers)
   "-5
1/3
1
02
2.3")

  (check-equal?
   (sort-lines-num-desc lines-of-numbers)
   "2.3
02
1
1/3
-5")

  (check-equal?
   (sort-lines-an-asc lines-of-numbers)
   "-5
02
1
1/3
2.3")

  (check-equal?
   (sort-lines-an-desc lines-of-numbers)
   "2.3
1/3
1
02
-5")
  
)
