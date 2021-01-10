#lang racket/base

(require racket/set
         racket/class
         racket/list
         racket/string
         racket/format
         syntax/parse
         syntax/parse/lib/function-header
         quickscript)

;;; Author: sorawee https://github.com/sorawee
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/17

(script-help-string
 "A proof-of-concept completion with fishy static analysis.")

(define magic-inkantation '#%fishy-completion-magic-inkantation)
(define magic-word '#%fishy-completion-magic-word)

;; The submodule is to override #%top so that it's lenient re: unbound ids
;; It will be required for runtime eval and for syntax, since they are
;; the most common ones.
(define the-submod
  `(module ,magic-inkantation racket/base
     (require (for-syntax racket/base))
     (provide (rename-out [@#%top #%top]))
     (define-syntax (@#%top stx)
       #'(void))))

(define-syntax-class lambda-clause
  (pattern (f:formals body ...)
           #:with (ids ...) #'f.params))

;; visible? :: identifier? -> boolean?
(define (visible? id)
  (define scopes (hash-ref (syntax-debug-info id) 'context (λ () '())))
  (not (for/or ([scope (in-list scopes)])
         (eq? 'macro (vector-ref scope 1)))))

;; id/c = (cons/c identifier? integer?)

;; locals :: (listof ids/c)
(define locals '())
;; phases :: (listof integer?)
(define phases '())

(define-logger fishy-completion)

;; make-id/phase-list :: (listof identifier?) integer? -> (listof ids/c)
(define (make-id/phase-list xs phase)
  (for/list ([x (in-list xs)]) (cons x phase)))

;; walk :: syntax? integer? list? -> void?
(define (walk stx phase ids)
  (define (toplevel-walk stxs phase ids)
    (define current-ids
      (append
       ids
       (let loop ([stxs stxs] [phase phase])
         (append*
          (for/list ([form (in-list stxs)])
            (syntax-parse form
              #:literal-sets ([kernel-literals #:phase phase])
              [(begin-for-syntax form ...)
               (loop (attribute form) (add1 phase))]
              [(define-values (id ...) _)
               (make-id/phase-list (attribute id) phase)]
              [(define-syntaxes (id ...) _)
               (make-id/phase-list (attribute id) phase)]
              [_ '()]))))))
    (for ([form (in-list stxs)]) (walk form phase current-ids)))

  (define (get-current-ids new-ids)
    (append ids (make-id/phase-list new-ids phase)))

  (syntax-parse stx
    #:literal-sets ([kernel-literals #:phase phase])

    [(quote x)
     #:when (eq? (syntax-e #'x) magic-word)

     (define the-candidates
       (for/list ([id (in-list ids)] #:when (visible? (car id)))
         (cons (~s (syntax-e (car id))) (cdr id))))
     (log-fishy-completion-debug "at phase: ~s" phase)
     (log-fishy-completion-debug "found candidates: ~s" the-candidates)
     (set! locals (append the-candidates locals))
     (set! phases (cons phase phases))]

    [(module _ _ (#%plain-module-begin form ...))
     (toplevel-walk (attribute form) 0 '())]
    [(module* _ #f (#%plain-module-begin form ...))
     (toplevel-walk (attribute form) phase ids)]
    [(module* _ _ (#%plain-module-begin form ...))
     (toplevel-walk (attribute form) 0 '())]
    [({~or* #%provide #%declare #%require #%variable-reference} _ ...) (void)]
    [({~or quote quote-syntax #%top} . _) (void)]
    [({~or* #%expression begin begin0
            if #%plain-app with-continuation-mark}
      form ...)
     (for ([form (in-list (attribute form))]) (walk form phase ids))]
    [(begin-for-syntax form ...)
     (for ([form (in-list (attribute form))]) (walk form (add1 phase) ids))]

    [(let-values ([(id ...) e] ...) body ...)
     (for ([form (in-list (attribute e))]) (walk form phase ids))
     (define current-ids (get-current-ids (append* (attribute id))))
     (for ([form (in-list (attribute body))])
       (walk form phase current-ids))]
    [(letrec-values ([(id ...) e] ...) body ...)
     (define current-ids (get-current-ids (append* (attribute id))))
     (for ([form (in-sequences (attribute e) (attribute body))])
       (walk form phase current-ids))]
    [(#%plain-lambda . c:lambda-clause)
     (define current-ids (get-current-ids (attribute c.ids)))
     (for ([form (in-list (attribute c.body))])
       (walk form phase current-ids))]
    [(case-lambda c:lambda-clause ...)
     (for ([c-ids (in-list (attribute c.ids))]
           [c-body (in-list (attribute c.body))])
       (define current-ids (get-current-ids c-ids))
       (for ([form (in-list c-body)])
         (walk form phase current-ids)))]
    [(set! _ form) (walk #'form phase ids)]
    [(define-values _ form) (walk #'form phase ids)]
    [(define-syntaxes _ form) (walk #'form (add1 phase) ids)]
    [:id (void)]
    [_ (error 'fishy-autocompletion
              "unexpected ~e at phase ~e"
              stx
              phase)]))

;; find-candidates :: syntax? (or/c path? #f) -> (listof identifier?)
(define (find-candidates form dir)
  (define stx
    (with-handlers ([exn:fail? (λ (ex)
                                 (log-fishy-completion-warning
                                  "compile-time error: ~s"
                                  ex)
                                 #f)])
      (parameterize ([current-namespace (make-base-namespace)])
        (if dir
            (parameterize ([current-directory dir])
              (expand form))
            (expand form)))))
  (cond
    [stx
     (set! locals '())
     (set! phases '())
     (walk stx 0 '())
     (define phase-set (list->set phases))
     (filter
      values
      (for/list ([group (in-list (group-by car locals))])
        (and (subset? phase-set (list->set (map cdr group)))
             (car (first group)))))]
    [else '()]))

;; the-id :: (or/c #f identifier?)
(define the-id #f)

(define-syntax-class idable
  (pattern :id)
  ;; number could potentially be an identifier once completed
  (pattern :number))

;; replace :: syntax? exact-positive-integer? any/c -> syntax?
(define (replace top-stx position new-stx)
  (syntax-parse top-stx
    [(mod name lang {~and mb-pair (mb . mb-body)})
     (define mb-body*
       (let loop ([stx #'mb-body])
         (syntax-parse stx
           [() this-syntax]
           [(a . b) (datum->syntax this-syntax
                                   (cons (loop #'a) (loop #'b))
                                   this-syntax
                                   this-syntax)]
           [x:idable
            #:when (and (log-fishy-completion-debug
                         "found id: ~s at ~a with span ~a"
                         (syntax-e stx)
                         (syntax-position stx)
                         (syntax-span stx))
                        ;; the above should return void? which is truthy
                        (syntax-source #'x)
                        (syntax-position #'x)
                        (syntax-span #'x)
                        (equal? (syntax-source #'x) (syntax-source top-stx))
                        (<= (add1 (syntax-position #'x))
                            position
                            (+ (syntax-position #'x) (syntax-span #'x))))
            (set! the-id #'x)
            (datum->syntax this-syntax new-stx this-syntax this-syntax)]
           [_ this-syntax])))
     (datum->syntax
      this-syntax
      (list #'mod #'name #'lang
            (datum->syntax
             #'mb-pair
             (list* #'mb
                    the-submod
                    `(require ',magic-inkantation
                              (for-syntax ',magic-inkantation))
                    mb-body*)
             #'mb-pair
             #'mb-pair))
      this-syntax
      this-syntax)]))

;; my-read :: string? -> (or/c #f syntax?)
(define (my-read s)
  (define p (open-input-string s))
  (port-count-lines! p)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (parameterize ([read-accept-reader #t])
      (read-syntax (string->path "dummy") p))))

;; query :: exact-positive-integer? string? (or/c path? #f) ->
;;          (either (values #f #f '()) (values string? string? (listof string?)))
(define (query position code-str dir)
  (log-fishy-completion-info "looking for position: ~a" position)
  (with-cache (list position code-str dir)
    (define orig-stx (my-read code-str))
    (cond
      [orig-stx
       (log-fishy-completion-info "read successfully")
       (set! the-id #f)
       (define replaced (replace orig-stx position (list 'quote magic-word)))
       (cond
         [the-id
          (log-fishy-completion-info "found an id at the indicated position")
          (define as-string (~s (syntax-e the-id)))
          (define as-list (string->list (if (= (+ 2 (string-length as-string))
                                               (syntax-span the-id))
                                            (string-append "|" as-string "|")
                                            as-string)))
          (define-values (left right)
            (split-at as-list (- position (syntax-position the-id))))
          (define left* (list->string left))
          (define right* (list->string right))
          (define candidates (for/list ([x (find-candidates replaced dir)]
                                        #:when (string-prefix? x left*))
                               x))
          (values left* right* (sort candidates string<?))]
         [else (values #f #f '())])]
      [else (values #f #f '())])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cached (cons #f #f))

(define (cache-proc key proc)
  (cond
    [(equal? (car cached) key) (apply values (cdr cached))]
    [else (call-with-values proc
                            (λ xs
                              (set! cached (cons key xs))
                              (apply values xs)))]))

(define-syntax-rule (with-cache key body ...)
  (cache-proc key (λ () body ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from https://github.com/Metaxal/quickscript-extra/blob/master/scripts/dynamic-abbrev.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-script fishy-completion
  #:label "Fishy completion v2"
  #:menu-path ("Re&factor")
  #:shortcut #\m
  #:shortcut-prefix (ctl)
  #:persistent
  (λ (_sel #:editor ed #:definitions d)
    (define pos (send ed get-end-position))
    (define txt (send ed get-text))
    (define fname (send d get-filename))
    (define dir (and fname
                     (let-values ([(base name must-be-dir?) (split-path fname)])
                       (and (path? base) base))))
    (define-values (left right matches)
      (query (add1 pos) txt dir))
    (unless (empty? matches)
      (define mems (member (string-append left right) matches))
      (define str
        (if (and mems (not (empty? (rest mems))))
            (second mems)
            (first matches)))
      (when str
        (define right* (substring str (string-length left)))
        (send ed begin-edit-sequence)
        (send ed delete pos (+ pos (string-length right)))
        (send ed insert right*)
        (send ed set-position pos)
        (send ed end-edit-sequence)
        (set! cached (cons (list (add1 pos) (send ed get-text) dir)
                           (list left right* matches)))))
    #f))
