#lang scribble/manual
@(require racket/runtime-path
          racket/dict
          racket/path
          racket/match
          quickscript/base)

@; From: https://github.com/Metaxal/quickscript-extra
@; License: MIT

@;TODO: How to have video links in script-help-string? Use scribble/manual too?

@(define-runtime-path scripts-path "../scripts")

@;; If calling this function is slow, compile the scripts first.
@(define (get-script-help-strings scripts-path)
  (filter
   values
   (for/list ([filename (in-list (directory-list scripts-path #:build? #f))])
     (define filepath (build-path scripts-path filename))
     (and (script-file? filepath)
          (cons (path->string (path-replace-extension filename #""))
                (get-script-help-string filepath))))))
@(define help-strings (get-script-help-strings scripts-path))


@title{Scripts from the Quickscript Competition July 2020}

This is a collection of scripts that result from the @(hyperlink "https://github.com/Metaxal/quickscript" "Quickscript") Competition July 2020.

@section{Installation}

In DrRacket, in @tt{File|Package manager|Source}, enter @tt{quickscript-competition-2020}.

Or, on the command line, type: @tt{raco pkg install quickscript-competition-2020}.

If DrRacket is already running, click on @tt{Scripts|Manage scripts|Reload menu}.

@section{Scripts}

The following scripts are included.

@(itemlist
  (for/list ([(name str) (in-dict help-strings)])
     (if str
       (item @(index name (bold name)) ": "
             (let loop ([str str])
               (match str
                 ;; link
                 [(regexp #px"^(.*)\\[([^]]+)\\]\\(([^)]+)\\)(.*)$" (list _ pre txt link post))
                  (list (loop pre)
                        (hyperlink link txt)
                        (loop post))]
                 [else str])))
       (error 'make-readme.rkt "No script-help-string for ~a" name))))

@section{Customizing}

If the default keybindings, names or submenus are not to you taste, they can be fully customized
using Quickscript's
@hyperlink["https://docs.racket-lang.org/quickscript/index.html?q=quickscripts#%28part._.Shadow_scripts%29"]{shadow scripts}.

Scripts can also be selectively deactivated altogether from the library
(@tt{Scripts|Manage scripts|Library}).

