# Quickscript Competition:

NOTE: Unofficial event run by a Racket user (@spdegabrielle). 

[Quickscript](https://www.cs.utah.edu/plt/snapshots/current/doc/quickscript/index.html) is the scripting functionality behind the DrRacket `Scripts` menu:

For the month of July we will be running a Quickscript competition: Write your own script and win prizes!
There will be weekly winners in categories to be determined by the judges and overall awards at the end of the month. 

We follow Racket's and the ACM's [Code of Conduct](https://racket-lang.org/friendly.html).


## There will be amazing prizes!
* An exclusive badge for your github profile recognising your efforts and contribution to the community. (you must [enable it](https://docs.github.com/en/github/setting-up-and-managing-your-github-user-account/publicizing-or-hiding-organization-membership) )
* If you participate once, you get stickers,
* if you participate twice time, you get also a mug,
* if you participate three times, you get also a t-shirt

(while stocks last. Note - prizes still available)
You can participate more than once.

Prizes to be announced on Racket Users mailing list/google group.

Scripts licensed appropriately will be included in a package for installation and universal fame.

# End date: 31-July - but that is still plenty of time!

## Getting started

[Getting started for the impatient](https://www.cs.utah.edu/plt/snapshots/current/doc/quickscript/index.html#%28part._.Make_your_own_script__.First_simple_example%29) in DrRacket.

See quickscript-extra [[readme](https://github.com/Metaxal/quickscript-extra/blob/master/README.md)] [[scripts](https://github.com/Metaxal/quickscript-extra/tree/master/scripts)] for a bunch of useful and example scripts.

Help and discussion will be available through the [#quickscript-competition](https://racket.slack.com/archives/C0168JZ2QUD) slack channel.
---

---
The example in the documentation is bad - it redefines 'reverse' (which is fine), then attempts to use it.
The working version changes the name of the script (which is just a function) to 'reverse-selection' so it doesn't cause the error:

```
(define-script reverse-selection
  #:label "Reverse"
  (λ (selection)
    (list->string (reverse (string->list selection)))))
```

I still think making scripts is easy and fun but I apologise for not testing the documentation.
here is a script I just made (and tested);

```

#lang racket/base
;;; License: MIT/Apache2.0
(require browser/external
         quickscript)
(script-help-string "Racket Survey.")
(define-script racket-survey
  #:label "Racket Survey (browser)"
  #:menu-path ("&News")
  #:help-string "Complete the Racket Survey now"
  (λ (str) 
    (send-url "https://forms.gle/XeHdgv8R7o2VjBbF9")
    #f))
```

click new script in the menu and give it a name 'survey' this will create a file 'survey.rkt' in the user scripts folder paste in the above, save and click 'compile scripts and reload'.
you will find the script "Racket Survey (browser)" under 'News' in the scripts menu.

----

### debugging

you can use a submodule - I used main below but I should use a drracket submodule for that, and edit the prefs in DrRacket to run the drracket submodule (but not the main, which is for the console)

```
#lang racket/base

(require quickscript)

(define-script reverse-selection
  #:label "Reverse"
  #:help-string "reverses the selection"
  (λ (selection)
    (list->string (reverse (string->list selection)))))

(module+ main
  (reverse-selection "!norahS emocleW"))

```

## how to enter
Once your script is ready, [submit your entry](https://github.com/Quickscript-Competiton/July2020entries/issues/new/choose)!

1. create a [gist](https://docs.github.com/en/github/writing-on-github/creating-gists) or [snippet](https://gitlab.com/snippets/new) or [paste](http://pasterack.org)rack of your script
1. [submit your entry](https://github.com/Quickscript-Competiton/July2020entries/issues/new/choose)


###  Need some [ideas to get started](IDEAS.md)?
