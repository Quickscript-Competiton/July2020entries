# Scripts from the Quickscript Competition July 2020

This is a collection of scripts that result from the
[Quickscript](https://github.com/Metaxal/quickscript) Competition July
2020.

Each script has its own license (either MIT, Apache 2.0, both, or
CC-BY).

## 1. Installation

In DrRacket, in `File|Package manager|Source`, enter
`quickscript-competition-2020`.

Or, on the command line, type: `raco pkg install
quickscript-competition-2020`.

If DrRacket is already running, click on `Scripts|Manage scripts|Reload
menu`.

## 2. Scripts

The following scripts are included.

* **breakout**: Breakout game. Move: left and right arrows. New ball: b.
  Reset: r.

* **colorscheme2package**: Save the current colorscheme as a new package

* **copy-selection-as-html**: Copy selection as HTML with rainbow parens

* **count-lines**: Count lines in the current selection, or for all
  definitions.

* **cs111-course-links**: CS111 links

* **cve-search**: A function to help you gather CVE information, if you
  use it with a text slection it will try and work out if there any CVEs
  referenced by your selection

* **defines**: List, search, and go to the top level definitions

* **design-recipe-template**: Apply a Design Recipe template to the
  selected text

* **eyes**: Eyeballs are following you.

* **fishy-completion**: A proof-of-concept completion with fishy static
  analysis.

* **format-selection**: Formats the selected text to wrap around a 78
  character limit. It is able to detect comments.
  [demo](https://gist.github.com/alex-hhh/9577db5c936161546c1a730028491145#gistcomment-3366757)

* **letterfall**: Stare at your code falling like rocks.

* **open-recent**: Open a recent file (dialog uses a search-list-box)

* **plot-selected-numbers**: Takes a selection of numbers which are
  separated by whitespaces and plots them

* **preview-markdown**: Preview current markdown file in a web browser

* **racket-news**: Racket news and events.

* **robopat**: Produces simple asciiart robo-head saying encouraging
  words

* **rot13**: Rot13 cipher of the selected text

* **show-highlighted**: The highlighted text will be displayed in a
  World window in randomly-colored text, flashing 5 times, then "CLOSE
  MEEEE" will be displayed to warn user to close that World window

* **sort-lines**: Sort line numerically or alphanumerically, ascending
  or descending

* **visit-url**: visit url at insertion point

## 3. Customizing

If the default keybindings, names or submenus are not to your taste,
they can be fully customized using Quickscript’s [shadow
scripts](https://docs.racket-lang.org/quickscript/index.html?q=quickscripts#%28part._.Shadow_scripts%29).

Scripts can also be selectively deactivated from the library
\(`Scripts|Manage scripts|Library`).
