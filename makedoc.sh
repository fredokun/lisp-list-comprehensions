#!/usr/bin/env sh

python3 ../markdownize/markdownize.py -i ./list-comprehensions.lisp -o ./list-comprehensions.lisp.md -b '#|' -e '|#' -l lisp