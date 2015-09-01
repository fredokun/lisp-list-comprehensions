#!/usr/bin/env sh

python3 ../markdownize/markdownize.py -i ./list-comprehensions.lisp -o ./list-comprehensions.lisp.md -b '#|' -e '|#' -l "lisp"

python3 ../markdownize/markdownize.py -i ./list-comprehensions.lisp -o ./list-comprehensions.lisp-pandoc.md -b '#|' -e '|#' -l "commonlisp"

# "commonlisp" for pandoc !
pandoc -s --highlight-style=pygments list-comprehensions.lisp-pandoc.md -o list-comprehensions.lisp.pdf

rm -f lisp-comprehensions.lisp-pandoc.md


