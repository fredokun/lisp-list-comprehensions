
# List comprehensions in Lisp (tutorial)

```lisp
(list-of (cons i j)
    for i in '(1 2 3 4 5 6 7 8)
    for j in '(A B)
    when (evenp i))
```

    => ((2 . A) (2 . B) (4 . A) (4 . B) (6 . A) (6 . B) (8 . A) (8 . B))


----

(C) 2015 Frederic Peschanski - CC BY SA 3.0
