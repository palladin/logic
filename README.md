# logic
A logic programming library for F# based on [miniKanren] and [μKanren]. It is designed to offer an idiomatic F# programming style and also resemble the scheme version of miniKanren.

## Example
The peano function in Scheme-miniKanren
``` scheme
(define peano
  (lambda (n)
    (conde
     ((== 'z n))
     ((fresh (n-)
             (== `(s. ,n-) n)
             (peano n-))))))
             
(run 3 (q) (peano q)) ;; '(z (s. z) (s. (s. z)))
```

## The Reasoned Schemer
For the functional programmer who wants to learn to think logically there is no better introduction than the [The Reasoned Schemer].

![alt text](http://mitpress.mit.edu/sites/default/files/imagecache/booklist_node/9780262562140.jpg "The Book")


[miniKanren]: http://minikanren.org/
[μKanren]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
[The Reasoned Schemer]: http://mitpress.mit.edu/books/reasoned-schemer
