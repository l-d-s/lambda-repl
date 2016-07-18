A bare-bones read-eval-print loop (REPL) for the untyped lambda
calculus, written for Haskell and Stack practice.

The lambda-free syntax is inspired by [Alberto
Luiz](http://dis.um.es/~alberto/)'s
[λ-calculus](http://dis.um.es/~alberto/pages/lambdac.html) page.

Example usage:

 ```
$ lambda-repl 
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
<enter>
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
((b.(((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
(((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)
((b.(((a.(a a)) (a.(a a))) (a.a))) c)
(((a.(a a)) (a.(a a))) (a.a))
 ```

`lambda-repl` uses normal-order reduction (left outermost) by
default.  To use applicative-order reduction (right innermost):

```
$ lambda-repl 
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
<enter>
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
```

As the above example shows, `lambda-repl` currently stops reducing
expressions either when no further beta-reductions are possible
with the current strategy, or when the resultant term evaluates to
itself -- without distinguishing the two cases.
