A bare-bones read-eval-print loop (REPL) for the untyped lambda
calculus, written for Haskell and Stack practice.

The lambda-free syntax is inspired by [Alberto
Luiz](http://dis.um.es/~alberto/)'s
[λ-calculus](http://dis.um.es/~alberto/pages/lambdac.html) page.

`lambda-repl` accepts a lambda expression from stdin, and outputs
this expression and a sequence of beta-reductions to stdout:

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

Variables are lowercase ascii characters with optional indices
(e.g. `x23`).

`lambda-repl` uses normal-order reduction (left outermost) by
default. Add the `-a` flag to use applicative-order reduction 
(right innermost):

```
$ lambda-repl -a
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
<enter>
(((a.(b.a)) (((a.(b.a)) (((a.(a a)) (a.(a a))) (a.a))) c)) c)
```

As the above example shows, `lambda-repl` currently stops reducing
expressions either when no further beta-reductions are possible
with the chosen strategy, or when the resultant term evaluates to
itself -- without distinguishing the two cases.
