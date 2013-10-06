nock - a didactic Nock evaluator
================================

I've made this hoping to grok Nock in the process.  As far as grokking
Nock goes the jury is still out, but the evaluator turned out to be
kind of cute.  So here it is.

Other lispheads may or may not find this useful.

In case you are wondering what the hell is all this about:

* [Urbit](http://www.urbit.org/)
* [Nock](http://www.urbit.org/2013/08/22/Chapter-2-nock.html)

Syntax matters
--------------

The advertised way to create (and evaluate) Nock terms is by their
read syntaxes.

One of those syntaxes aims to resemble the syntax used by the Nock
spec.  I don't like it much (it masks out several important symbols),
so there's also another syntax that is friendlier to Lisp.

You can write terms either like `?[a b]` or like `{? [a b]}`,
depending on the active readtable.  The syntax is recursive, so you
can put terms inside nouns and they will be automagically evaluated.

As a side effect, the `[]` noun syntax is available on its own (though
probably not very useful, `[a b]` simply expands into `(LIST* a b`).

Usage
-----

0. Install `Quicklisp`, if you haven't already.

1. Put this system somewhere where `ASDF` can find it, or just start
your lisp in the system's directory (or `,cd` into it in `Slime`.  You
do use `Slime`, right?).

1.5. If using ECL (for whatever reason), this helps: `(compile-file #p"nock.asd" :load t)`

2. `(ql:quickload "nock")`

3. `(named-readtables:in-readtable nock:spec-readtable)`.  It's sort
of a mouthful, but only needs to be issued once at the beginning of
the session.

4. `/[7 [[4 5] [6 14 15]]] ;let's try this!`
```
{/ [7 [4 5] 6 14 15]}
 |16| {/ [3 [4 5] 6 14 15]}
 |16| <- [6 14 15]
 |16| {/ [3 6 14 15]}
 |16| <- [14 15]
<- [14 15]
(14 . 15)
```

5. Or you could use `nock:lisp-friendly-readtable` instead, and write
the above term as `{/ [7 [[4 5] [6 14 15]]]}`.

6. Notice how the output above helpfully shows (on the left-hand side)
every reduction that is performed, by spec rule number.  As you become
a more confident nocker and the verbosity starts bothering you, just
turn Nock tracing off by saying `(nock:set-evaluation-mode :traced
nil)`.  And if you are worried of running out of stack due to too many
nested reductions, try `(nock:set-evaluation-mode :tail-recursive t)`.
Those knobs are orthogonal, but tail recursion does tend to make
tracing less informative.
