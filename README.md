nock - a didactic Nock evaluator
================================

I've made this hoping to grok Nock in the process.  As far as grokking
Nock, the results are somewhat underwhelming so far, but I kind of
like the evaluator, so here it is.

Other lispheads may or may not find this useful.

In case you are wondering what the hell am I on about:

* [Urbit](http://www.urbit.org/)
* [Nock spec](http://www.urbit.org/2013/08/22/Chapter-2-nock.html)

Syntax matters
--------------

The only exported way to create Nock terms is by their read syntax,
which tries to resemble the spec's syntax (as far as Lisp and taste
allow).

So `?[a b]` in the spec's syntax is `{? [a b]}` here.  Pretty nice, I
think.  You can freely nest sub-terms inside the noun, they'll be
automagically evaluated.

As a side effect, the `[]` noun syntax is available on its own (though
probably not very useful).  It expands into a call to `LIST*`.

Usage
-----

0. Install `Quicklisp` if you haven't already.

1. Put this system somewhere where `ASDF` can find it, or just start
your lisp in the system's directory (or `,cd` into it in `Slime`.  You
do use `Slime`, right?).

2. `(ql:quickload "nock")`

3. `(named-readtables:in-readtable nock:user-readtable)`.  Sort of a
mouthful, but only needed to be issued once at the beginning of the
session.`

4. `(nock:nock {/ [7 [[4 5] [6 14 15]]]}) ;let's try this!`
```
{/ [7 [4 5] 6 14 15]}
 |16| {/ [3 [4 5] 6 14 15]}
 |16| <- [6 14 15]
 |16| {/ [3 6 14 15]}
 |16| <- [14 15]
<- [14 15]
(14 . 15)
```

5. Note how the output above helpfully shows (on the left-hand side)
the reductions performed, by spec rule number.  As you become a more
confident nocker and the verbosity starts bothering you, just turn
Nock tracing off by saying `(setf nock:*trace* nil)` -- which, in
addition to silence, also brings tail recursion.
