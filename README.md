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

The only exported way to create Nock terms is by their read syntaxes.

One of those aims to be indistinguashable from the syntax used by the
spec.  I don't like it much (it takes over some characters that are
important in Lisp), so there's also another variety.

So you can use `?[a b]` or `{? [a b]}`, depending on the readtable.
The syntax is recursive, so you can use terms inside nouns -- they
will be automagically evaluated.

As a side effect, the `[]` noun syntax is available on its own (though
probably not very useful).  It expands into a call to `LIST*`.

Usage
-----

0. Install `Quicklisp` if you haven't already.

1. Put this system somewhere where `ASDF` can find it, or just start
your lisp in the system's directory (or `,cd` into it in `Slime`.  You
do use `Slime`, right?).

2. `(ql:quickload "nock")`

3. `(named-readtables:in-readtable nock:spec-readtable)`.  Sort of a
mouthful, but only needed to be issued once at the beginning of the
session.`

4. `(nock:nock /[7 [[4 5] [6 14 15]]]) ;let's try this!`
```
{/ [7 [4 5] 6 14 15]}
 |16| {/ [3 [4 5] 6 14 15]}
 |16| <- [6 14 15]
 |16| {/ [3 6 14 15]}
 |16| <- [14 15]
<- [14 15]
(14 . 15)
```

5. Or could use `nock:lisp-friendly-readtable` instead, and write the
above term as `{/ [7 [[4 5] [6 14 15]]]}`.

6. Note how the output above helpfully shows (on the left-hand side)
the reductions performed, by spec rule number.  As you become a more
confident nocker and the verbosity starts bothering you, just turn
Nock tracing off by saying `(setf nock:*trace* nil)` -- which, in
addition to silence, also brings tail recursion.
