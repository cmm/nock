nock - a didactic Nock evaluator
================================

I've made this hoping to grok Nock in the process.  As far as grokking
Nock, the results are somewhat underwhelming so far, but I kind of
like the evaluator, so here it is.

Other lispheads may or may not find this useful.

In case you are wondering what the hell am I on about:

* [Urbit](http://www.urbit.org/)
* [Nock spec](http://www.urbit.org/2013/08/22/Chapter-2-nock.html)

Caveat emptor
-------------

The only exported way to create Nock terms is by their read syntax,
which tries to resemble the spec's syntax (as far as Lisp and taste
allow).

So `?[a b]` in the spec's syntax is `${? [a b]}` here.  Pretty nice, I
think.

Only we clobber the global readtable instead of using something nice
(like `NAMED-READTABLES`), so keep that in mind.

(Why the dollar sign?  No deep reasons, other than expediency and
implementation details leaking outside.  I might change this later).

Usage
-----

0. Install `Quicklisp` if you haven't already.

1. Put this system somewhere where `ASDF` can find it, or just start
your lisp in the system's directory (or `,cd` into it in `Slime`.  You
do use `Slime`, right?).

2. `(asdf:oos 'asdf:load-op "nock")`

3. `(setf nock:*trace-nock-p* t) ;you want tracing, it's cool``

4. `(nock:nock ${/ [7 [[4 5] [6 14 15]]]}) ;let's try this!`
```
{/ [7 [4 5] 6 14 15]}
 |16| {/ [3 [4 5] 6 14 15]}
 |16| <- [6 14 15]
 |16| {/ [3 6 14 15]}
 |16| <- [14 15]
<- [14 15]
(14 . 15)
```

5. Note how the output above helpfully shows (on the left side) the
reductions performed, by spec rule number.  As you become a more
confident nocker and the verbosity starts bothering you, just turn
Nock tracing off -- which, in addition to silence, will also give you
tail recursion.
