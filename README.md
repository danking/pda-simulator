# Push-down Automaton Simulator

## Dependencies

This package depends on [rackdan/maybe](https://github.com/danking/rackdan)
which is a package for the new Racket package manager. You can install it with
the command

    raco pkg install github://github.com/danking/rackdan/master

If you're not cool with that--and I totally get it--you can just clone that repo
somewhere and run

    for i in *.rkt; sed -i.bak -e 's:rackdan/maybe:"RELATIVE_PATH_TO_RACKDAN_FOLDER/maybe.rkt"a:' $i

on this folder. You can delete all the `.bak` files if you think the command
worked correctly.

## Example

If you want a ready made example, check `main.rkt` for an example grammar from
the `LL(1)` parsing section of Grune & Jacobs
[Grune, 8.2.4 "Full LL(1) Parsing"].

    S -> a A b | b  a
    A -> c S | ε

## Overview

This is a simple Push-down automaton simulator. There are two primary data
types:

```racket
(state [name : String]
       [description : String]
       [id : Natural]
       [transition-table : (U False [TT StackPeek InputPeek Stack Input])]
       [final? : Boolean]
       )
```

and

```racket
(pda-instance [state : (State StackPeek InputPeek Stack Input)]
              [stack : Stack]
              [input : Input]
              )
```

I'm playing fast and loose with the Typed Racket syntax but I think you get the
idea. The `name`, `description`, and `id` fields are arbitrary, for now. In the
future, I'd like to produce dot graphs including the description and/or
name. The `id` field might be useful for running static analyses on these PDAs.

I don't recommend producing the transition table by hand. The `rule-helpers.rkt`
file contains a `(rules ...)` form which allows for rules of the form

```Racket
(rules StackPeek InputPeek Stack Input
       ('A 's   (nt=> (list 's 'b))))
```

which corresponds to a rule of the form

    A -> s b

Where `A` is the non-terminal and `s` and `b` are the terminals.

The `nt=>` form doesn't consume anything from the input-string.

The `t=>` form does consume a single token from the input-string.

Both forms always pop the top element off the stack.

These forms are all inspired by encoding the pda corresponding to an `LL(1)`
parser, so they are probably awkward for other kinds of parsers. In particular,
there probably should be forms which drop more than a single token from the
input for `LL(k) k>1`, parsers.

I think `LALR` and `LR` parsers PDAs could be encoded pretty naturally into this
description as well. I think these parsers would require `=>`'s that pop more
than one element off of the stack at once.

The `StackPeek` and `InputPeek` types correspond to whatever representation of
peeking you're using. For an `LL(3)` parser, then your input peek is probably a
list of three input elements.

```racket
(define-type InputElement Symbol)
(define-type Input [Listof InputElement])
(define-type InputPeek [List InputElement InputElement InputElement])
```

All the computation is done by the `runPDA` prcoedure. The type of htis
procedure is a bit intimidating at first glance, but I assure you it is quite
simple.

```racket
(: runPDA : (All (StackPeek InputPeek Stack Input)
                 ([PDAInstance StackPeek InputPeek Stack Input]
                  (Stack -> [Maybe StackPeek])
                  (Input -> [Maybe InputPeek])
                  ([PDAInstance StackPeek InputPeek Stack Input] -> Boolean)
                  ->
                  [PDAInstance StackPeek InputPeek Stack Input])))

(define (runPDA pdai stack-peeker input-peeker final?)
  ...)
```

It takes a `pda-instance` which represents the initial conditions of your
pda. The initial conditions for most PDA runs should be the start state, an
empty stack, and the input stream to parse.

It also takes a `stack-peeker` and an `input-peeker` which should convert your
stack and input stream into some smaller representation that the PDA will use to
determine which transition to take.

The `final?` predicate defines the final state of the pda.

## References

Grune, Dick and Jacobs, Ceriel J.H. "Parsing Techniques: A Paractical
Guide". Springer. 2008. ISBN: 978-0-387-20248-8.
