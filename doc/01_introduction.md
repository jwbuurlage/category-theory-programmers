\chapter*{Preface}

This document contains notes for a small-scale seminar on category theory in the context of (functional) programming, organized at Centrum Wiskunde \& Informatica, the national Dutch research centre for mathematics and computer science. The goal of the seminar is to gain familiarity with concepts of category theory (and other branches of mathematics) that apply (in a broad sense) to the field of functional programming.

Although the main focus is on the mathematics, examples are given in Haskell to illustrate how to apply the concepts. In some places, examples are given in other languages as well (such as Python and C++).

I would like to thank:

- Tom Bannink for supplying the proof for the bifunctor example.
- Peter Kristel for valuable comments on the Yoneda embedding
- Willem Jan Palenstijn for corrections and comments regarding cartesian closed categories.
- Tom de Jong for examples and suggestions for the section on adjunctions

Also Matt Noonan, and Ruben Pieters for fixing typos and/or making other improvements to the text.

And everyone else who has attended or contributed to the seminar.

-- Jan-Willem Buurlage (<janwillembuurlage@gmail.com>)

\chapter*{Introduction}

Today, the most common programming style is *imperative*. Imperative programming lets the user describes *how* a program should operate, mostly by directly changing the memory of a computer. Most computer hardware is imperative; a processor executes a machine code sequence, and this sequence is certainly imperative. Imperative programming was first treated by mathematicians such as Turing and von Neuman in the 30s.

A different way of programming is *declarative programming*, which is a way of expressing *what* you want the program to compute (without explicitely saying how it should do this). A good way of expressing what you want to have computed, is by describing your program mathematically, i.e. *using functions*. This exactly what we will explore. The functional style of looking at computations is based on work done in the 20s/30s by Curry and Church among others.

Partically speaking, the difficulty in using a *(typed, pure) functional* programming language, is that the **functions that you write** between types **should behave like mathematical functions** on the corresponding sets. This means, for example, that if you call a function multiple times with the same arguments, it should produce the same result every time. This is often summarized as a *side-effect free function*. More generally, values are in principle immutable.

Something else that would allow us to more accurately describe our programs in a mathematical way is if execution is *lazy* (which is the case in e.g. Haskell). This means we can work with **infinite lists and sequences**, and only peeking inside such a list causes the necessary computations to be done.

In these notes I will assume some 'mathematical maturity' from the reader, but I have tried throughout to keep everything as simple as possible. There is certainly some advanced mathematics to be found, and for those who have not encountered abstract mathematics some sections may be hard to follow. In any case, as a rule there are no overly exotic examples. All the concepts introduced should be accompanied by practical and understandable examples.
