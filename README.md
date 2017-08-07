# Notes on Category Theory and Haskell

The notes can be found here:
- [pdf](https://github.com/jwbuurlage/category-theory-programmers/raw/master/doc/categories_for_programmers.pdf)
- [epub](https://github.com/jwbuurlage/category-theory-programmers/raw/master/doc/categories_for_programmers.epub) (work in progress, missing diagrams and theorems)

This document contains notes on category theory in the context of (functional) programming. Originally they were lecture notes for a seminar hosted at Centrum Wiskunde & Informatica, the national research centre for mathematics and computer science in the Netherlands. The main reason for compiling these notes is to provide a way to gain familiarity with concepts of category theory (and other branches of mathematics) that apply in a broad sense to the field of functional programming.

Although the main focus is on the mathematics, examples are given in Haskell to illustrate how to apply the concepts. In some places, examples are given in other languages as well (such as Python and C++). The topics discussed include:

* Categories
* Types as a category
* Products
* Yoneda
* Closed Cartesian categories
* Adjunctions
* Monads
* F-algebras and recursion
* Comonads
* Lenses

## Generating the document

The notes are written in Markdown, and `pandoc` is used to generate the document. Running `make` inside the `doc` directory should result in an (updated) pdf, granted that `pandoc`, `pandoc-citeproc` and a LaTeX environment are installed.

## Contributing

GitHub can be used for any issues (typos, inaccuracies, ...). Pull requests with fixes or additional material are also very welcome.
