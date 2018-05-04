# TriCats

A set of tools to perform calculations with trivalent diagrams in *Mathematica*.

*Written by* Deniz Stiegemann.

## Table of Contents

* [**Installation & Setup**](#installation-setup)
* [**List of Files in the Repository**](#list-of-files-in-the-repository)
* [**Background**](#background)
* [**Documentation**](#documentation)
	* [Analysis of Trivalent Diagrams](#analysis-of-trivalent-diagrams)<br />
    	b, Components, d, Diagram, dimC4, FindDiagramIsomorphisms, IsomorphicDiagramQ, MakeGraphs, ReduceDiagram, ReduceSquares, t
    * [Operations on Diagrams](#operations-on-diagrams)<br />
    	ConnectAt, DiagramCompose, DiagramConjugate, DiagramFlipH, DiagramMoveDown, DiagramMoveUp, DiagramNorm, DiagramRotate, DiagramScalar, DiagramTensor, DiagramTrace
    * [Other Tools](#other-tools)<br />
    	Bilinearize, ConjugateLinearize, EnsureGraph, EnsureMatrix, Linearize, Sesquilinearize
    * [Libraries](#libraries)<br />
    	ClearLibrary, Description, LoadLibrary, Retrieve
* [**Acknowledgements**](#acknowledgements)
* [**License**](#license)

## Installation & Setup

You need *Mathematica* 11.2 or higher to run the package.

The easiest way to get started is to copy the two files

* `TriCats.m`
* `stdlib.m`

into the folder that also contains the notebook in which you want to use the package. You can then load the package with the following two lines:

```mathematica
SetDirectory[NotebookDirectory[]];
<< TriCats`;
```

## List of Files in the Repository

File | Description
--- | ---
`LICENSE` | full license statement
`README.md` | readme and short documentation (this file)
`stdlib.m` | standard library, which contains shortcuts for frequently-used diagrams and relations
`TriCats.m` | contains the package code


## Background

The main reference is

* S. Morrison, E. Peters, and N. Snyder. _Categories generated by a trivalent vertex._ Selecta Mathematica **23** (2017), no. 2, pp. 817–868. doi:[10.1007/s00029-016-0240-3](https://doi.org/10.1007/s00029-016-0240-3), arXiv:[1501.06869](https://arxiv.org/abs/1501.06869),

where trivalent categories were introduced and classified for a large variety of dimensions. The arXiv source contains notebooks with which the authors did some of the calculations and where they implement many functions also found in this package.


## Documentation

After having loaded the package, you can use

```mathematica
?TriCats`*
```

to obtain a list of all symbols and functions introduced by the **TriCats** package, together with their usage descriptions.

:warning: Please note that the package does not yet have any exception handling, so you are always expected to enter valid arguments, and errors might not always be visible.


### Analysis of Trivalent Diagrams

#### b

`b` represents the formal bigon parameter of a trivalent category. It is also an option of functions such as `ReduceDiagram`, having the symbol `b` as its default value.

#### Components

`Components[expr, diagrams]` gives the coefficients that the diagrams specified in the list `diagrams` have in the linear combination `expr` of diagrams.

`expr` must be an expanded expression of the form
```mathematica
c1 Diagram[…] + c2 Diagram[…] + …
```
which can be obtained by applying `Expand[expr]`.

#### d

`d` represents the formal loop parameter of a trivalent category. It is also an option of functions such as `ReduceDiagram`, having the symbol `d` as its default value.

#### Diagram

`Diagram[a, in, out]`<br />
represents a diagram with adjacency matrix `a`, ingoing legs `in`, and outgoing legs `out`.

`Diagram[a]`<br />
represents a diagram with adjacency matrix `a` and no exernal legs. Equivalent to `Diagram[a,{},{}]`.

`Diagram[g, in, out]` and `Diagram[g]`<br />
represent diagrams with graph `g`.

Legs are represented by 1-valent vertices. Obsolete 2-valent vertices (i.e. 2-valent vertices that are not loops) are allowed and correctly removed by `ReduceDiagram`.

The convention for graphical representations of diagrams is that ingoing legs are located at the bottom of a diagram and outgoing legs at the top.

#### dimC4

`dimC4` represents the dimension of C4 of a trivalent category, the linear space of diagrams with four external legs. It is an option of functions such as `ReduceDiagram`, where it is relevant for substituting squares.
In this case, the default value is 4.

#### FindDiagramIsomorphisms

`FindDiagramIsomorphisms[diagram1, diagram2]` finds all graph isomorphisms from `diagram1` to `diagram2` that correctly map open legs.

#### IsomorphicDiagramQ

`IsomorphicDiagramQ[diagram1, diagram2]` yields `True` if `diagram1` and `diagram2` are isomorphic, and `False` ortherwise.

#### MakeGraphs

`MakeGraphs[expr]` gives a list of graphs for all adjacency matrices occuring in `expr`.

#### ReduceDiagram

`ReduceDiagram[diagram]` simplifies `diagram` by removing 2-valent vertices and applying substitution rules for loops, lollipops, bigons, triangles, and squares. If the diagram has no external legs and can be completely reduced, an expression in terms of only d, b, and t is returned. Otherwise, the simplified form of `diagram` is returned.

`ReduceDiagram` admits the following options:

Option | Default | Description
--- | --- | ---
[b](#b) | b | bigon parameter
[d](#d) | d | loop parameter
[dimC4](#dimc4) | 4 | dimension of C4
[ReduceSquares](#reducesquares) | True | whether to reduce squares
[t](#t) | t | triangle parameter

Note that unless there is no other way of reducing a diagram, it is often helpful to set `ReduceSquares->False` in order to avoid complicated return values.

`ReduceDiagram` is linear with respect to expressions with head `Diagram`.

#### ReduceSquares

`ReduceSquares` is an option of `ReduceDiagram` that specifies whether squares should be reduced. Its default value is `True`.

Note that unless there is no other way of reducing a diagram, it is often helpful to set `ReduceSquares->False` in order to avoid complicated return values when calling `ReduceDiagram`.

#### t

`t` represents the formal triangle parameter of a trivalent category. It is also an option of functions such as `ReduceDiagram`, having the symbol `t` as its default value.


### Operations on Diagrams

#### ConnectAt

`ConnectAt[a1, a2, legs1, legs2]` is a low-level function that gives the adjacency matrix obtained by connecting the legs `legs1` of `a1` to the legs `legs2` of `a2`.

#### DiagramCompose

`DiagramCompose[diagram1, diagram2]` gives the diagram obtained by composing `diagram1` and `diagram2`.

`DiagramCompose[diagram1, diagram2, …]` composes a finite sequence of diagrams.

`DiagramCompose` is bilinear with respect to expressions with head `Diagram`.

#### DiagramConjugate

`DiagramConjugate[diagram]` gives `diagram` reflected horizontally by swapping ingoing with outgoing legs.

`DiagramConjugate` is the conjugate-linear version of `DiagramFlipH` and therefore more useful for computations.

`DiagramConjugate` is conjugate-linear with respect to expressions with head `Diagram`.

#### DiagramFlipH

`DiagramFlipH[diagram]` gives `diagram` reflected horizontally by exchanging the lists of in and out vertices.

`DiagramConjugate` is the conjugate-linear version of `DiagramFlipH` and therefore more useful for computations.

#### DiagramMoveDown

`DiagramMoveDown[diagram,n]` takes the `n` rightmost outgoing legs of `diagram` and makes them ingoing legs in reverse order.

`DiagramMoveDown[diagram,-n]` takes the `n` leftmost outgoing legs of `diagram` and makes them ingoing legs in reverse order.

`DiagramMoveDown` is linear with respect to expressions with head `Diagram`.

#### DiagramMoveUp

`DiagramMoveUp[diagram,n]` takes the `n` rightmost ingoing legs of `diagram` and makes them outgoing legs in reverse order.

`DiagramMoveUp[diagram,-n]` takes the `n` leftmost ingoing legs of `diagram` and makes them outgoing legs in reverse order.

`DiagramMoveUp` is linear with respect to expressions with head `Diagram`.

#### DiagramNorm

`DiagramNorm[diagram]` gives the norm of `diagram`.

`DiagramNorm` uses `ReduceDiagram` to compute the value of the scalar product of `diagram` with itself. Options to be used by `ReduceDiagram` can be specified as options for `DiagramNorm` and are passed along.

`DiagramNorm` uses `DiagramScalar` and therefore supports linear combinations of expressions with head `Diagram` as input.

#### DiagramRotate

`DiagramRotate[diagram]` gives `diagram` rotated by 180 degrees, i.e. the lists of ingoing and outgoing legs are swapped and each reversed.

`DiagramRotate` is linear with respect to expressions with head `Diagram`.

#### DiagramScalar

`DiagramScalar[diagram1, diagram2]` gives the scalar product of `diagram1` and `diagram2`.

`DiagramScalar` is sesquilinear with respect to expressions with head `Diagram`, i.e. conjugate-linear in the first and linear in the second argument.

#### DiagramTensor

`DiagramTensor[diagram1, diagram2]` gives the tensor product of the diagrams `diagram1` and `diagram2`.

`DiagramTensor` is bilinear with respect to expressions with head `Diagram`.

#### DiagramTrace

`DiagramTrace[diagram]` gives the trace of `diagram`.

`DiagramTrace` is linear with respect to expressions with head `Diagram`.

### Other Tools

#### Bilinearize

`Bilinearize[f]` makes the function `f` bilinear with respect to expressions with head `Diagram`.

`f` can be any function of two arguments which has already been defined for expressions with head `Diagram` in the following way:
```mathematica
f[diagram1_Diagram, diagram2_Diagram] := expr
```

#### ConjugateLinearize

`ConjugateLinearize[f]` makes the function `f` conjugate-linear, in its first argument, with respect to expressions with head `Diagram`.

`f` can be any function which has already been defined for expressions with head `Diagram` in the following way:
```mathematica
f[diagram_Diagram, …] := expr
```
`f` can have more than one argument.

#### EnsureGraph

`EnsureGraph[expr]` replaces adjacency matrices with graphs, if
necessary, in all diagrams occuring in `expr`.

#### EnsureMatrix

`EnsureMatrix[expr]` replaces graphs with adjacency matrices, if
necessary, in all diagrams occuring in `expr`.

#### Linearize

`Linearize[f]` makes the function `f` linear, in its first argument, with respect to expressions with head `Diagram`.

`f` can be any function which has already been defined for expressions with head `Diagram` in the following way:
```mathematica
f[diagram_Diagram, …] := expr
```
`f` can have more than one argument.

#### Sesquilinearize

`Sesquilinearize[f]` makes the function `f` sesquilinear with respect to expressions with head `Diagram`, i.e. conjugate-linear in its first and linear in its second argument.

`f` can be any function of two arguments which has already been defined for expressions with head `Diagram` in the following way:
```mathematica
f[diagram1_Diagram, diagram2_Diagram] := expr
```

### Libraries

Diagrams and relations often used in computations can be conveniently stored in a file and are loaded with the following routines.

The *library* is a dictionary whose entries can be looked up with `Retrieve["name of item"]`. By default, the library is empty. `LoadLibrary` can be used to load the contents of a file into the library. For example, to load the standard library *stdlib*, use
```mathematica
LoadLibrary["stdlib"]
```
and use
```mathematica
Retrieve["C4Atoms"]
```
to obtain the basic diagrams in C4.

#### ClearLibrary

`ClearLibrary[]` deletes all entries from the library.

#### Description

`Description[key]` gives the description of `key` in the library.

#### LoadLibrary

`LoadLibrary[libname]` adds the contents of the file `<libname>.m` to the library.

`libname` must be a string.

#### Retrieve

`Retrieve[key]` gives the value of `key` in the current library.

Options can be specified in the form `Retrieve[key, opts]` and are applied to the result of the library search. `dimC4` has default value 4.

## Acknowledgements

I would like to thank Tobias Osborne for introducing me to the topic, and Markus Duwe and Ramona Wolf for many helpful comments and for testing the package.

## License

The package is available under the terms of the MIT license. See the LICENSE file for details.

Copyright (c) 2018 Deniz Stiegemann
