# Foundations for programming and implementing effect handlers

**NOTE** I have made a draft copy of the dissertation available in
this repository. I ask that you **do not** link to or distribute the
draft anywhere, because I will delete the file once the final revision has
been submitted after the viva.

---

Submitted May 30, 2021. Pending examination.

The board of examiners consists of

* [Andrew Kennedy](https://github.com/andrewjkennedy) (Facebook London)
* [Edwin Brady](https://www.type-driven.org.uk/edwinb/) (University of St Andrews)
* [Ohad Kammar](http://denotational.co.uk/) (The University of Edinburgh)
* [Stephen Gilmore](https://homepages.inf.ed.ac.uk/stg/) (The University of Edinburgh)

## Thesis structure

The dissertation is structured as follows.

### Background

 * Chapter 2 defines some basic mathematical notation and
constructions that are they pervasively throughout this dissertation.

 * Chapter 3 presents a literature survey of continuations and
first-class control. I classify continuations according to their
operational behaviour and provide an overview of the various
first-class sequential control operators that appear in the
literature. The application spectrum of continuations is discussed as
well as implementation strategies for first-class control.

### Programming

 * Chapter 4 introduces a polymorphic fine-grain call-by-value core
calculus, λ<sub>b</sub>, which makes key use of Remy-style row polymorphism
to implement polymorphic variants, structural records, and a
structural effect system. The calculus distils the essence of the core
of the Links programming language.

 * Chapter 5 presents three extensions of λ<sub>b</sub>,
which are λ<sub>h</sub> that adds deep handlers, λ<sup>†</sup> that adds shallow
handlers, and λ<sup>‡</sup> that adds parameterised handlers. The chapter
also contains a running case study that demonstrates effect handler
oriented programming in practice by implementing a small operating
system dubbed Tiny UNIX based on Ritchie and Thompson's original
UNIX.

### Implementation

 * Chapter 6 develops a higher-order continuation passing
style translation for effect handlers through a series of step-wise
refinements of an initial standard continuation passing style
translation for λ<sub>b</sub>. Each refinement slightly modifies the notion
of continuation employed by the translation. The development
ultimately leads to the key invention of generalised continuation,
which is used to give a continuation passing style semantics to deep,
shallow, and parameterised handlers.

 * Chapter 7 demonstrates an application of generalised continuations
to abstract machine as we plug generalised continuations into
Felleisen and Friedman's CEK machine to obtain an adequate abstract
runtime with simultaneous support for deep, shallow, and parameterised
handlers.

### Expressiveness
 * Chapter 8 shows that deep, shallow, and parameterised notions of
handlers can simulate one another up to specific notions of
administrative reduction.

 * Chapter 9 studies the fundamental efficiency of effect handlers. In
this chapter, we show that effect handlers enable an asymptotic
improvement in runtime complexity for a certain class of
functions. Specifically, we consider the *generic count* problem using
a pure PCF-like base language λ<sub>b</sub><sup>→</sup> (a simply typed variation of
λ<sub>b</sub>) and its extension with effect handlers λ<sub>h</sub><sup>→</sup>.  We
show that λ<sub>h</sub><sup>→</sup> admits an asymptotically more efficient
implementation of generic count than any λ<sub>b</sub><sup>→</sup> implementation.

### Conclusions
  * Chapter 10 concludes and discusses future work.

## Building

To build the dissertation you need the [Informatics thesis LaTeX
class](https://github.com/dhil/inf-thesis-latex-cls) with the
University of Edinburgh crests. Invoking `make` on the command line
ought to produce a PDF copy of the dissertation named `thesis.pdf`,
e.g.

```shell
$ make
```

