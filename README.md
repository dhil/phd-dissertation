# Foundations for programming and implementing effect handlers

A copy of my dissertation can be [downloaded via my
website](https://dhil.net/research/papers/thesis.pdf).

----

Submitted on May 30, 2021. Examined on August 13, 2021.

The board of examiners were

* [Andrew Kennedy](https://github.com/andrewjkennedy) (Facebook London)
* [Edwin Brady](https://www.type-driven.org.uk/edwinb/) (University of St Andrews)
* [Ohad Kammar](http://denotational.co.uk/) (The University of Edinburgh)
* [Stephen Gilmore](https://homepages.inf.ed.ac.uk/stg/) (The University of Edinburgh)

## Thesis structure

The dissertation is structured as follows.

### Introduction

 * Chapter 1 puts forth an argument for why effect handlers
   matter. Following this argument it provides a basic introduction to
   several different approaches to effectful programming through the
   lens of the state effect. In addition, it also declares the scope
   and contributions of the dissertation, and discusses some related
   work.

### Programming

 * Chapter 2 illustrates effect handler oriented programming by
   example by implementing a small operating system dubbed Tiny UNIX,
   which captures some essential traits of Ritchie and Thompson's
   UNIX. The implementation starts with a basic notion of file i/o,
   and then, it evolves into a feature-rich operating system with full
   file i/o, multiple user environments, multi-tasking, and more, by
   composing ever more effect handlers.

 * Chapter 3 introduces a polymorphic fine-grain call-by-value core
   calculus, λ<sub>b</sub>, which makes key use of Rémy-style row
   polymorphism to implement polymorphic variants, structural records,
   and a structural effect system. The calculus distils the essence of
   the core of the Links programming language. The chapter also
   presents three extensions of λ<sub>b</sub>, which are λ<sub>h</sub>
   that adds deep handlers, λ<sup>†</sup> that adds shallow handlers,
   and λ<sup>‡</sup> that adds parameterised handlers.

### Implementation

 * Chapter 4 develops a higher-order continuation passing style
   translation for effect handlers through a series of step-wise
   refinements of an initial standard continuation passing style
   translation for λ<sub>b</sub>. Each refinement slightly modifies
   the notion of continuation employed by the translation. The
   development ultimately leads to the key invention of generalised
   continuation, which is used to give a continuation passing style
   semantics to deep, shallow, and parameterised handlers.

 * Chapter 5 demonstrates an application of generalised continuations
   to abstract machine as we plug generalised continuations into
   Felleisen and Friedman's CEK machine to obtain an adequate abstract
   runtime with simultaneous support for deep, shallow, and
   parameterised handlers.

### Expressiveness

 * Chapter 6 shows that deep, shallow, and parameterised notions of
   handlers can simulate one another up to specific notions of
   administrative reduction.

 * Chapter 7 studies the fundamental efficiency of effect handlers. In
   this chapter, we show that effect handlers enable an asymptotic
   improvement in runtime complexity for a certain class of
   functions. Specifically, we consider the *generic count* problem
   using a pure PCF-like base language λ<sub>b</sub><sup>→</sup> (a
   simply typed variation of λ<sub>b</sub>) and its extension with
   effect handlers λ<sub>h</sub><sup>→</sup>.  We show that
   λ<sub>h</sub><sup>→</sup> admits an asymptotically more efficient
   implementation of generic count than any λ<sub>b</sub><sup>→</sup>
   implementation.

### Conclusions
  * Chapter 8 concludes and discusses future work.

### Appendices

 * Appendix A contains a literature survey of continuations and
   first-class control. I classify continuations according to their
   operational behaviour and provide an overview of the various
   first-class sequential control operators that appear in the
   literature. The application spectrum of continuations is discussed
   as well as implementation strategies for first-class control.
 * Appendix B contains a proof that shows the `Get-get` equation for
   state is redundant.
 * Appendix C contains the proof details and gadgetry for the
   complexity of the effectful generic count program.
 * Appendix D provides a sample implementation of the Berger count
   program and discusses it in more detail.

## Building

To build the dissertation you need the [Informatics thesis LaTeX
class](https://github.com/dhil/inf-thesis-latex-cls) with the
University of Edinburgh crests. Invoking `make` on the command line
ought to produce a PDF copy of the dissertation named `thesis.pdf`,
e.g.

```shell
$ make
```

## Timeline

I submitted my thesis on May 30, 2021. It was examined on August 13,
2021, where I received pass with minor corrections. The revised thesis
was submitted on December 22, 2021. It was approved on March
14, 2022. The final revision was submitted on March 23, 2022. I
received my PhD award letter on March 24, 2022.
