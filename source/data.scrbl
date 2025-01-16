#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/control
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Nomlisp object representation}

@chunk[<Data>
       (module Data racket
         (provide <provisions/data>)
         <data>)]

@section{Tuples}

Let's start with tuples, which happen to be immutable
vectors.

@chunk[<data>
       (define (make-tuple . components)
         (apply vector-immutable components))

       (define (tuple-ref tuple n)
         (vector-ref tuple n))

       (define-match-expander tuple
         (syntax-rules ()
           [(_ x ...)
            (vector x ...)])
         (syntax-rules ()
           [(_ x ...)
            (make-tuple x ...)]))]

@chunk[<provisions/data>
       tuple tuple-ref make-tuple]

@section{References}

Mutable references are mutable Racket boxes.

@chunk[<data>
       (define (make-reference value)
         (box value))

       (define (dereference value)
         (unbox value))

       (define (set-reference! reference value)
         (set-box! reference value))]

@chunk[<provisions/data>
       make-reference dereference set-reference!]

@section{Other values}

In Nomlisp, ADTs and records are social constructs.
Elaboration turns them into tuples. Everything else can be
mapped straight onto equivalent Racket values.

Unit is an empty tuple.

@chunk[<data>
       (define unit-value (tuple))]

@chunk[<provisions/data>
       unit-value]

@section{Undefined}

Lastly, we need a special placeholder value for occasional
use. In correct programs such values should never be
referred to.

@chunk[<data>
       (define undefined (list #f))

       (define (undefined? x)
         (eqv? x undefined))]

@chunk[<provisions/data>
       undefined undefined?]