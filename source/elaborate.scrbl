#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/match
                    racket/control
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Elaboration}

The goal of elaboration is to turn a fully-expanded, parsed
program into a simpler program in an untyped core language.

@chunk[<Elaborate>
       (module Elaborate racket
         (provide <provisions/elaborate>)
         (require <requirements/elaborate>)
         <elaborate>)]

@section{Types}

Nomlisp uses a bidirectional type checking algorithm in
order to handle variadic functions, which require
type-directed treatment. As such, the process of checking is
split into two operations, one verifying whether a term is
of a specified type, the other synthesizing a type from a
term.

Here is some (Racket-level) sugar for expressing Nomlisp
types.

@chunk[<elaborate>
       (define arrow-constructor (TypeName "→"))
       (define varrow-constructor (TypeName "→*"))
       (define unit-type (TypeName "Unit"))

       (define-match-expander →
         (syntax-rules ()
           [(_ T1 T2)
            (TypeApplication (TypeApplication (== arrow-constructor) T1) T2)])
         (syntax-rules ()
           [(_ T1 T2)
            (TypeApplication arrow-constructor T1 T2)]))

       (define-match-expander →*
         (syntax-rules ()
           [(_ T1 T2)
            (TypeApplication (TypeApplication (== varrow-constructor) T1) T2)])
         (syntax-rules ()
           [(_ T1 T2)
            (TypeApplication varrow-constructor T1 T2)]))]

@chunk[<requirements/elaborate>
       (submod "syntax.scrbl" Syntax)]

We use environments to keep track of typing information.

@chunk[<requirements/elaborate>
       (submod "environment.scrbl" Environment)]

@chunk[<elaborate>
       (define (unify! t1 t2)
         (void))]

@section{Checking types}

@chunk[<elaborate>
       (define (elaborate/check expression context type)
         (match expression
           (code:comment @#,elem{fall back to inference})
           [_
            (define-values [elaborated inferred] (elaborate/infer expression context))
            (unify! inferred type)
            (values elaborated type)]))]

@section{Synthesizing types}

@chunk[<elaborate>
       (define (elaborate/infer expression context)
         (match expression
           [(The expression type)
            (elaborate/check expression type context)]
           [(Variable name)
            (values expression
                    (lookup 'variable name context
                            (λ (binding)
                              (Binding-value binding))
                            (λ ()
                              (error "unbound variable post-expansion >.<"))))]))]

@section{Elaborating the program}

After expansion, a program is just a @code{let} or
@code{letrec} expression that performs an application of
@code{main}. Hence the final result of a program is
(supposed to be) @code{Unit}.

@chunk[<elaborate>
       (define (elaborate-program program context)
         (values (elaborate/check program context unit-type)))]

@chunk[<provisions/elaborate>
       elaborate-program]