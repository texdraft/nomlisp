#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/control
                    racket/match
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Evaluation}

Now we are finally ready to run some code! This
bootstrapping evaluator is a conventional environment-based
big-step tree-walker that leans on Racket for continuation
support and closures.

@chunk[<Evaluation>
       (module Evaluation racket
         (provide <provisions/evaluation>)
         (require <requirements/evaluation>)
         <evaluation>)]

@chunk[<requirements/evaluation>
       (submod "syntax.scrbl" Syntax)
       (submod "environment.scrbl" Environment)
       (submod "data.scrbl" Data)
       racket/control]

We use the massive environments from before even though only
variables matter at runtime.

@chunk[<evaluation>
       (define (evaluate expression environment)
         (match expression
           [(Variable name)
            (lookup 'variable name environment
                    (λ (binding)
                      (Binding-value binding)))]
           [(Constant value)
            value]
           [(FixedLambda parameter body)
            (λ (argument)
              (evaluate body (extend-environment environment 'variable parameter argument)))]
           [(Application function argument)
            ((evaluate function environment) (evaluate argument environment))]
           [(Delimit expression)
            (reset (evaluate expression environment))]
           [(Capture name expression)
            (shift k (evaluate expression (extend-environment environment 'variable name k)))]
           [(Sequence expressions)
            (let loop ([expressions expressions])
              (if (null? (rest expressions))
                  (evaluate (first expressions) environment)
                  (begin (evaluate (first expressions) environment)
                         (loop (rest expressions)))))]
           [(Tuple components)
            (tuple (map (λ (c) (evaluate c environment)) components))]
           <evaluate/external>
           <evaluate/case>
           <evaluate/let>
           <evaluate/letrec>))]

External variables are initialized lazily so that modules
can be mutually recursive. We rely on Racket to thwart
attempts at using a variable during its initialization,
since forcing a currently-forcing promise raises an
exception.

@chunk[<evaluate/external>
       [(ExternalVariable module-variable name)
        (lookup 'variable module-variable environment
                (λ (environment)
                  (lookup 'variable name environment
                          (λ (binding)
                            (match binding
                              [(Binding _ promise)
                               (force promise)])))))]]

@section{Pattern matching}

The only patterns left after elaboration are tuples,
constants, and variables.

@chunk[<evaluate/case>
       [(Case scrutinee cases)
        (let ([scrutinee (evaluate scrutinee environment)])
          (let loop ([cases cases])
            (match (first cases)
              [`(,pattern ,expression)
               (match-then-or pattern
                              scrutinee
                              (λ (environment)
                                (evaluate expression environment))
                              (λ ()
                                (loop (rest cases)))
                              environment)])))]]

@chunk[<evaluation>
       (define (match-then-or pattern scrutinee yes no environment)
         (match pattern
           [(TuplePattern patterns)
            (let loop ([patterns patterns]
                       [component 0]
                       [environment environment])
              (if (null? patterns)
                  (yes environment)
                  (match-then-or (first patterns)
                                 (tuple-ref scrutinee component)
                                 (λ (environment)
                                   (loop (rest patterns)
                                         (+ component 1)
                                         environment))
                                 no
                                 environment)))]
           [(VariablePattern name)
            (yes (extend-environment environment 'variable name scrutinee))]
           [(ConstantPattern value)
            (if (equal? value scrutinee)
                (yes environment)
                (no))]))]

@section{The @code{let} family}

@chunk[<evaluate/let>
       [(Let bindings body)
        (define old-environment environment)
        (let loop ([bindings bindings]
                   [environment environment])
          (if (null? bindings)
              (evaluate body environment)
              (loop (rest bindings)
                    (match (first bindings)
                      [`(,name ,value)
                       (extend-environment environment 'variable name (evaluate value old-environment))]))))]]

First, we go through the bindings and add the names to the
environment, bound to placeholder values. Then we evaluate
the values in this environment, updating the inital bindings
as we go.

@chunk[<evaluate/letrec>
       [(LetRec bindings body)
        (let loop ([original-bindings bindings]
                   [environment environment]
                   [Bindings (list)]
                   [expressions (list)])
          (if (null? original-bindings)
              (begin (map (λ (binding expression)
                            (set-Binding-value! binding (evaluate expression environment)))
                          Bindings
                          expressions))
              (match (first original-bindings)
                [`(,name ,value)
                 (define binding (Binding name undefined))
                 (loop (rest original-bindings)
                       (extend-environment/binding environment 'variable binding)
                       (cons Binding Bindings)
                       (cons value expressions))])))]]

@chunk[<provisions/evaluation>
       evaluate]