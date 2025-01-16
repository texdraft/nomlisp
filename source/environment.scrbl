#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/control
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Environments}

Throughout the Nomlisp system, we use environments to keep
track of name bindings. An environment is simply a mapping
from Nomlisp identifiers to host objects, with the added
complications of multiple namespaces and nested lexical
scope.

@chunk[<Environment>
       (module Environment racket
         (provide <provisions/environment>)
         <environment>)]

Environments hold bindings, which are like half-mutable
conses. (Nomlisp has no mutable variables, but to implement
@code{letrec} it is easiest to introduce empty bindings and
fix them up later.)

The @racket[name] field of a @racket[Binding] often holds a
string, but it need not. Names are compared by
@racket[equal?].

@chunk[<environment>
       (struct Binding
         (name
          (value #:mutable)))]

@chunk[<environment>
       (struct Environment
         (variables
          types
          macros
          type-macros
          pattern-macros
          sugar
          type-sugar
          pattern-sugar
          modules
          parent))]

@chunk[<provisions/environment>
       (struct-out Environment)
       (struct-out Binding)]

Of course, we don't want to have to remember the order of
all those fields, so here's a by-name constructor.

@chunk[<environment>
       (define (make-environment  #:variables [variables (list)]
                                  #:types [types (list)]
                                  #:macros [macros (list)]
                                  #:type-macros [type-macros (list)]
                                  #:pattern-macros [pattern-macros (list)]
                                  #:sugar [sugar (list)]
                                  #:type-sugar [type-sugar (list)]
                                  #:pattern-sugar [pattern-sugar (list)]
                                  #:modules [modules (list)]
                                  #:parent [parent #f])
         (Environment variables
                      types
                      macros
                      type-macros
                      pattern-macros
                      sugar
                      type-sugar
                      pattern-sugar
                      modules
                      parent))]

Normally only one namespace grows at a time; the usual way
to add a binding is by calling @racket[extend-environment].

@chunk[<environment>
       (define (extend-environment/binding environment namespace binding)
         (match environment
           [(Environment variables
                         types
                         macros
                         type-macros
                         pattern-macros
                         sugar
                         type-sugar
                         pattern-sugar
                         modules
                         parent)
            (define (maybe-augment bindings binding-namespace)
              (if (equal? binding-namespace namespace)
                  (cons binding bindings)
                  bindings))
            (Environment (maybe-augment variables 'variable)
                         (maybe-augment types 'type)
                         (maybe-augment macros 'macro)
                         (maybe-augment type-macros 'type-macro)
                         (maybe-augment pattern-macros 'pattern-macros)
                         (maybe-augment sugar 'sugar)
                         (maybe-augment type-sugar 'type-sugar)
                         (maybe-augment pattern-sugar 'pattern-sugar)
                         (maybe-augment modules 'module)
                         parent)]))

       (define (extend-environment environment namespace name meaning)
         (extend-environment/binding environment namespace (Binding name meaning)))]

@chunk[<environment>
       (define (lookup namespace name environment [bound identity] [unbound (const #f)])
         (let* ([bindings (case namespace
                            [(variable)
                             (Environment-variables environment)]
                            [(type)
                             (Environment-types environment)]
                            [(macro)
                             (Environment-macros environment)]
                            [(type-macro)
                             (Environment-type-macros environment)]
                            [(pattern-macro)
                             (Environment-pattern-macros environment)]
                            [(sugar)
                             (Environment-sugar environment)]
                            [(type-sugar)
                             (Environment-type-sugar environment)]
                            [(pattern-sugar)
                             (Environment-pattern-sugar environment)]
                            [(module)
                             (Environment-modules environment)])]
                [result (findf (λ (binding)
                                 (equal? (Binding-name binding)
                                         name))
                               bindings)])
           (if result
               (bound result)
               (unbound))))]

@chunk[<provisions/environment>
       make-environment
       extend-environment
       extend-environment/binding
       lookup]