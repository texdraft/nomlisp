#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/stream
                    racket/control
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Expansion}

Nomlisp code undergoes a Racket-like expansion process
before elaboration and execution, where all macros are
expanded by running their code in a higher phase level. Some
(not all) syntactic sugar is also eliminated during
expansion.

@chunk[<Expander>
       (module Expander racket
         (provide <provisions/expander>)
         (require <requirements/expander>)
         <expander>)]

@section{Overview}

Expansion has two main tasks. The first is, of course, to
expand macros. Because Nomlisp supports local macro
definitions, the expander keeps track of lexical scope
(usually recording merely that a name has some binding as a
variable or type); hence a good amount of initial
elaboration and desugaring can be done during expansion. The
second task of expansion is to remove all “declaratives”
from the source program, turning them into @code{let},
@code{letrec}, etc. In doing so, the module-structured
surface program becomes a giant nest of binding forms.

Here is an example of the expander's dirty work:
@verbatim{
(define/module Library
  (export do-it SomeSum)
  (define/data (Threeither a b c)
    (Left a)
    (Middle b)
    (Right c))

  (define (do-it ()) (→ Unit Unit)
    (print "hi")))

(define/module Main
  (define/module Submodule
    (export run)
    (define run (→ Unit Unit)
      (let ()
        (import Library)
        do-it)))

  (define (main ()) (→ Unit Unit)
    (import Submodule)
    (run ())))
}
becomes something like
@verbatim{
(letrec ((module_Library %Module
                         (let/types/data ((data (Threeither a b c)
                                            (Left a)
                                            (Middle b)
                                            (Right c)))
                           (let ((do-it (→ Unit Unit)
                                        (λ (())
                                          (print "hi"))))
                             (%capture-environment)))
         (module_Main %Module
                      (let ((module_Submodule %Module
                                              (let ((run (→ Unit Unit)
                                                         (let ()
                                                           (external module_Library do-it))))
                                                (%capture-environment)))
                            (main (→ Unit Unit)
                                  (λ (())
                                    ((external module_Submodule run) ()))))
                        (%capture-environment))))
  ((external module_Main main) ()))
}

Most of the time, expansion is a relation between
s-expressions (i.e., trees of conses and symbols), but
occasionally AST objects are part of the expansion. (Only
s-expressions are converted to Nomlisp syntax objects to be
processed by macros, however.) Constructors in patterns are
turned into @racket[ConstructorPattern] trees, to take care
of a context-sensitive aspect of parsing the language. The
module system is also dealt with during expansion;
@code{import} declaratives get removed and references to
imported names become @racket[ExternalVariable] or
@racket[ExternalType] or @racket[ExternalConstructor].

By the end of expansion, we have the following guarantees
about the transformed source program:

@itemlist[
 @item{There are no macro calls or uses of the operators
  @code{define/syntax}, @code{define/type-syntax},
  @code{define/pattern-syntax}, @code{for-syntax}, or @code{import}.}
 @item{All bare symbols refer to lexically apparent bindings
  (unbound variables, etc., are caught).}
 @item{References to imported bindings become @code{External…} trees}
 @item{Bodies are rewritten in terms of @code{letrec} and
  @code{letrec/types/data}}]

At the heart of the expander are the functions
@racket[expand/term], @racket[expand/type],
@racket[expand/body], and @racket[expand/pattern], which
handle expansion for each syntactic context found in
Nomlisp.

The very last step is to convert the (primarily)
s-expression source code into a tree to be elaborated.

@subsection{Expansion and evaluation}

Nomlisp macros are implemented by Nomlisp functions. Thus
expanding a macro necessitates running some Nomlisp code,
even though expansion is supposed to happen before the
program runs. In fact expansion @emph{does} happen before
the program runs, and outside of external side-effects, the
evaluation of the program is entirely independent of any
evaluation that happens during expansion. Therefore all
macro-related stuff in the input can be fully erased from
the source program before runtime.

How does this work? We adopt Racket's notion of phases,
albeit in a slightly more restricted form. Code that will be
executed at runtime is said to be at phase 0. Only code at
higher phase levels will be executed at expansion time.

This implementation uses an infinite tower of environments
to deal with phase levels, inspired by this Gist:
https://gist.github.com/pqwy/5350651.

@chunk[<expander>
       (define (make-tower first-environment)
         (let build ()
           (stream-cons first-environment (build))))
       (define (tower-cons environment tower)
         (stream-cons environment (tower-rest tower)))
       (define (tower-transform f tower)
         (tower-cons (f (tower-first tower))
                     tower))
       (define (tower-first tower)
         (stream-first tower))
       (define (tower-rest tower)
         (stream-rest tower))
       (define (evaluate-meta expression tower)
         (evaluate expression (tower-first (tower-rest tower))))]

@chunk[<requirements/expander>
       (submod "environment.scrbl" Environment)
       (submod "evaluate.scrbl" Evaluation)]

@section{Expanding a body}

Let's get the hard part over with first: expanding internal
definition contexts, AKA bodies. Most of the complexity of
the expander lies in this part, because all declaratives
occur in bodies.

We make two passes over the body. Initially each subform is
“tentatively expanded”, to figure out what kind of form it
is (declarative or not). For example, consider
@codeblock{
(begin (define/syntax my-def …) ; imagine it expands to define
       (define/syntax my-mac …) ; expands to whatever
       (my-def a _ (my-mac …))
       (my-def b _ (my-mac …))
       …)
}
Tentative expansion peels off a single layer of macros, producing
@codeblock{
(begin (define a _ (my-mac …))
       (define b _ (my-mac …))
       …)
}
You can see that the macro transformers were installed and
the @code{define/syntax} forms were deleted. We can now go
in and add dummy bindings for all names defined in the body,
so as to achieve the @code{letrec} effect.

Then the body subforms are fully expanded, eliminating any
macro calls. The macro environment active at the point of
encountering each form is saved along with the form in the
first pass, so that the proper context is captured for full
expansion.

If a @code{for-syntax} form appears, its subforms are
expanded at the next phase level, but not yet evaluated.
They are then classified as all declarative or all
executable (or else in error). A series of declarative
@code{for-syntax} forms, possibly interrupted by any
declaratives besides @code{define/syntax},
@code{define/type-syntax}, or @code{define/pattern-syntax},
get merged as if there had been a single @code{for-syntax}.
Executable @code{for-syntax} expressions are executed
immediately in the next phase level. As soon as a
@code{define/syntax}, etc., declarative is encountered, the
@code{for-syntax} declaratives are “committed”.

The fully-expanded body is partitioned into type and
term-level definitions, which then get collected as the
final expansion is prepared.

@chunk[<expander>
       (define (expand/head form namespace tower)
         (define environment0 (tower-first tower))
         (match form
           [`(,(? symbol? operator) ,operands ...)
            (lookup namespace operator environment0
                    (λ (binding)
                      (expand/head ((Binding-value binding) form) namespace tower))
                    (λ ()
                      form))]
           [(? symbol?)
            (lookup 'macro form environment0
                    (λ (binding)
                      (expand/head ((Binding-value binding) form) namespace tower))
                    (λ ()
                      form))]
           [_
            form]))]

@chunk[<expander>
       (define (rewrite-declaratives body tower expression)
         body)
       (define (expand/body body tower)
         body)]

@chunk[<expander>
       (define (declarative? expression)
         (match expression
           [(or `(define _ ...)
                `(define/type _ ...)
                `(define/new-type _ ...)
                `(define/class _ ...)
                `(define/instance _ ...)
                `(define/module _ ...)
                `(declare _ ...)
                `(import _ ...))
            #t]
           [`(begin ,(? declarative?) ...)
            #t]
           [_
            #f]))]

@section{Expanding a module}

The expansion of a module is a pair of name/type/value
triples ready to be spliced into a @code{let}-like binding
form. The first triple represents only the type namespace
bindings from the module; the second triple contains all
other bindings. For example,
@verbatim{
(define/module Foo
  (define/type T1 …)
  (define/type T2 …)
  (define/type T3 …)
  (define v1 …)
  (define v2 …)
  (define v3 …))
}
results in the two values
@verbatim{
(module_Foo_types %Module (let/types ((T1 …) (T2 …) (T3 …))
                            (%capture-environment)))
;
(module_Foo %Module (let ((v1 …) (v2 …) (v3 …))
                      (%capture-environment)))
}
except that @code{module_Foo_types} and @code{module_Foo}
are actually freshly-generated unique names.

@chunk[<expander>
       (define (expand/module name body tower)
         (let ([expanded-body (expand/body body tower)])
           expanded-body))]

@section{Expanding terms}

@chunk[<expander>
       (define (expand/term term tower)
         (define (go x) (expand/term x tower))
         (match (expand/head term 'macro tower)
           [`(λ (,patterns ...) . ,body)
            (define patterns (map (λ (pattern)
                                    (expand/pattern pattern tower))
                                  patterns))
            `(λ ,patterns
               ,(expand/body body (tower-transform (λ (environment)
                                                     (augment-environment-with-pattern-variables environment `(tuple ,@patterns)))
                                                   tower)))]
           [`(delimit ,expression)
            `(delimit ,(go expression))]
           [`(capture ,name ,expression)
            `(capture ,name ,(expand/term expression (tower-transform (λ (environment)
                                                                        (extend-environment environment 'variable name #t))
                                                                      tower)))]
           [`(the ,expression ,type)
            `(the ,(go expression) ,(expand/type type tower))]
           [`(begin ,body ...)
            (expand/body body tower)]
           <expand/term/let>
           <expand/term/letrec>
           <expand/term/case>
           [`(,subexpressions ...)
            (map go subexpressions)]
           [x
            x]))]

@chunk[<expand/term/let>
       [`(let (,bindings) . ,body)
        (define outer-environment (tower-first tower))
        (let loop ([bindings bindings]
                   [expanded-bindings (list)]
                   [inner-environment outer-environment])
          (if (null? bindings)
              `(let ,(reverse expanded-bindings)
                 ,(expand/body body (tower-cons inner-environment tower)))
              <expand/term/binding-form-pass>))]]

@chunk[<expand/term/letrec>
       [`(letrec (,bindings) . ,body)
        (define outer-environment (tower-first tower))
        (let loop ([bindings bindings]
                   [expanded-bindings (list)]
                   [inner-environment outer-environment])
          (if (null? bindings)
              (let ([new-tower (tower-cons inner-environment tower)])
                `(letrec ,(reverse (map (λ (b)
                                          (match b
                                            [`(,name ,type ,value)
                                             `(,name ,type ,(expand/term value new-tower))]))
                                        expanded-bindings))
                   ,(expand/body body new-tower)))
              <expand/term/binding-form-pass>))]]

@chunk[<expand/term/binding-form-pass>
       (let ([binding (first bindings)])
         (match binding
           [`(,variable ,_ ...)
            (loop (rest bindings)
                  (cons (match (first bindings)
                          [`(,_ ,type ,value)
                           `(,variable ,(expand/type type tower)
                                       ,value)]
                          [`(,_ ,value)
                           `(,variable _ ,value)])
                        expanded-bindings)
                  (extend-environment inner-environment 'variables variable #t))]))]

The helper function for binding pattern variables will be
defined as part of pattern expansion.

@chunk[<expand/term/case>
       [`(case ,scrutinee . ,cases)
        `(case ,(go scrutinee)
           ,(map (λ (case)
                   (match case
                     [`(,pattern ,expression)
                      (define pattern (expand/pattern pattern tower))
                      `(,pattern ,(expand/term expression (tower-transform (λ (environment)
                                                                             (augment-environment-with-pattern-variables environment pattern))
                                                                           tower)))]))
                 cases))]]


@section{Expanding types}

Types do not have the complexities of internal definition
contexts or local macros (they have to be bound in an
enclosing term), so we can just expand eagerly.

@chunk[<expander>
       (define (expand/type type tower)
         (define environment0 (tower-first tower))
         (define (go x) (expand/type x tower))
         (match (expand/head type 'type-macro tower)
           [`(,operator ,operands ...)
            (foldl (λ (operand operator)
                     `(,operator ,(go operand)))
                   (go operator)
                   operands)]
           [_
            type]))]

@section{Expanding patterns}

Patterns are also much simpler than terms. We just have to
account for constructor bindings.

@chunk[<expander>
       (define (constructor-name? name environment)
         (lookup name 'variables environment
                 (λ (binding)
                   #t)))

       (define (expand/pattern pattern tower)
         (define (go x) (expand/pattern x tower))
         (match (expand/head pattern 'pattern-macro tower)
           [`(,(and constructor (? (λ (x) (constructor-name? x (tower-first tower))))) ,patterns ...)
            `(,constructor ,@(map go patterns))]
           [_
            pattern]))]

@chunk[<expander>
       (define (augment-environment-with-pattern-variables environment pattern)
         (define (collect-pattern-variables pattern)
           (match pattern
             [(? (λ (x) (constructor-name? x environment)))
              '()]
             [(? symbol?)
              (list pattern)]
             [`(,_ ,patterns ...)
              (foldl (λ (pattern variables)
                       (append (collect-pattern-variables pattern) variables))
                     '()
                     patterns)]
             [_
              '()]))
         (let loop ([environment environment]
                    [variables (collect-pattern-variables pattern)])
           (if (null? variables)
               environment
               (loop (extend-environment environment 'variable (first variables) #t)
                     (rest variables)))))]

@section{Expanding the program}

To the Nomlisp system, a program initially presents itself
as a single module, which should define a function named
@code{main} somewhere. Here is how the whole expansion
process starts. The main module is given to
@racket[expand/program] in the form of a list of
s-expressions.

@chunk[<expander>
       (define (expand/program main-module)
         main-module)]

@chunk[<provisions/expander>
       expand/program]

@section{Parsing}

But alas we aren't done. The s-expression expansion now has
to be fully converted into a tree.