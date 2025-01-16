#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/control
                    racket/match
                    syntax/parse
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Syntax}

Here we define data structures to represent Nomlisp code.

@chunk[<Syntax>
       (module Syntax racket
         (provide <provisions/syntax>)
         (require <requirements/syntax>)
         <syntax>)]

Nomlisp has a Racket-like expansion model, so we really need
(at least) @emph{two} representations, one concrete and one
abstract. The concrete syntax may get mapped into Nomlisp
objects for macros, but the abstract syntax is only for
internal use after expansion.

@chunk[<syntax>
       <abstract>
       <concrete>]

@section{Abstract syntax}

After expansion the CST can be parsed into an AST
representing a fully-expanded program. Elaboration will
transform this AST further.

Actually, the AST is not quite as ‘A’ as it could be,
because some syntactic sugar remains. Function applications
have type-directed treatment, which must happen during type
checking.

@chunk[<abstract>
       <expression-syntax>
       <type-syntax>
       <pattern-syntax>
       <post-elaboration-syntax>]

@subsection{Type trees}

@chunk[<type-syntax>
       (struct TypeName
         (name))

       (struct ExternalType
         (module name))]

Modules end up represented as environment objects at
runtime; for type checking, we need to know the types of the
bindings in that environment.

@chunk[<type-syntax>
       (struct ModuleType
         (types))]

@chunk[<type-syntax>
       (struct TypeApplication
         (type
          argument))]

@chunk[<type-syntax>
       (struct ForAll
         (name
          type))]

@chunk[<type-syntax>
       (struct TupleType
         (types))]

@chunk[<type-syntax>
       (struct ConstrainedType
         (constraints
          type))

       (struct Constraint
         (name
          types))]

@chunk[<provisions/syntax>
       (struct-out TypeName)
       (struct-out ExternalType)
       (struct-out ModuleType)
       (struct-out TypeApplication)
       (struct-out ForAll)
       (struct-out TupleType)
       (struct-out ConstrainedType)
       (struct-out Constraint)]

@subsection{Expression trees}

@chunk[<expression-syntax>
       (struct Variable
         (name))

       (struct ExternalVariable
         (module name))]

@chunk[<expression-syntax>
       (struct Constant
         (value))

       (struct Unit
         ())]

@chunk[<expression-syntax>
       (struct Lambda
         (parameter-thing
          body))

       (struct Application
         (function
          arguments))]

@chunk[<expression-syntax>
       (struct The
         (expression
          type))]

@chunk[<expression-syntax>
       (struct Delimit
         (expression))

       (struct Capture
         (name expression))

       (struct Sequence
         (expressions))]

@chunk[<expression-syntax>
       (struct Primitive
         (name))]

@chunk[<expression-syntax>
       (struct Tuple
         (components))]

@chunk[<expression-syntax>
       (struct Case
         (scrutinee
          cases))]

@chunk[<expression-syntax>
       (struct Let
         (bindings
          body))

       (struct LetRec
         (bindings
          body))]

@chunk[<provisions/syntax>
       (struct-out Variable)
       (struct-out ExternalVariable)
       (struct-out Constant)
       (struct-out Unit)
       (struct-out Lambda)
       (struct-out Application)
       (struct-out The)
       (struct-out Delimit)
       (struct-out Capture)
       (struct-out Sequence)
       (struct-out Primitive)
       (struct-out Tuple)
       (struct-out Case)
       (struct-out Let)
       (struct-out LetRec)]

@subsection{Patterns}

@chunk[<pattern-syntax>
       (struct VariablePattern
         (name))

       (struct ConstantPattern
         (value))

       (struct TuplePattern
         (components))]

@chunk[<pattern-syntax>
       (struct ConstructorPattern
         (name
          arguments))

       (struct ExternalConstructorPattern
         (name
          arguments))

       (struct RecordPattern
         (fields))]

@chunk[<provisions/syntax>
       (struct-out VariablePattern)
       (struct-out ConstantPattern)
       (struct-out TuplePattern)
       (struct-out ConstructorPattern)
       (struct-out ExternalConstructorPattern)
       (struct-out RecordPattern)]

@subsection{Beyond the surface language}

After expansion and elaboration some new expression trees
appear.

@chunk[<post-elaboration-syntax>
       (struct FixedLambda
         (parameter
          body))

       (struct Lambdas
         (lambdas))

       (struct CaptureEnvironment
         ())

       (struct LetRec/Types/Data
         (bindings
          body))]

@chunk[<provisions/syntax>
       (struct-out FixedLambda)
       (struct-out Lambdas)
       (struct-out CaptureEnvironment)
       (struct-out LetRec/Types/Data)]

@section{Concrete syntax}

The current Nomlisp representation of its own concrete
syntax is as follows.

@codeblock{
  (define/data Syntax
    (Symbol String) ; 0
    (Label String) ; 1
    (Literal LiteralValue) ; 2
    (Cons Syntax Syntax) ; 3
    Nil) ; 4

  (define/data LiteralValue
    (Integer Integer) ; 0
    (String String)) ; 1
}

@chunk[<requirements/syntax>
       (submod "data.scrbl" Data)]

@chunk[<concrete>
       (define (label? string)
         (char=? (string-ref string 0)))

       (define (literal? thing)
         (or (integer? thing)
             (string? thing)))

       (define (syntax->nom syntax)
         (match syntax
           [(? symbol? symbol)
            (let ([name (symbol->string symbol)])
              (if (label? name)
                  (tuple 1 name)
                  (tuple 0 name)))]
           [(? literal? x)
            (tuple 2 (tuple (if (integer? x)
                                0
                                1)
                            x))]
           [`(,operator ,operands ...)
            (tuple 3 (syntax->nom operator) (syntax->nom operands))]
           ['()
            (tuple 4)]))

       (define (nom->syntax nom)
         (match nom
           [(tuple (or 0 1) name)
            (string->symbol name)]
           [(tuple 2 (tuple _ value))
            value]
           [(tuple 3 x xs)
            (cons x (nom->syntax xs))]
           [(tuple 4)
            '()]))]

@chunk[<provisions/syntax>
       syntax->nom
       nom->syntax]