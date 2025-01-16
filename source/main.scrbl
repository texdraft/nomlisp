#lang scribble/lp2/manual

@title{Nomlisp—unfinished!!}

@author{Asher Olsen}

This is (well, @emph{will be}) the literate source code for
the Nomlisp bootstrapping interpreter.

@table-of-contents[]

@include-section[(submod "data.scrbl" doc)]
@include-section[(submod "syntax.scrbl" doc)]
@include-section[(submod "environment.scrbl" doc)]
@include-section[(submod "expand.scrbl" doc)]
@include-section[(submod "elaborate.scrbl" doc)]
@include-section[(submod "evaluate.scrbl" doc)]

@section{The main program}

@chunk[<*>
       (require (submod "evaluate.scrbl" Evaluation)
                (submod "syntax.scrbl" Syntax)
                (submod "environment.scrbl" Environment)
                (submod "elaborate.scrbl" Elaborate))
       (define (top-level)
         (print "I can't wait until I can actually run code."))
       (top-level)]
