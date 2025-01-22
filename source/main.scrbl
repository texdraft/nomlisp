#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/stream
                    racket/control
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

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
       (module main racket
         (require (submod "evaluate.scrbl" Evaluation)
                  (submod "syntax.scrbl" Syntax)
                  (submod "environment.scrbl" Environment)
                  (submod "elaborate.scrbl" Elaborate))
         (define (top-level)
           (displayln (show (read (open-input-string "(define/module Main (define (factorial n) (→ Integer Integer) (if (=? n 0) 1 (× n (factorial (- n 1))))))"))))
           (displayln (show (read (open-input-string "#xxx⟨x y z⟩")))))
         (top-level))]