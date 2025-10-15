#lang racket

(require "syntax.rkt"
         "ast.rkt"
         "evaluate.rkt"
         "elaborate.rkt")

(struct Context
  (synspaces ; current synspaces
   phase ; current phase
   environment)) ; current syntactic environment

(struct Transformer
  (procedure
   syntactic-environment)) ; environment at point of definition

(define (add-synspace context synspace)
  (struct-copy Context context
               [synspaces (cons synspace (remq synspace (Context-synspaces context)))]))

(define (remove-synspace context synspace)
  (struct-copy Context context
               [synspaces (remq synspace (Context-synspaces context))]))

(define (set-synspaces context synspaces)
  (struct-copy Context context
               [synspaces synspaces]))

