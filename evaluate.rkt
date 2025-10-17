#lang racket

(provide evaluate
         evaluate/module
         evaluate/body
         evaluate/declaration
         empty-runtime-environment
         runtime-environment-from-association-list)

(struct Environment
  (values))

(define (empty-runtime-environment)
  (Environment (make-hash)))

(define (runtime-environment-from-association-list l)
  (Environment (make-hash l)))

(define (evaluate expression environment)
  (displayln "i'm evaluating????"))

(define (evaluate/declaration declaration environment)
  (displayln "i'm evaluating declaration????"))

(define (evaluate/module)
  (displayln "i'm evaluating modue????"))

(define (evaluate/body)
  (displayln "i'm evaluating body????"))