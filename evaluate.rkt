#lang racket

(provide evaluate
         evaluate/module
         evaluate/body
         empty-runtime-environment)

(struct Environment
  (values))

(define (empty-runtime-environment)
  (Environment '()))

(define (evaluate expression environment)
  (displayln "i'm evaluating????"))

(define (evaluate/module)
  (displayln "i'm evaluating modue????"))

(define (evaluate/body)
  (displayln "i'm evaluating body????"))