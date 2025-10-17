#lang racket

(require "environments.rkt")

(provide elaborate
         elaborate/body
         elaborate/transformer?
         empty-elaboration-context)

(define (empty-elaboration-context)
  (Syntactic-Environment (hash) (hash) (hash) (hash)))

(define (elaborate x . r)
  (displayln "i'm elaborate"))

(define (elaborate/body . r)
  (displayln "i'm elabody"))

(define (elaborate/transformer? . r)
  (displayln "i'm elatransform"))