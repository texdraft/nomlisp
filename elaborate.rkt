#lang racket

(provide elaborate
         elaborate/body
         elaborate/transformer)

(define (elaborate x . r)
  (displayln "i'm elaborate"))

(define (elaborate/body . r)
  (displayln "i'm elabody"))

(define (elaborate/transformer . r)
  (displayln "i'm elatransform"))