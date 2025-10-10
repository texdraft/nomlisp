#lang racket

(require "reader.rkt"
         "expander.rkt")

(print (expand/program (call-with-input-file "test.nom" parse-file)
                       (empty-tower)))

`(List '(($ (Identifier "define/module"))
         ($ (Identifier "Test"))
         ($ (List '(($ (Identifier "a"))
                    ($ (Identifier "b"))
                    ($ (Identifier "c")))))
         ($ (List '(($ (Identifier "define/module"))
                    ($ (Identifier "Local"))
                    ($ (List '($ (List '($ (Identifier "syntax")
                                           ($ (Identifier "m")))))))
                    ($ (List '(($ (Identifier "define/syntax"))
                               ($ (Identifier "m"))
                               ($ (List '(($ (Identifier "λ"))
                                          ($ (List '($(Identifier "x"))))
                                          ($ (Identifier "x")))))))))))
         ($ (List '(($ (Dotted ($ (Identifier "Local"))
                               ($ (Identifier "m"))))
                    ($ (Identifier "Hello")))))))