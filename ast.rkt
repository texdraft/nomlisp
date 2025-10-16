#lang racket

(require racket/struct)

(provide (all-defined-out))

(define-syntax define-tree
  (syntax-rules ()
    [(_ name (fields ...))
     (struct name (fields ...)
             #:methods gen:custom-write
             [(define write-proc
                (make-constructor-style-printer
                 (lambda (s) 'name)
                 (lambda (s) (match s
                               [(name fields ...)
                                (list fields ...)]))))])]))

; Input s-expressions

(define-tree Identifier (name))
(define-tree Dotted (left right))
(define-tree Label (name))
(define-tree Constant (value))

(define-tree Left-Double-Arrow (expression))
(define-tree At (expression))

(define-tree List
  (expressions))

(define-tree Prefixed
  (prefix
   expression))

(define-tree Delimited
  (delimiters
   expression))

; Representation of fully expanded programs

(struct Export
  (external-name
   internal-name
   namespace
   syntax?))

(define-tree External (module rhs))

(define-tree Program (modules))

(define-tree Body (declarations expressions))

(define-tree Instance-Supply (expression)) ; ⇐ x
(define-tree Spread-Argument (expression)) ; @ x

(define-tree Annotated ; x : t
  (name type))

; terms

(define-tree Lambda (parameters body)) ; parameters is list of id or single id
(define-tree The (type expression))
(define-tree Delimit (prompt expression))
(define-tree Capture-To (name prompt expression))
(define-tree Make-Record (fields+values row))
(define-tree Make-Tuple (values))
(define-tree Case (expression patterns+expressions))
(define-tree Let (bindings body))
(define-tree Let-Recursive (bindings body))
(define-tree Call (operator operands))
(define-tree Quote (datum))

; types

(define-tree For-All (variables type))
(define-tree Arrow (types))
(define-tree Constrained (constraints type))
(define-tree Record-Type (fields+types))
(define-tree Tuple-Type (types))
(define-tree Type-Application (operator operand))

(define-tree Constraint (instance-variable? class))

; patterns

(define-tree As (name pattern))
(define-tree Constructor-Pattern (name patterns))
(define-tree Record-Pattern (fields+patterns))
(define-tree Tuple-Pattern (patterns))
(define-tree Where (pattern guards))

; declarations

(define-tree Define-Type (name etc. definition))
(define-tree Define-Data (name etc. constructors))
(define-tree Define-Class (name etc. items))
(define-tree Define-Module (name exports body))
(define-tree Define-Instance (name class items))
(define-tree Define (name type body))
(define-tree Declare (metadata))
(define-tree Implicit (instances))

(define-tree Deprecated (names))
(define-tree Ignore (names))
(define-tree Ignorable (names))
(define-tree Inline (names))
(define-tree Notinline (names))
(define-tree Optimize (debug speed space))

; post-elaboration

(define-tree Make-Environment
  ())