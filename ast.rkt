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

(define-tree Symbol (name))
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

;; Representation of fully expanded programs

(struct Expanded ())

(define-syntax define-tree/expanded
  (syntax-rules ()
    [(_ name (fields ...))
     (struct name Expanded (fields ...)
             #:methods gen:custom-write
             [(define write-proc
                (make-constructor-style-printer
                 (lambda (s) 'name)
                 (lambda (s) (match s
                               [(name fields ...)
                                (list fields ...)]))))])]))

;; resolved name; stores source name, namespace/synspace,
;; and origin for error messages
(struct Name Expanded
  (name
   space
   origin)
  #:methods gen:custom-write
  [(define (write-proc s out _)
     (fprintf out
              "⟨~A:~A~A⟩"
              (Name-name s)
              (Name-space s)
              (if (Name-origin s)
                  (format " ~V" (Name-origin s))
                  "")))])

(struct Module
  ((exports #:mutable) ; list of Export
   unexpanded-body ; body as syntax object
   (expanded-body #:mutable) ; body as AST or #f
   (context #:mutable))) ; expansion context at point of define/module

(struct Export
  (external-name
   internal-name
   synspace
   for-syntax?))

(define-tree/expanded External (module name))

(define-tree/expanded Program (modules))

(define-tree/expanded Body (declarations expressions))

(define-tree/expanded Instance-Supply (x)) ; ⇐ x
(define-tree/expanded Spread-Argument (x)) ; @ x

(define-tree/expanded Annotated ; x : t
  (name type))

; terms

(define-tree/expanded Lambda (parameters body)) ; parameters is list of id or single id
(define-tree/expanded The (type expression))
(define-tree/expanded Delimit (prompt expression))
(define-tree/expanded Capture-To (name prompt expression))
(define-tree/expanded Make-Record (fields+values row))
(define-tree/expanded Make-Tuple (values))
(define-tree/expanded Case (expression patterns+expressions))
(define-tree/expanded Let (bindings body))
(define-tree/expanded Let-Recursive (bindings body))
(define-tree/expanded Call (operator operands))
(define-tree/expanded Quote (datum))

; types

(define-tree/expanded For-All (variables type))
(define-tree/expanded Arrow (types))
(define-tree/expanded Constrained (constraints type))
(define-tree/expanded Record-Type (fields+types row))
(define-tree/expanded Tuple-Type (types))
(define-tree/expanded Type-Application (operator operand))

(define-tree/expanded Constraint (instance-variable? class))

; patterns

(define-tree/expanded Pattern (pattern variables))

(define-tree/expanded As (name pattern))
(define-tree/expanded Constructor-Pattern (name patterns))
(define-tree/expanded Record-Pattern (fields+patterns row))
(define-tree/expanded Tuple-Pattern (patterns))
(define-tree/expanded Where (pattern guards))

; declarations

(define-tree/expanded Define-Type (name etc. definition))
(define-tree/expanded Define-Data (name etc. constructors))
(define-tree/expanded Define-Class (name etc. items))
(define-tree/expanded Define-Module (name exports body))
(define-tree/expanded Define-Instance (name constraint items))
(define-tree/expanded Define (name type body))
(define-tree/expanded Declare (metadata))
(define-tree/expanded Implicit (instances))

(define-tree/expanded Deprecated (names))
(define-tree/expanded Ignore (names))
(define-tree/expanded Ignorable (names))
(define-tree/expanded Inline (names))
(define-tree/expanded Notinline (names))
(define-tree/expanded Optimize (debug speed space))

; post-elaboration

(define-tree/expanded Make-Environment
  ())