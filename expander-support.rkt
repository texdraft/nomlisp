#lang racket

(provide (struct-out Context)
         (struct-out Transformer)
         (struct-out Primitive-Transformer)
         (struct-out Rename)
         
         macro-phase

         ;; synspaces
         term-synspace
         type-synspace
         pattern-synspace
         module-synspace
         declaration-synspace
         keyword-synspace
         synspace-synspace

         ;; functions
         outer-environment
         outer-context
         shifted-context
         bind-in-context!
         new-scope
         add-synspace
         remove-synspace
         set-synspaces
         in-synspaces?
         do-first-synspace
         lookup-in-context

         ;; match expanders
         $$
         Sugar-Template

         ;; macros
         with-context         
         define-single-expander
         define-single-expander/inner)

(require "ast.rkt"
         "syntax.rkt"
         "environments.rkt")
(require racket/splicing)

(struct Context
  (synspaces ; current synspaces
   phase ; current phase
   environment ; current syntactic environment
   add-unbound-synspace?)
  #:transparent)

;;; expansion-time bindings

;; the meaning of a name defined with define/syntax, when the rhs
;; is of type (→ Syntax Syntax)
(struct Transformer
  (procedure ; host-level procedure to do the expansion
   syntactic-environment)) ; environment at point of definition

;; the meaning of built-in things like define/module, case; as opposed
;; to user macros, these turn syntax objects into ASTs
(struct Primitive-Transformer
  (procedure
   name))

;; expander-only tree used for explicit renaming; when one of these is
;; encountered, it's replaced by the meaning field
(define-tree Rename (meaning))

;; a name bound by import is mapped to an External AST, unless it's a macro,
;; in which case it is mapped directly to the transformer

(define macro-phase 0)

(splicing-let-syntax ([define-synspace
                        (syntax-rules ()
                          [(_ name1 name2)
                           (define name1 (Synspace name2))])])
  ;; built-in synspaces
  (define-synspace term-synspace "term")
  (define-synspace type-synspace "type")
  (define-synspace pattern-synspace "pattern")
  (define-synspace module-synspace "module")
  (define-synspace declaration-synspace "declaration")
  (define-synspace synspace-synspace "synspace")
  (define-synspace keyword-synspace "keyword"))

;; helper to deconstruct context
(define-syntax with-context
  (syntax-rules ()
    [(_ context (synspaces phase environment add-unbound-synspace?) . body)
     (match-let ([(Context synspaces phase environment add-unbound-synspace?) context])
       . body)]))

;; go up a level of scoping
(define (outer-environment se)
  (Syntactic-Environment (cdr (Syntactic-Environment-frames se))))

(define (outer-context context)
  (with-context context (synspaces phase environment add-unbound-synspace?)
    (Context synspaces phase (outer-environment environment) add-unbound-synspace?)))

(define (shifted-context context)
  (with-context context (synspaces phase environment _)
    (Context synspaces (+ phase 1) environment)))

;; bind name in current environment at current phase, given expander context
(define (bind-in-context! name synspaces meaning context)
  (with-context context (_ phase environment _)
    (add-syntactic-binding! name phase synspaces meaning environment)))

;; add new level of scope to context
(define (new-scope context)
  (with-context context (synspaces phase environment add-unbound-synspace?)
    (Context synspaces phase (add-frame environment) add-unbound-synspace?)))

;; add synspace to current
(define (add-synspace context synspace)
  (struct-copy Context context
               [synspaces (cons synspace (remq synspace (Context-synspaces context)))]))

;; remove synspace from current
(define (remove-synspace context synspace)
  (struct-copy Context context
               [synspaces (remq synspace (Context-synspaces context))]))

;; replace synspaces
(define (set-synspaces context synspaces)
  (struct-copy Context context
               [synspaces synspaces]))

(define (in-synspaces? synspace context)
  (with-context context (synspaces _ _ _)
    (memq synspace synspaces)))

(define (do-first-synspace context
                           #:term [term-p identity]
                           #:type [type-p identity]
                           #:pattern [pattern-p identity]
                           #:declaration [declaration-p identity])
  (define mapping
    (hash term-synspace term-p
          type-synspace type-p
          pattern-synspace pattern-p
          declaration-synspace declaration-p))
  (with-context context (synspaces _ _ _)
    (let/ec found
      (for ([synspace synspaces])
        (define result (hash-ref mapping synspace (const #f)))
        (when result
          (found (result)))))))

;; lookup a name in context
(define (lookup-in-context name context [k unbound-error])
  (with-context context (synspaces phase environment _)
    (lookup-syntactic name phase synspaces environment
                      k)))

(define-match-expander $$
  (syntax-rules ()
    [(_ x)
     (app unwrap x)]))

(define-match-expander Sugar-Template
  (syntax-rules ()
    [(_)
     (or (Prefixed _ _)
         (Delimited _ _)
         (Prefixed _ (Delimited _ '())))]))

(define-syntax define-single-expander
  (syntax-rules ()
    [(_ (name pattern context) . body)
     (define (name expression context)
       (match expression
         [pattern . body]))]))

(define-syntax define-single-expander/inner
  (syntax-rules ()
    [(_ (name pattern context inner) . body)
     (define (name expression context)
       (define inner (new-scope context))
       (match expression
         [pattern . body]))]))