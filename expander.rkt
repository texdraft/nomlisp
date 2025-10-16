#lang racket

(require "syntax.rkt"
         "ast.rkt"
         "evaluate.rkt"
         "elaborate.rkt"
         "environments.rkt")

(require racket/splicing)
(require syntax/parse)
(require syntax/parse/class/paren-shape)

(struct Context
  (synspaces ; current synspaces
   phase ; current phase
   environment)) ; current syntactic environment

;; the meaning of a name defined with define/syntax, when the rhs
;; is of type (→ Syntax Syntax)
(struct Transformer
  (procedure ; host-level procedure to do the expansion
   syntactic-environment)) ; environment at point of definition

;; the meaning of built-in things like define/module, case; as opposed
;; to user macros, these turn syntax objects into ASTs
(struct Primitive-Transformer
  (procedure))

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
    [(_ context (synspaces phase environment) . body)
     (match-let ([(Context synspaces phase environment) context])
       . body)]
    [(_ context outer (synspaces phase environment) . body)
     (match-let ([(Context synspaces phase environment) context])
       (let ([outer (Context synspaces phase (outer-environment environment))])
         . body))]))

;; go up a level of scoping
(define (outer-environment se)
  (Syntactic-Environment (cdr (Syntactic-Environment-frames se))))

;; bind name in current environment at current phase, given expander context
(define (bind-in-context! name synspace meaning context)
  (with-context context (_ phase environment)
    (add-syntactic-binding! name synspace phase synspace meaning environment)))

;; add new level of scope to context
(define (new-scope context)
  (with-context context (synspaces phase environment)
    (Context synspaces phase (add-frame environment))))

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

(define-match-expander $$
  (syntax-rules ()
    [(_ x)
     (app unwrap x)]))

(define-match-expander $
  (syntax-rules (TERM
                 BODY
                 LABEL
                 TYPE
                 TYPE-LIST
                 ID
                 PATTERN
                 COLON
                 BIND!
                 BIND-LIST!
                 PARAMETERS!
                 REST
                 @
                 ⇐)
    [(_ @ x)
     ($$ (At x))]
    [(_ ⇐ x)
     ($$ (Left-Double-Arrow x))]
    [(_ (TERM context expanded))
     (app (λ (e) (expand-term e context)) expanded)]
    [(_ (TYPE context expanded))
     (app (λ (e) (expand-type e context)) expanded)]
    [(_ (PATTERN context expanded))
     (app (λ (e) (expand-pattern e context)) expanded)]
    [(_ (BIND! context synspace v))
     (and (app unwrap (? Identifier?))
          (app (match-λ [(Identifier name)
                         (bind-in-context! name synspace context #t context)])
               v))]
    [(_ (BIND-LIST! context synspace vs))
     (and (app unwrap (? List?))
          (app (match-λ [($$ (List (list* (and variables
                                               (? (λ (x)
                                                    (Identifier? (unwrap x))))))))
                         (map (λ (v)
                                (bind-in-context! (Identifier-name (unwrap v))
                                                  synspace
                                                  context
                                                  #t
                                                  context)
                                (unwrap v))
                              variables)])
               vs))]
    [(_ (PARAMETERS! context expanded))
     (and ($$ (List _))
          (app (λ (x)
                 (match (unwrap x)
                   [(List parameters)
                    (map (λ (p) (expand-parameter p context))
                         parameters)]))
               expanded))]
    [(_ LABEL)
     ($$ (Label _))]
    [(_ ID)
     ($$ (? Identifier?))]
    [(_ (ID name))
     ($$ (Identifier name))]
    [(_ COLON)
     ($$ (Identifier ":"))]
    [(_ (p ... (REST rest)))
     (Syntax ($$ (List (list* ($ p) ... rest)))
             _ _ _)]
    [(_ (p ... (BODY context expanded)))
     (Syntax ($$ (List (list* ($ p) ... (app (λ (e) (expand-body e context)) expanded))))
             _ _ _)]
    [(_ (p ...))
     (Syntax ($$ (List (list ($ p) ...)))
             _ _ _)]
    [(_ p)
     (or (Syntax ($$ p) _ _ _)
         p)]))

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

(define (primordial-environment)
  (let ([e (empty-syntactic)])
    (define (add! name synspace p)
      (add-syntactic-binding! name macro-phase synspace p e))
    (define-syntax make-definers
      (syntax-rules ()
        [(_)
         (begin)]
        [(_ name! synspace . rest)
         (begin (define-syntax name!
                  (syntax-rules ()
                    [(_)
                     (begin)]
                    [(_ name1 p1 . more)
                     (begin (add! (symbol->string 'name1) synspace (Primitive-Transformer p1))
                            (name! . more))]))
                (make-definers . rest))]))
    (define-syntax keywords!
      (syntax-rules ()
        [(_)
         (begin)]
        [(_ name . rest)
         (begin (add! (symbol->string 'name) keyword-synspace 'keyword))]))
    (make-definers terms! term-synspace
                   types! type-synspace
                   patterns! pattern-synspace
                   declarations! declaration-synspace
                   synspaces! synspace-synspace)
    (terms! λ expand-λ
            the expand-the
            delimit expand-delimit
            capture-to expand-capture-to
            begin expand-begin
            quote expand-quote
            record expand-record-expression
            tuple expand-tuple-expression
            case expand-case
            let expand-let
            letrec expand-letrec)
    (types! ∀ expand-∀
            ⇒ expand-⇒
            record expand-record-type
            tuple expand-tuple-type)
    (patterns! as expand-as
               record expand-record-pattern
               tuple expand-tuple-pattern
               where expand-where)
    (declarations! define/module expand-define/module
                   define expand-define
                   define/type expand-define/type
                   define/class expand-define/class
                   define/instance expand-define/instance
                   define/syntax expand-define/syntax
                   define/sugar expand-define/sugar
                   implicit expand-implicit
                   import expand-import
                   splice expand-splice
                   for-syntax expand-for-syntax
                   declare expand-declare)
    (synspaces! term term-synspace
                type type-synspace
                pattern pattern-synspace
                module module-synspace
                declaration declaration-synspace
                synspace synspace-synspace)
    (keywords! class
               deprecated
               except
               for-syntax
               from
               ignorable
               ignore
               inline
               instance
               likely
               module
               notinline
               optimize
               pattern
               prefix
               syntax
               type
               unlikely
               unreachable
               :)
    e))

;; expand s-expression in given context. if add-unbound-synspace? is #f,
;; then an unbound identifier is considered an error; otherwise, the argument
;; is the synspace in which new entries for unbound identifiers are made.
(define (expand sx add-unbound-synspace? context)
  (with-context context (synspaces phase environment)
    (match sx
      [(? syntactic-closure?)
       4]
      [($$ (Identifier name))
       (match (lookup-syntactic name phase synspaces environment
                                (λ (name _ __)
                                  (if add-unbound-synspace?
                                      (bind-in-context! name add-unbound-synspace? #t context)
                                      (error "Unbound identifier"))))
         [(Transformer p transformer-environment)
          (expand (make-syntactic-closure transformer-environment '() (p sx environment))
                  add-unbound-synspace?
                  context)]
         [(Primitive-Transformer p)
          (p sx context)])]
      [($$ (Dotted left right))
       4]
      [($$ (Label _))
       (error "Label out of context")]
      [($$ (and c (Constant _)))
       c]
      [($ @ _) (error "@ out of context")]
      [($ ⇐ _) (error "⇐ out of context")])))

(define (expand-term term context)
  (expand term #f (set-synspaces context (list term-synspace))))

;; expand s-expression in type synspace. if defining? is true
;; then this “type” is really part of a define/type etc. declaration,
;; so we're gonna be adding new entries for all undefined identifiers.
(define (expand-type type defining? context)
  (expand type (set-synspaces context (list type-synspace))))

(define (expand-pattern pattern context)
  (expand pattern term-synspace (set-synspaces context (list term-synspace))))

(define (expand-body) 4)
(define (expand-parameter parameter context)
  (match parameter
    [($ @ ($ (BIND! context term-synspace v)))
     (Spread-Argument v)]
    [($ (PATTERN context p))
     p]))

(define (expand-list xs context)
  (map (λ (x)
         (expand x context))
       xs))

(define (expand-λ expression context)
  (define inner (new-scope context))
  (match expression
    [($ (λ (BIND! inner term-synspace i)
          COLON (TYPE inner t)
          (BODY inner b)))
     (Lambda i t b)]
    [($ (λ (and i (BIND! inner term-synspace))
          (BODY inner b)))
     (Lambda i #f b)]
    [($ (λ (PARAMETERS! inner ps)
          (BODY inner b)))
     (Lambda ps #f b)]))

(define-single-expander/inner (expand-the ($ (the (TYPE inner t)
                                                  (TERM inner e)))
                                          context
                                          inner)
  (The t e))

(define-single-expander (expand-delimit ($ (delimit (TERM context e1)
                                                    (TERM context e2)))
                                        context)
  (Delimit e1 e2))

(define-single-expander/inner (expand-capture-to ($ (capture-to (TERM context e1)
                                                                (BIND! inner term-synspace i)
                                                                (TERM context e2)))
                                                 context
                                                 inner)
  (Capture-To e1 i e2))

(define-single-expander/inner (expand-begin ($ (begin (BODY inner b))) context inner)
  (Body b))

(define (expand-quote expression context)
  (Quote expression))

(define (expand-record-expression expression context)
  (with-context context (synspaces phase environment)
    (match expression
      )))

(define-single-expander (expand-tuple-expression ($ (tuple (REST vs)))
                                                 context)
  (Make-Tuple (expand-list vs context)))

(define-single-expander (expand-case ($ (case (TERM context e)
                                          (REST clauses)))
                                     context)
  (Case e (map (λ (clause)
                 (define inner (new-scope context))
                 (match clause
                   [(list ($ (PATTERN inner p))
                          ($ (TERM inner e)))
                    (list p e)]))
               clauses)))

(define-single-expander/inner (expand-let ($ (let ((REST bindings)) (REST body)))
                                          outer inner)
  (Let (map (λ (binding)
              (match binding
                [(list ($ (PATTERN inner p))
                       ($ (TERM outer e)))
                 (list p e)]))
            bindings)
       (expand-body body inner)))

(define-single-expander/inner (expand-letrec ($ (letrec ((REST bindings)) (REST body)))
                                             context inner)
  (Let-Recursive (map (λ (binding)
                        (list (car binding)
                              (expand-term (cadr binding) inner)))
                      (map (λ (binding)
                             (match binding
                               [(list ($ (PATTERN inner p))
                                      ($ e))
                                (list p e)]))
                           bindings))
                 (expand-body body inner)))

(define-single-expander/inner (expand-∀ ($ (∀ (BIND-LIST! inner type-synspace vs) (TYPE context t)))
                                        context
                                        inner)
  (For-All vs t))

(define-single-expander (expand-⇒ ($ (⇒ (REST constraints+type))) context)
  4)

(define-single-expander (expand-record-type ($ (record (REST fields+row))) context)
  4)

(define-single-expander (expand-tuple-type ($ (tuple (REST ($ (TYPE context ts)))))
                                           context)
  (Tuple-Type ts))

(define-single-expander (expand-as ($ (as (BIND! context term-synspace v)
                                          (PATTERN context p)))
                                   context)
  (As v p))

(define-single-expander (expand-record-pattern ($ (record (REST fields+row))) context)
  4)

(define-single-expander (expand-tuple-pattern ($ (tuple (REST ($ (PATTERN context ps)))))
                                              context)
  (Tuple-Pattern ps))

(define-single-expander (expand-where ($ (where (PATTERN context p)
                                                (REST ($ ((PATTERN context ps)
                                                          (TERM context ts))))))
                                      context)
  (Where p (map list ps ts)))

;; the declaration expanders do not need to add whatever the declaration is
;; defining to the environment; it will have already been done

(define (expand-define/module expression context)
  4)

(define (expand-define expression context)
  4)

(define (expand-define/type expression context)
  4)

(define (expand-define/class expression context)
  4)

(define (expand-define/instance expression context)
  4)

(define (expand-define/syntax expression context)
  4)

(define (expand-define/sugar expression context)
  4)

(define (expand-implicit expression context)
  4)

(define (expand-import expression context)
  4)

(define (expand-splice expression context)
  4)

(define (expand-for-syntax expression context)
  4)

(define (expand-declare expression context)
  4)

