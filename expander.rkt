#lang racket

(require "syntax.rkt"
         "ast.rkt"
         "evaluate.rkt"
         "elaborate.rkt"
         "environments.rkt"
         "expander-support.rkt")

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
    [(_ (⇐ x))
     ($$ (Left-Double-Arrow x))]
    [(_ (TERM context expanded))
     (app (λ (e) (expand-term e context)) expanded)]
    [(_ (TYPE context expanded))
     (app (λ (e) (expand-type e context)) expanded)]
    [(_ (PATTERN context expanded))
     (app (λ (e) (expand-pattern e context)) expanded)]
    [(_ (BIND! context synspace v))
     (and (app unwrap (? Symbol?))
          (app (match-λ [(Symbol name)
                         (bind-in-context! name
                                           (list synspace)
                                           context
                                           (Name name)
                                           context)])
               v))]
    [(_ (BIND-LIST! context synspace vs))
     (and (app unwrap (? List?))
          (app (match-λ [($$ (List (list* (and variables
                                               (? (λ (x)
                                                    (Symbol? (unwrap x))))))))
                         (map (λ (v)
                                (define name (Symbol-name (unwrap v)))
                                (bind-in-context! name
                                                  (list synspace)
                                                  context
                                                  (Name name synspace #f)
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
     ($$ (? Symbol?))]
    [(_ (ID name))
     ($$ (Symbol name))]
    [(_ COLON)
     ($$ (Symbol ":"))]
    [(_ (p ... (REST rest)))
     (Syntax (or ($$ (List (list* ($ p) ... rest)))
                 (list* ($ p) ... rest))
             _)]
    [(_ (p ... (BODY context expanded)))
     (Syntax ($$ (List (list* ($ p) ... (app (λ (e) (expand-body e context)) expanded))))
             _)]
    [(_ (p ...))
     (Syntax (or ($$ (List (list ($ p) ...)))
                 (list ($ p) ...))
             _)]
    [(_ p)
     (or (Syntax ($$ p) _)
         p)]))

(define (primordial-environment)
  (let ([e (empty-syntactic)])
    (define (add! name synspace p)
      (add-syntactic-binding! name macro-phase (list synspace) p e))
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
                     (begin (add! (symbol->string 'name1)
                                  synspace
                                  (Primitive-Transformer p1 (symbol->string 'name1)))
                            (name! . more))]))
                (make-definers . rest))]))
    (define-syntax keywords!
      (syntax-rules ()
        [(_)
         (begin)]
        [(_ name . rest)
         (begin (add! (symbol->string 'name) keyword-synspace 'keyword))]))
    (define-syntax synspaces!
      (syntax-rules ()
        [(_)
         (begin)]
        [(_ name1 p1 . more)
         (begin (add! (symbol->string 'name1) synspace-synspace p1)
                (synspaces! . more))]))
    (make-definers terms! term-synspace
                   types! type-synspace
                   patterns! pattern-synspace
                   declarations! declaration-synspace)
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

(define (declaration-name? name)
  (member name '("define/module"
                 "define"
                 "define/type"
                 "define/class"
                 "define/instance"
                 "define/syntax"
                 "define/sugar"
                 "implicit"
                 "import"
                 "splice"
                 "for-syntax"
                 "declare")))

(define (allowed-primitive? name context)
  (or (and (in-synspaces? declaration-synspace context)
           (declaration-name? name))
      (and (in-synspaces? term-synspace context)
           (member name '("λ"
                          "the"
                          "delimit"
                          "capture-to"
                          "begin"
                          "quote"
                          "record"
                          "tuple"
                          "case"
                          "let"
                          "letrec")))
      (and (in-synspaces? type-synspace context)
           (member name '("∀"
                          "⇒"
                          "record"
                          "tuple")))
      (and (in-synspaces? pattern-synspace context)
           (member name '("as"
                          "record"
                          "tuple"
                          "where")))))

;; explicit renaming support

(define (close-syntax s-expression context)
  (traverse-symbols (match-λ [($$ (Symbol name))
                              (define result (lookup-in-context name context (const #f)))
                              (if result
                                  (Rename result)
                                  (Symbol name))])
                    s-expression))

(define (make-rename context)
  (λ (s-expression)
    (close-syntax s-expression context)))

(define (make-compare context)
  (λ (i1 i2)
    (match* ((close-syntax i1 context)
             (close-syntax i2 context))
      [((Rename m1) (Rename m2))
       (eqv? m1 m2)]
      [(_ _)
       #f])))

;; look for binding of name to module
(define (lookup-module name context)
  (with-context context (_ phase environment _)
    (lookup-syntactic name
                      phase
                      (list module-synspace)
                      environment)))

(struct Import
  (binding-name
   external-name
   synspace
   for-syntax?))

(define (ported-name-import ported synspace for-syntax?)
  (match ported
    [($ ((ID "rename") (ID external) (ID binding)))
     (list (Import binding external synspace for-syntax?))]
    [($ (ID name))
     (list (Import name name synspace for-syntax?))]))

(define (compute-imported-bindings import)
  (match import
    [($ ((ID "module") m))
     (ported-name-import m module-synspace #f)]
    [($ ((ID "type") t))
     (ported-name-import t type-synspace #f)]
    [($ ((ID "class") c))
     (ported-name-import c type-synspace #f)]
    [($ ((ID "instance") i))
     (ported-name-import i type-synspace #f)]
    [($ ((ID "pattern") p))
     (ported-name-import p pattern-synspace #f)]
    [($ ((ID "syntax") ports))
     (flatten (map (λ (port)
                     (match port
                       [($ ((ID synspace) (REST ps)))
                        (map (λ (p)
                               (ported-name-import p synspace #t))
                             ps)]
                       [($ (synspaces (REST ps)))
                        (map (λ (synspace)
                               (map (λ (p)
                                      (ported-name-import p synspace #t))
                                    ps)))]))
                   ports))]
    [_
     (ported-name-import import term-synspace #f)]))

(define ((make-filter except?) module imports)
  (define imported (append-map compute-imported-bindings imports))
  (λ (internal-name external-name1 synspace1 for-syntax?1)
    (let ([found (findf (match-λ [(Import _ external-name2 synspace2 for-syntax?2)
                                  (and (equal? external-name1 external-name2)
                                       (eqv? synspace1 synspace2)
                                       (equal? for-syntax?1 for-syntax?2))]))])
      (if (if except?
              found
              (not found))
          (values (Import-binding-name found)
                  (with-context (Module-context module) (synspaces phase environment _)
                    (lookup-syntactic internal-name
                                      (if for-syntax?1
                                          (+ phase 1)
                                          phase)
                                      synspaces
                                      environment)))
          (values #f #f)))))

(define from-filter
  (make-filter #f))

(define except-filter
  (make-filter #t))

;; perform an import by mutating the context
(define (do-import! import context prefix? for-syntax?)
  (with-context context (synspaces phase environment _)
    (match import
      [($ ((ID "for-syntax") (REST imports)))
       (map (λ (i)
              (do-import! i (shifted-context context) prefix?))
            imports)]
      [($ ((ID "prefix") (Symbol prefix) import))
       (when prefix?
         (error "Already have a prefix"))
       (do-import! import context prefix)]
      [($ ((ID "from") (ID module-name) (REST imports)))
       (define module (lookup-module module-name context))
       (import-module! module
                       (from-filter module imports)
                       prefix?
                       for-syntax?
                       context)]
      [($ ((ID "except" (ID module-name) (REST imports))))
       (define module (lookup-module module-name context))
       (import-module! module
                       (except-filter module imports)
                       prefix?
                       for-syntax?
                       context)])))

(define (ensure-expanded-module! module)
  (match module
    [(Module exports
             unexpanded-body
             expanded-body
             inner)
     (unless expanded-body
       (set-Module-expanded-body! (expand-body unexpanded-body inner #t))
       (check-exports module))]))

;; when doing define/module, we add the module binding, which holds the export
;; list, *before* the body is expanded. as a result, we can't check whether
;; the export list is valid until later.

(define (check-exports module)
  (match module
    [(Module exports _ _ inner)
     (with-context inner (_ phase environment _)
       (for ([export exports])
         (match export
           [(Export external-name
                    internal-name
                    synspace
                    for-syntax?)
            (lookup-syntactic internal-name
                              (if for-syntax?
                                  (+ phase 1)
                                  phase)
                              (list synspace)
                              environment)])))]))

;; import all bindings exported from a module, applying filter to each one.
;; if filter returns false, the binding is omitted, otherwise filter should
;; return a name and a meaning to add to the environment.
(define (import-module! module context filter prefix? for-syntax?)
  (with-context context (_ phase environment _)
    (define (maybe-prefix name)
      (if prefix?
          (format "~A~A" prefix? name)
          name))
    (match module
      [(Module exports
               unexpanded-body
               expanded-body
               inner)
       (ensure-expanded-module! module)
       (for ([export exports])
         (match export
           [(Export external-name
                    internal-name
                    synspace
                    for-syntax?)
            (define-values (name meaning) (filter internal-name
                                                  external-name
                                                  synspace
                                                  for-syntax?))
            (when name
              (add-syntactic-binding! (maybe-prefix name)
                                      (if for-syntax?
                                          (+ phase 1)
                                          phase)
                                      (list synspace)
                                      meaning
                                      environment))]))])))

;; resolve dotted name to binding referred to
(define (resolve! dotted context)
  (match (unwrap dotted)
    [(Dotted ($ (ID module)) ($$ x))
     (define inner (new-scope context))
     (import-module! (lookup-module module context)
                     inner)
     (match x
       [(Dotted _ _)
        (resolve! x inner)]
       [(Symbol name)
        (lookup-in-context name inner)])]))

;;; NB: all expansion functions are inherently destructive; they mutate the
;;; environment to add bindings. I don't mark their names with ! because it's
;;; not really helpful when reading the code.

(define (transform meaning sx context)
  (match meaning
    [(Transformer p context)
     (close-syntax (mark-macro-generated (p sx
                                            (make-rename context)
                                            (make-compare context)))
                   context)]
    [(Primitive-Transformer p n)
     (if (allowed-primitive? n context)
         (p sx context)
         (error (format "Primitive operator ~A used out of context" n)))]))

(struct Class
  (type
   info)
  #:transparent)

(define (classify-meaning name meaning unbound)
  (match meaning
    [(? Transformer?)
     (Class 'macro-id meaning)]
    [(Primitive-Transformer _ name)
     (Class 'primitive-id (cons meaning name))]
    [(External _ _)
     (Class 'external meaning)]
    [(Name _ _ _)
     (Class 'non-macro meaning)]
    [_
     (if (eqv? meaning unbound)
         (Class 'unbound name)
         (Class 'random meaning))]))

(define (classify-combination operator-classification operator operands)
  (match operator-classification
    [(Class 'macro-id meaning)
     (Class 'macro-call meaning)]
    [(Class 'primitive-id (cons meaning name))
     (if (declaration-name? name)
         (Class 'declaration (cons name meaning))
         (Class 'primitive meaning))]
    [(Class 'unbound name)
     (Class 'unbound-application (cons name operands))]
    [(Class 'rename meaning)
     (classify-meaning #f meaning #f)]
    [_
     (Class 'application (cons operator operands))]))

(define (classify sx context)
  (match sx
    [($$ (Rename meaning))
     (classify-meaning #f meaning #f)]
    [($$ (? Expanded?))
     (Class 'expanded sx)]
    [($$ (Symbol name))
     (define unbound (cons #f #f))
     (classify-meaning name
                       (lookup-in-context name context (const unbound))
                       unbound)]
    [($$ (and d (Dotted _ _)))
     (classify-meaning (resolve! d context) #f)]
    [($$ (List (list* ($$ operator) operands)))
     (classify-combination (classify operator context)
                           operator
                           operands)]
    [($$ (and c (Constant _)))
     (Class 'constant c)]
    [($$ (and l (Label _)))
     (Class 'label l)]
    [($ @ x)
     (Class '@ x)]
    [($ ⇐ x)
     (Class '⇐ x)]))

(define (self-expanding? class)
  (match class
    [(or (Class 'expanded _)
         (Class 'constant _)
         (Class 'label _)
         (Class 'random _))
     #t]
    [_
     #f]))

(define (unbound-error/context name context)
  (with-context context (synspaces phase _ _)
    (unbound-error name phase synspaces)))

(define (combine operator operands context)
  (define (make c)
    (c (expand operator context)
       (expand-list operands context)))
  (do-first-synspace context
                     #:term (make Call)
                     #:type (make Type-Application)
                     #:pattern (make Constructor-Pattern)))

;; expand s-expression in given context
(define (expand sx context)
  (define add-unbound-synspace? (Context-add-unbound-synspace? context))
  (define (maybe-add! name)
    (if add-unbound-synspace?
        (bind-in-context! name
                          (list add-unbound-synspace?)
                          (Name name add-unbound-synspace? #f)
                          context)
        (unbound-error/context name context)))
  (match (classify sx context)
    ;; atomic forms
    [(? self-expanding?)
     sx]
    [(Class 'non-macro x)
     x]
    [(Class '@ x)
     (if (in-synspaces? term-synspace context)
         (Spread-Argument (expand x context))
         (error "@ used out of context"))]
    [(Class '⇐ x)
     (if (or (in-synspaces? term-synspace context)
             (in-synspaces? type-synspace context))
         (Instance-Supply (expand-type x add-unbound-synspace? context))
         (error "⇐ used out of context"))]
    [(Class 'unbound name)
     (maybe-add! name)]
    [(Class 'macro-id meaning)
     (transform meaning sx context)]
    [(Class 'primitive-id name)
     (error (format "Primitive ~A used out of context" name))]
    ;; compound forms
    [(or (Class 'macro-call meaning)
         (Class 'primitive meaning))
     (expand (transform meaning sx context)
             context)]
    [(Class 'declaration (cons name _))
     (error (format "Declaration ~A used out of context" name))]
    [(Class 'unbound-application (cons name operands))
     (combine (maybe-add! name) operands context)]
    [(Class 'application (cons operator operands))
     (combine operator operands context)]))

(define (expand-term term context)
  (expand term (set-synspaces context (list term-synspace))))

;; expand s-expression in type synspace. if defining? is true
;; then this “type” is really part of a define/type etc. declaration,
;; so we're gonna be adding new entries for all undefined identifiers.
(define (expand-type type defining? context)
  (with-context (set-synspaces context (list type-synspace)) (synspaces
                                                              phase
                                                              environment
                                                              _)
    (expand type
            (Context synspaces phase environment (and defining? type-synspace)))))


(define (expand-pattern pattern context)
  (expand pattern (set-synspaces context (list term-synspace))))

;; elaboration turns a body with only declarations into Let or Let-Recursive
;; around a Make-Record expression. this function evaluates this, and merges
;; the “bindings” embodied by the resulting record object into the syntactic
;; environment
(define (evaluate-for-syntax elaborated evaluation-context shifted-context)
  (define record (evaluate elaborated evaluation-context))
  (define (get-name label)
    (match label
      [(Label name)
       name]))
  (for ([(label value) record])
    (bind-in-context! (get-name label)
                      (list term-synspace)
                      value
                      shifted-context)))

(define (grab-runtime-values context)
  (runtime-environment-from-association-list
   (map (match-λ [(list meaning name _ _)
                  (cons name meaning)])
        (filter-bindings (λ (meaning name synspace phase)
                           (and (= phase (+ (Context-phase context) 1))
                                (eqv? synspace term-synspace)))
                         (Context-environment context)))))

(define (preprocess-body body inner module?)
  (define elaboration-context (empty-elaboration-context))
  (define evaluation-context (grab-runtime-values (outer-context inner)))
  (let loop ([items (match body
                      [($$ (List l))
                       l]
                      [(list* l)
                       body])]
             [declarations '()])
    (if (null? items)
        (if module?
            (values declarations items)
            (error "Body can't have only declarations"))
        (let ([item (car items)])
          (match (classify (car items) inner)
            [(Class 'declaration (cons "define/syntax" _))
             (match item
               [($ (define/syntax (ID name) ((REST ($$ synspaces))) (REST body)))
                (define shifted (shifted-context inner))
                (define expanded (expand-body body shifted))
                (define elaborated (elaborate/transformer? expanded elaboration-context))
                (define value (evaluate elaborated evaluation-context))
                (bind-in-context!
                 name
                 (map (λ (name)
                        (lookup-in-context name
                                           (set-synspaces inner
                                                          (list synspace-synspace))))
                      synspaces)
                 (Transformer value inner)
                 inner) ; bind at current phase
                (loop (cdr items) declarations)])]
            [(Class 'declaration (cons "define/sugar" _))
             (error "Sugar not implemented yet")]
            [(Class 'declaration (cons "for-syntax" _))
             (match (car items)
               [($ (for-syntax (REST declarations)))
                (define shifted (shifted-context inner))
                (define expanded (Body (expand-list declarations
                                                    #f
                                                    (set-synspaces shifted
                                                                   (list declaration-synspace)))
                                       '()))
                (define elaborated (elaborate/body expanded elaboration-context))
                (evaluate-for-syntax elaborated evaluation-context shifted)
                (loop (cdr items) declarations)])]
            [(Class 'declaration (cons "splice" _))
             (match item
               [($ (splice (REST declarations)))
                (loop (append declarations items) declarations)])]
            [(Class 'declaration (cons "import" expander))
             (expander item 'binding inner)
             (loop (cdr items) declarations)]
            [(Class 'declaration (cons name expander))
             (expander item 'binding inner)
             (loop (cdr items) (cons (cons expander item)
                                     declarations))]
            [_
             (values declarations items)])))))

(define (expand-body body context [module? #f])
  (define inner (new-scope (add-synspace (add-synspace context term-synspace)
                                         declaration-synspace)))
  (let-values ([(declarations expressions) (preprocess-body body inner module?)])
    (Body (map (match-λ [(cons p d)
                         (p d 'expanding inner)])
               declarations)
          (expand-list expressions #f inner))))

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

(define-single-expander (expand-record-expression ($ (record (REST fields+row))) context)
  (let loop ([stuff (match fields+row
                      [($$ (List l))
                       l])]
             [fields+values '()])
    (match stuff
      [(list)
       (Make-Record (reverse fields+values) #f)]
      [(list row)
       (Make-Record (reverse fields+values) (expand row context))]
      [(list ($$ (Label l)) x rest ...)
       (loop rest
             (cons (cons (Label l) (expand x context))
                   fields+values))])))

(define-single-expander (expand-tuple-expression ($ (tuple (REST vs)))
                                                 context)
  (Make-Tuple (expand-list vs context)))

(define-single-expander (expand-case ($ (case (TERM context e)
                                          (REST clauses)))
                                     context)
  (Case e (map (λ (clause)
                 (define inner (new-scope context))
                 (match clause
                   [($$ (List (list ($ (PATTERN inner p))
                                    ($ (TERM inner e)))))
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
  (let loop ([stuff (match constraints+type
                      [($$ (List l))
                       l])]
             [constraints '()])
    (match stuff
      [(list type)
       (Constrained constraints (expand type context))]
      [(list ($$ (Symbol instance)) ($ COLON) type rest ...)
       (define name (Name instance type-synspace #f))
       (bind-in-context! instance (list type-synspace) name context)
       (loop rest
             (cons (Constraint name (expand type context))
                   constraints))]
      [(list type rest ...)
       (loop rest
             (cons (Constraint #f (expand type context))
                   constraints))])))

(define-single-expander (expand-record-type ($ (record (REST fields+row))) context)
  (let loop ([stuff (match fields+row
                      [($$ (List l))
                       l])]
             [fields+types '()])
    (match stuff
      [(list)
       (Record-Type (reverse fields+types) #f)]
      [(list row)
       (Record-Type (reverse fields+types) (expand row context))]
      [(list ($$ (Label l)) x rest ...)
       (loop rest
             (cons (cons (Label l) (expand x context))
                   fields+types))])))

(define-single-expander (expand-tuple-type ($ (tuple (REST ($ (TYPE context ts)))))
                                           context)
  (Tuple-Type ts))

(define-single-expander (expand-as ($ (as (BIND! context term-synspace v)
                                          (PATTERN context p)))
                                   context)
  (As v p))

(define-single-expander (expand-record-pattern ($ (record (REST fields+row))) context)
  (let loop ([stuff (match fields+row
                      [($$ (List l))
                       l])]
             [fields+patterns '()])
    (match stuff
      [(or (list ($$ (Label l)))
           (list ($$ (Label l)) ($$ (Label _)) _ ...))
       (define name (Name l term-synspace #f))
       (bind-in-context! l (list term-synspace) name context)
       (loop (cdr stuff)
             (cons (cons (Label l)
                         name)))]
      [(list)
       (Record-Pattern (reverse fields+patterns) #f)]
      [(list ($$ row))
       (Record-Pattern (reverse fields+patterns) (expand row context))]
      [(list ($$ (Label l)) ($$ pattern) rest ...)
       (loop rest
             (cons (cons (Label l) (expand pattern context))
                   fields+patterns))])))

(define-single-expander (expand-tuple-pattern ($ (tuple (REST ($ (PATTERN context ps)))))
                                              context)
  (Tuple-Pattern ps))

(define-single-expander (expand-where ($ (where (PATTERN context p)
                                                (REST ($ ((PATTERN context ps)
                                                          (TERM context ts))))))
                                      context)
  (Where p (map list ps ts)))

;; the mode argument in the declaration expanders can be one of two values:
;; * 'binding means that we're just binding names (by mutating the context)
;; * 'expanding means that we're actually expanding to an AST

(define (parse-exports exports prefix for-syntax?)
  (flatten (map parse-export exports)))

(define (exported-name name synspace prefix? for-syntax?)
  (define (maybe-prefix name)
    (if prefix?
        (format "~A~A" prefix? name)
        name))
  (match name
    [($ ((ID "rename") internal-name external-name))
     (Export (maybe-prefix external-name) internal-name synspace for-syntax?)]
    [($$ (Symbol internal-name))
     (Export (maybe-prefix internal-name) internal-name synspace for-syntax?)]))

(define (parse-export export prefix? for-syntax?)
  (match export
    [($ ((ID "syntax") (REST exports)))
     (flatten (map (λ (port)
                     (match port
                       [($ ((ID synspace) (REST ps)))
                        (map (λ (p)
                               (exported-name p synspace prefix? for-syntax?))
                             ps)]
                       [($ (((REST synspaces)) (REST ps)))
                        (map (λ (synspace)
                               (map (λ (p)
                                      (exported-name p synspace prefix? for-syntax?))
                                    ps))
                             (unwrap synspaces))]))
                   exports))
     (parse-exports exports prefix? for-syntax?)]
    [($ ((ID "and") (REST exports)))
     (parse-exports exports prefix? for-syntax?)]
    [($ ((ID "for-syntax") (REST exports)))
     (parse-exports exports prefix? #t)]
    [($ ((ID "prefix") (ID name) (REST exports)))
     (parse-exports exports name for-syntax?)]
    [($ ((ID "module") name))
     (exported-name name module-synspace prefix? for-syntax?)]
    [(or ($ ((ID "type") name))
         ($ ((ID "class") name))
         ($ ((ID "instance") name)))
     (exported-name name type-synspace prefix? for-syntax?)]
    [($ ((ID "pattern") name))
     (exported-name name pattern-synspace prefix? for-syntax?)]
    [_
     (exported-name export term-synspace prefix? for-syntax?)]))

(define (expand-define/module expression mode context)
  (match expression
    [($ (define/module (ID name) exports (REST body)))
     (case mode
       [(binding)
        (bind-in-context! name
                          (list module-synspace)
                          (Module (parse-exports exports)
                                  body
                                  #f
                                  context)
                          context)]
       [(expanding)
        (define module (lookup-module name context))
        (match module
          [(Module exports _ expanded-body _)
           (Define-Module (Name name) exports (cond [expanded-body]
                                                    [else
                                                     (ensure-expanded-module! module)]))])])]))

(define (expand-define expression mode context)
  (define inner (new-scope context))
  (define (bind/expand name type body)
    (case mode
      [(binding)
       (bind-in-context! name (list term-synspace) (Name name) context)]
      [(expanding)
       (Define (lookup-in-context name (set-synspaces context (list term-synspace)))
               (if type
                   (expand-type type #f context)
                   #f)
               (if (Expanded? body)
                   body
                   (expand-body body inner)))]))
  (define (make-lambda parameters type body context)
    (Lambda (map (λ (parameter)
                   (expand-parameter parameter context))
                 parameters)
            type
            (expand-body body context)))
  (match expression
    [($ (define_ (ID name) COLON type (REST body)))
     (bind/expand name type body)]
    [($ (define_ (ID name) (REST body)))
     (bind/expand name #f body)]
    [($ (define_ ((ID name) (REST parameters)) COLON type (REST body)))
     (define inner (new-scope context))
     (bind/expand name type (make-lambda parameters type body inner))]
    [($ (define_ ((ID name) (REST parameters)) (REST body)))
     (bind/expand name #f (make-lambda parameters #f body inner))]))

(define (parse-namething namething context inner)
  (match namething
    [($ ((ID "⇒") (REST (list constraints ... type))))
     (define expanded-constraints (expand-list constraints
                                               (set-synspaces inner (list type-synspace))))
     (define-values (name _) (parse-defining-type-name type context inner))
     (values name (Constrained expanded-constraints (expand-type type #f context)))]
    [($ ((ID "∀") ((REST parameters)) type))
     (define parameters (expand-type-parameters parameters inner))
     (define-values (name expanded) (parse-defining-type-name type context inner))
     (values name (For-All parameters expanded))]
    [($ (ID name))
     (define the-name (Name name))
     (bind-in-context! name (list type-synspace) the-name context)
     (values name the-name)]
    [($ (type (⇐ type2)))
     (define-values (name expanded) (parse-defining-type-name type context inner))
     (values name (Type-Application expanded (Instance-Supply (expand-type type2 #f context))))]
    [($ (type type2))
     (define-values (name expanded) (parse-defining-type-name type context inner))
     (values name (Type-Application expanded (expand-type type2 #t context)))]
    [($ (type (REST _)))
     (parse-defining-type-name (curryify namething)
                               context
                               inner)]))

(define (curryify application)
  (match application
    [(list f x)
     application]
    [(list f x y rest ...)
     (let loop ([curried #f]
                [remaining application])
       (if (null? remaining)
           curried
           (loop (list curried (car remaining))
                 (cdr remaining))))]))

(define (expand-type-parameters parameters context)
  (let loop ([parameters parameters]
             [expanded '()])
    (match parameters
      [(list)
       (reverse expanded)]
      [(list type)
       (loop (cdr parameters)
             (cons (expand-type type #t context)
                   expanded))]
      [(list ($$ (Symbol type-parameter)) ($ COLON) type rest ...)
       (define name (Name type-parameter type-synspace #f))
       (bind-in-context! type-parameter name (set-synspaces context (list type-synspace)))
       (loop rest
             (cons (Annotated name (expand-type type #t context))
                   expanded))]
      [(list type rest ...)
       (loop rest
             (cons (Constraint #f (expand type context))
                   expanded))])))

(define (parse-defining-type-name namething context inner)
  (let ([context (struct-copy Context (set-synspaces (list type-synspace) context)
                              (add-unbound-synspace? type-synspace))])
    (parse-namething namething context inner)))

(define (expand-define/type expression mode context)
  (match expression
    [($ (define/type namething synonym))
     (case mode
       [(binding)
        (parse-defining-type-name namething context (new-scope context))]
       [(expanding)
        (define inner (new-scope context))
        (define-values (name etc.) (parse-defining-type-name namething context inner))
        (Define-Type name etc. (expand-type synonym #f inner))])]
    [($ (define/type namething (REST (and rest ($ (n1 COLON t1 (REST _)))))))
     4]
    [($ (define/type namething (REST (and rest ($ (e1 e2 (REST _)))))))
     4]))

(define (expand-define/class expression mode context)
  (match expression
    [($ (define/class namething (REST body)))
     (case mode
       [(binding)
        (define expanded (expand-type namething #t context))
        4]
       [(expanding)
        4])]))

(define (expand-define/instance expression mode context)
  (match expression
    [($ (define/instance (ID name) ($ constraint) (REST body)))
     (case mode
       [(binding)
        (bind-in-context! name (list type-synspace) (Name name) context)]
       [(expanding)
        (define inner (new-scope context))
        (Define-Instance (lookup-in-context name (set-synspaces context (list term-synspace)))
                         (expand-type constraint #f inner)
                         (expand-list body (set-synspaces inner (list declaration-synspace))))])]))

(define (expand-declare expression mode context)
  (match expression
    [($ (declare (REST metadata)))
     (Declare metadata)]))

(define (expand-import expression mode context)
  (match expression
    [($ (import (REST imported)))
     (when (eqv? mode 'expanding)
       (error "Internal error: import should bind only"))]))

(define (expand-implicit expression mode context)
  (match expression
    [($ (implicit (REST types)))
     (when (eqv? mode 'expanding)
       (Implicit (expand-list types (set-synspaces context (list type-synspace)))))]))

(define (bad-declaration name context)
  (error "Internal error: ~A shouldn't be handled like this"))

(define-syntax-rule (define-bad name operator)
  (define (name expression mode context)
    (bad-declaration 'operator context)))

(define-bad expand-define/syntax define/syntax)
(define-bad expand-define/sugar define/sugar)
(define-bad expand-splice splice)
(define-bad expand-for-syntax for-syntax)

(define (to-syntax x)
  (match x
    [(? symbol?)
     (make-syntax (Symbol (symbol->string x)) (Origin #f #f #f #f))]
    [(list xs ...)
     (make-syntax (List (map to-syntax xs)) (Origin #f #f #f #f))]
    [(? Syntax?)
     x]
    [(? number?)
     (make-syntax (Constant x) (Origin #f #f #f #f))]))

(define (test)
  (define context (Context (list term-synspace) 0 (primordial-environment) #f))
  (bind-in-context! "True"
                    (list term-synspace pattern-synspace)
                    (Name "True" (list term-synspace pattern-synspace) #f)
                    context)
    (bind-in-context! "False"
                    (list term-synspace pattern-synspace)
                    (Name "False" (list term-synspace pattern-synspace) #f)
                    context)
  (bind-in-context! "if"
                    (list term-synspace)
                    (Transformer (λ (s r c)
                                   (match s
                                     [($$ (List (list if p e1 e2)))
                                      (to-syntax `(,(r (to-syntax 'case))
                                                   ,p
                                                   (,(r (to-syntax 'True))
                                                    ,e1)
                                                   (,(r (to-syntax 'False))
                                                    ,e2)))]))
                                 context)
                    context)
  (expand (to-syntax '(if 1 2 3))
          context))
