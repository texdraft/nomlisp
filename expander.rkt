#lang racket

(require "syntax.rkt"
         "ast.rkt"
         "evaluate.rkt"
         "elaborate.rkt")

(provide expand/program
         empty-tower)

(struct Syntactic-Environment
  (terms
   patterns
   types
   modules)
  #:mutable
  #:transparent)

(struct Meta-Environment
  (syntactic
   elaboration
   runtime)
  #:transparent)

(struct Syntactic-Binding
  (name/template
   meaning))

(struct Template
  (prefix
   delimiters))

(struct Transformer
  (procedure
   syntactic-environment))

(struct Leave-Alone
  ())

(struct Rename
  (alias))

(define (empty-syntactic)
  (Syntactic-Environment '() '() '() '()))

(define (new-tower syntactic elaboration runtime [rest #f])
  (let build ()
    (stream-cons (Meta-Environment syntactic elaboration runtime)
                 (or rest (build)))))

(define (empty-tower)
  (new-tower (empty-syntactic)
             #f
             (empty-runtime-environment)))

(define-syntax define-environment
  (syntax-rules ()
    [(_ (syntactic elaboration runtime) tower)
     (define-values (syntactic elaboration runtime)
       (let ((m (stream-first tower)))
         (values (Meta-Environment-syntactic m)
                 (Meta-Environment-elaboration m)
                 (Meta-Environment-runtime m))))]))

(define (unbound name)
  (error (format "Unbound ~A" name)))

(define (lookup-binding name/template bindings [fail unbound])
  (displayln bindings)
  (define result (if (Template? name/template)
                     (assf (λ (x)
                             (match* (name/template x)
                               [((Template prefix1 delimiters1)
                                 (Template prefix2 delimiters2))
                                (and (equal? prefix1 prefix2)
                                     (equal? delimiters1 delimiters2))]
                               [(_ _)
                                #f])))
                     (assoc name/template bindings)))
  (if result
      (cdr result)
      (fail name/template)))

(define (lookup-binding/where name/template namespace environment [fail unbound])
  (match environment
    [(Syntactic-Environment terms patterns types modules)
     (lookup-binding name/template (match namespace
                                     ['terms terms]
                                     ['patterns patterns]
                                     ['types types]
                                     ['modules modules])
                     fail)]))

(define (augment-environment environment namespace name/template binding)
  (define (maybe-augment namespace2 bindings)
    (if (eqv? namespace namespace2)
        (cons (cons name/template binding) bindings)
        bindings))
  (match environment
    [(Syntactic-Environment terms patterns types modules)
     (Syntactic-Environment (maybe-augment 'terms terms)
                            (maybe-augment 'patterns patterns)
                            (maybe-augment 'types types)
                            (maybe-augment 'modules modules))]))

(define (augment-environment! environment namespace name/template binding)
  (define (maybe-augment! namespace2 bindings set-bindings!)
    (when (eqv? namespace namespace2)
      (set-bindings! environment (cons (cons name/template binding) bindings))))
  (match environment
    [(Syntactic-Environment terms patterns types modules)
     (maybe-augment! 'terms terms set-Syntactic-Environment-terms!)
     (maybe-augment! 'terms patterns set-Syntactic-Environment-patterns!)
     (maybe-augment! 'terms types set-Syntactic-Environment-types!)
     (maybe-augment! 'terms modules set-Syntactic-Environment-modules!)]))

(define (augment-tower tower namespace name binding)
  (define-environment (environment elaboration runtime) tower)
  (new-tower (augment-environment environment namespace name binding)
             elaboration
             runtime
             (stream-rest tower)))

(define (augment-tower! tower namespace name binding)
  (define-environment (environment _ __) tower)
  (augment-environment! environment namespace name binding)
  tower)

(define (replace-syntactic tower syntactic)
  (define-environment (environment elaboration runtime) tower)
  (new-tower syntactic elaboration runtime (stream-rest tower)))

(define (augment-tower/list tower namespace names bindings)
  (define-environment (environment elaboration runtime) tower)
  (new-tower (augment-environment/list environment namespace names bindings)
             elaboration
             runtime
             (stream-rest tower)))

(define (augment-environment/list environment namespace names bindings)
  (let loop ([names names]
             [bindings bindings]
             [environment environment])
    (if (null? names)
        environment
        (loop (cdr names)
              (cdr bindings)
              (augment-environment environment namespace (car names) (car bindings))))))

(define (augment-environment/list! environment namespace names bindings)
  (let loop ([names names]
             [bindings bindings]
             [_ #f])
    (if (null? names)
        (void)
        (loop (cdr names)
              (cdr bindings)
              (augment-environment! environment namespace (car names) (car bindings))))))

(define (add-pattern-variables variables environment)
  (augment-environment/list environment 'terms
                            variables (map Leave-Alone variables)))

(define (merge-free-names use-environment captured-environment free-names)
  (match* (use-environment captured-environment free-names)
    [((Syntactic-Environment use-terms use-patterns use-types modules)
      (Syntactic-Environment captured-terms captured-patterns captured-types _)
      (Free-Names free-terms free-patterns free-types))
     (define (merge free use-bindings captured-bindings)
       (map (λ (binding)
              (match binding
                [(cons (Template _ _) _)
                 binding]
                [(cons name meaning)
                 (if (member name free)
                     binding
                     (lookup-binding name captured-bindings))]))
            use-bindings))
     (Syntactic-Environment (merge free-terms use-terms captured-terms)
                            (merge free-patterns use-patterns captured-patterns)
                            (merge free-types use-types captured-types)
                            modules)]))

(define (make-template expression)
  (match expression
    [($ (Prefixed p (Delimited d e)))
     (values (Template p d)
             e)]
    [($ (Prefixed p e))
     (values (Template p #f)
             e)]
    [($ (Delimited d e))
     (values (Template #f d)
             e)]))

(define (bad-declaration _)
  (error "Declaration out of context"))

(define (expand/program text tower)
  (expand/top-level text tower))

(define (expand/top-level text tower)
  (expand/body text tower))

(define (expand/term/list list tower [declaration-handler bad-declaration])
  (map (λ (e)
         (expand/term e tower declaration-handler))
       list))

(define (expand/term expression tower [declaration-handler bad-declaration])
  (match expression
    [(Syntax form captured-environment free-names origin)
     (define-environment (environment _ __) tower)
     (cond [(syntactic-closure? expression)
            (expand/term expression
                         (merge-free-names environment captured-environment free-names)
                         tower
                         declaration-handler)]
           [else
            (match (unwrap form)
              [(Dotted left right)
               (expand/external 'term left right tower)]
              [(Identifier name)
               (define result (lookup-binding/where name 'terms environment (λ (_) #f)))
               (match result
                 [(Transformer procedure definition-environment)
                  (make-syntactic-closure '()
                                          definition-environment
                                          (procedure expression))]
                 [(Rename alias)
                  (make-syntax alias
                               (Syntax-origin expression)
                               environment
                               '())]
                 [else
                  expression])]
              [(or (Label _)
                   (Constant _))
               expression]
              [(or (Prefixed _ _)
                   (Delimited _ _))
               (expand/term (expand/sugar 'term expression tower)
                            tower
                            declaration-handler)]
              [_
               (expand/maybe-combination expression tower #f
                                         declaration-handler)])])]))

(define (transform expression operator operands tower leave-alone k)

  (define-environment (environment _ __) tower)
  (match operator
    [(Transformer procedure definition-environment)
     (make-syntactic-closure '()
                             definition-environment
                             (procedure expression))]
    [(Rename alias)
     (make-syntax (List (cons alias operands))
                  (Syntax-origin expression)
                  environment
                  '())]
    [(Leave-Alone)
     (leave-alone)]
    [#f
     (k)]))

(define (expand/maybe-combination expression tower stop? declaration-handler)
  (displayln (format "e/m-c: ~A, ~A~%" expression stop?))
  (define-environment (environment _ __) tower)
  (define (go operator operands)
    (match operator
      [($ (Dotted left right))
       (go (expand/external 'terms left right tower)
           operands)]
      [($ (External (Module _ _ environment _) name))
       (go (lookup-binding/where name 'terms environment)
           operands)]
      [(not ($ (Identifier _)))
       (if stop?
           expression
           (Call (expand/term operator tower)
                 (expand/term/list operands tower)))]
      [($ (Identifier name))
       (=> oops)
       (define transformed
         (transform expression
                    (lookup-binding/where name 'terms environment (λ (_) #f))
                    operands
                    tower
                    (λ ()
                      (if stop?
                          expression
                          (Call operator (expand/term/list operands tower))))
                    oops))
       (if stop?
           transformed
           (expand/term transformed tower))]
      [_
       (displayln "e/m-c: base case")
       (if stop?
           expression
           (expand/term/primitives expression tower declaration-handler))]))
  (match expression
    [($ (List (list operator operands ...)))
     (go operator operands)]))

(define (expand/term/primitives expression tower declaration-handler)
  (match expression
    [(List (list ($ (Identifier "λ"))
                 (and ($ (Identifier name)) parameters)
                 body ...))
     (Lambda parameters
             (expand/body body (augment-tower tower 'term name (Leave-Alone))))]
    [(List (list ($ (Identifier "λ"))
                 parameters
                 body ...))
     (expand/λ parameters body tower)]
    [(List (list ($ (Identifier "the"))
                 type
                 expression))
     (The (expand/type type tower)
          (expand/term expression tower))]
    [(List (list ($ (Identifier "delimit"))
                 expression1
                 expression2))
     (Delimit (expand/term expression1 tower)
              (expand/term expression2 tower))]
    [(List (list ($ (Identifier "capture-to"))
                 expression1
                 ($ (Identifier name))
                 expression2))
     (Capture-To (Identifier name)
                 (expand/term expression1 tower)
                 (expand/term expression2 (augment-tower tower 'terms
                                                         name (Leave-Alone))))]
    [(List (list ($ (Identifier "record"))
                 rest ...))
     (expand/make-record rest tower)]
    [(List (list ($ (Identifier "tuple"))
                 rest ...))
     (Make-Tuple (expand/term/list rest tower))]
    [(List (list ($ (Identifier "case"))
                 expression
                 clauses ...))
     (expand/case expression clauses tower)]
    [(List (list ($ (Identifier "let"))
                 ($ (List bindings))
                 body ...))
     (expand/let bindings body tower)]
    [(List (list ($ (Identifier "letrec"))
                 ($ (List bindings))
                 body ...))
     (expand/letrec bindings body tower)]
    [(? looks-like-declaration?)
     (declaration-handler expression tower)]))

(define (flatten-path right)
  (displayln (format "flattening: ~V~%" right))
  (match right
    [($ (Identifier _))
     (list right)]
    [($ (Dotted ($ left) right))
     (cons left (flatten-path right))]))

(define (resolve-path path tower)
  (displayln (format "resolving: ~V~%" path))
  (let loop ([path path]
             [module #f])
    (match path
      [(list ($ (Identifier module-name)) rest ...)
       (define-environment (environment _ __) tower)
       (loop rest
             (match (lookup-binding/where module-name 'modules environment)
               [(and (Module _ _ _ _) module)
                module]))]
      ['()
       module]
      [_
       (error "Bad path")])))

(define (expand/external namespace left right tower)
  (define-values (path namelist) (split-at-right (cons left (flatten-path right))
                                             1))
  (define name (car namelist))
  (define module (resolve-path path tower))
  (External module
            (lookup-binding/where name namespace (Module-environment module))))

(define (expand/parameters parameters tower)
  (let loop ([parameters parameters]
             [processed-parameters '()]
             [variables '()])
    (if (null? parameters)
        (values (reverse processed-parameters) (reverse variables))
        (match parameters
          [(list ($ (Identifier name)) ($ (Identifier ":")) type rest ...)
           (loop rest
                 (cons (Annotated (Identifier name) (expand/type type)))
                 (cons name variables))]
          [(list ($ (Identifier name)) rest ...)
           (loop rest
                 (cons (Identifier name) processed-parameters)
                 (cons name variables))]
          [(list (and ($ (List _)) pattern) rest ...)
           (define-values (expanded pattern-variables) (expand/pattern pattern tower))
           (loop rest
                 (cons expanded processed-parameters)
                 (append pattern-variables variables))]))))

(define (expand/λ parameters body tower)
  (define-environment (environment _ __) tower)
  (match parameters
    [($ (Identifier name))
     (Lambda parameters
             (expand/body body (augment-tower tower 'term name (Leave-Alone))))]
    [_
     (define-values (processed variables) (expand/parameters parameters tower))
     (Lambda processed
             (expand/body body (augment-tower/list tower
                                                   'term
                                                   variables
                                                   (map Leave-Alone variables))))]))

(define (expand/make-record things tower)
  (let loop ([items rest]
             [pairs '()])
    (if (null? items)
        (Make-Record (reverse pairs) #f)
        (match items
          [(list (Label name) value rest ...)
           (loop rest
                 (cons (cons (Label name) (expand/term value tower))
                       pairs))]
          [(list (Identifier name))
           (Make-Record pairs (Identifier name))]))))

(define (expand/case expression clauses tower)
  (Case (expand/term expression tower)
        (let loop ([clauses clauses]
                   [built '()])
          (if (null? clauses)
              (reverse built)
              (match (car clauses)
                [(List (list pattern body ...))
                 (define-values (expanded variables) (expand/pattern pattern tower))
                 (define-environment (environment _ runtime) tower)
                 (loop (cdr clauses)
                       (cons (cons expanded
                                   (expand/body body
                                                (new-tower (add-pattern-variables variables
                                                                                  environment)
                                                           runtime)))
                             built))])))))

(define (expand/let bindings body tower)
  (define-environment (environment _ runtime) tower)
  (let loop ([bindings bindings]
             [processed '()]
             [environment environment])
    (define tower´ (new-tower environment runtime))
    (if (null? bindings)
        (Let (reverse processed) (expand/body body tower´))
        (match (car bindings)
          [(List (list (Identifier name) (Identifier ":") type expression))
           (loop (cdr bindings)
                 (cons (cons (Annotated (Identifier name) (expand/type type tower´))
                             (expand/term expression tower))
                       processed)
                 (augment-environment environment 'terms name (Leave-Alone)))]
          [(List (list pattern expression))
           (define-values (expanded variables) (expand/pattern pattern tower´))
           (loop (cdr bindings)
                 (cons (cons expanded (expand/term expression tower)))
                 (add-pattern-variables variables environment))]))))

(define (expand/letrec bindings body tower)
  (define-environment (environment _ runtime) tower)
  (let loop ([bindings bindings]
             [names '()]
             [environment environment])
    (define tower´ (new-tower environment runtime))
    (if (null? bindings)
        (Let-Recursive (map (λ (b)
                              (match b
                                [(cons (or (Identifier name)
                                           (Annotated (Identifier name) _))
                                       expression)
                                 (cons (car b)
                                       (expand/term expression tower´))])))
                       (expand/body body tower´))
        (match (car bindings)
          [($ (List (list ($ (Identifier name)) ($ (Identifier ":")) type expression)))
           (loop (cdr bindings)
                 (cons (cons (Annotated (Identifier name) (expand/type type environment))
                             expression)
                       names)
                 (augment-environment environment 'terms name (Leave-Alone)))]
          [($ (List (list ($ (Identifier name)) expression)))
           (loop (cdr bindings)
                 (cons (cons (Identifier name) expression) names)
                 (augment-environment environment 'terms name (Leave-Alone)))]))))

(define (looks-like-declaration? expression)
  (match expression
    [($ (List (list ($ (Identifier (? (λ (x)
                                        (member x '("define"
                                                    "define/syntax"
                                                    "define/sugar"
                                                    "define/module"
                                                    "define/type"
                                                    "define/data"
                                                    "define/new-type"
                                                    "define/type-syntax"
                                                    "define/type-sugar"
                                                    "define/pattern-syntax"
                                                    "define/pattern-sugar"
                                                    "declare"
                                                    "splice"
                                                    "implicit"
                                                    "import"))))))
                    stuff ...)))
     #t]
    [_ #f]))

(define (expand/sugar namespace expression tower)
  (define-values (template operand)
    (make-template expression))
  (define-environment (environment _ __) tower)
  (transform expression
             (lookup-binding/where template namespace environment)
             #f
             tower
             error
             error))

(define (expand/1 expression tower)
  (define-environment (environment _ runtime) tower)
  (match expression
    [(Syntax form captured-environment free-names origin)
     (cond [(syntactic-closure? expression)
            (expand/1 expression (new-tower (merge-free-names environment
                                                              captured-environment
                                                              free-names)
                                            runtime))]
           [else
            (match (unwrap form)
              [(or (Identifier _)
                   (Label _)
                   (Constant _)
                   (Dotted _ _))
               expression]
              [(or (Prefixed _ _)
                   (Delimited _ _))
               (expand/1 (expand/sugar 'term expression tower))]
              [_
               (expand/maybe-combination expression tower #t
                                         (λ (declaration)
                                           declaration))])])]))

(define (do-rename original alias syntax? namespace)
  (match* (original alias)
    [(($ (Identifier original))
      ($ (Identifier alias)))
     (list (Export alias
                   original
                   namespace
                   syntax?))]
    [(($ (and (Sugar-Template)
              (app make-template original-template)))
      ($ (and (Sugar-Template)
              (app make-template alias-template))))
     (list (Export alias-template
                   original-template
                   namespace
                   #t))]))

(define (process-exports exports prefix)
  (define (maybe-rename ported-name syntax? namespace)
    (match ported-name
      [($ (Identifier name))
       (list (Export (if prefix
                         (string-append prefix name)
                         name)
                     name
                     namespace
                     syntax?))]
      [($ (and (Sugar-Template) (app make-template template)))
       (list (Export template
                     template
                     namespace
                     #t))]
      [($ (List (list ($ (Identifier "rename")) original alias)))
       (do-rename original alias syntax? namespace)]))
  (define (type+terms type-name term-names)
    (append (maybe-rename type-name #f 'types)
            (map (λ (pn)
                   (car (maybe-rename pn #f 'terms)))
                 term-names)))
  (define (go export)
    (match export
      [($ (List (list ($ (Identifier "prefix"))
                      ($ (Identifier prefix))
                      subexports ...)))
       (process-exports subexports prefix)]
      [($ (List (list ($ (Identifier "rename"))
                      original alias)))
       (do-rename original alias #f 'terms)]
      [($ (List (list ($ (Identifier "module"))
                      ($ ported-name))))
       (maybe-rename ported-name #f 'modules)]
      [($ (List (list ($ (Identifier (or "type" "class" "instance")))
                      ($ ported-name))))
       (maybe-rename ported-name #f 'types)]
      [($ (List (list ($ (Identifier "type"))
                      ($ ported-name)
                      ($ (List (list constructors ...))))))
       (type+terms ported-name constructors)]
      [($ (List (list ($ (Identifier "class"))
                      ($ ported-name)
                      ($ (List (list methods ...))))))
       (type+terms ported-name methods)]
      [($ (List (list ($ (Identifier "syntax"))
                      ($ (List (list (list namespace ported-name)))))))
       (maybe-rename ported-name #t (match namespace
                                      [($ (Identifier "type"))
                                       'types]
                                      [($ (Identifier "pattern"))
                                       'patterns]))]
      [($ (List (list ($ (Identifier "syntax"))
                      ($ ported-name))))
       (maybe-rename ported-name #t 'terms)]
      [($ thing)
       (maybe-rename thing #f 'terms)]))
  (match exports
    [(or ($ (List (list))) '())
     '()]
    [(or ($ (List (list export rest ...)))
         (list export rest ...))
     (append (go export)
             (process-exports rest prefix))]))

(define (instantiate-module! module tower)
  (define-values (body module-environment)
    (expand/top-level (Module-body module)
                      tower))
  (set-Module-body! module body)
  (set-Module-environment! module module-environment))

(define (shift-module! module tower)
  (define-environment (_ elaboration runtime) tower)
  (evaluate/module (elaborate/body (Module-body module)
                                   elaboration)
                   runtime))

(define (do-module! name exports body tower)
  (define processed (process-exports exports #f))
  (define-environment (environment _ __) tower)
  (define module (Module (make-syntactic-closure environment '() body)
                         processed
                         #f
                         #f))
  (augment-tower! tower
                  'modules
                  name
                  module)
  (when (match exports
          [($ (List (list exports ...)))
           (ormap (λ (x)
                    (match x
                      [(Export _ _ _ #t)
                       #t]
                      [_
                       #f]))
                  exports)])
    (instantiate-module! module tower)))

(define (make-importer import)
  (match import
    [($ (List (list ($ (Identifier "for-syntax"))
                    item)))
     (make-item-importer item #t)]
    [_
     (make-item-importer import #f)]))

(define (make-item-importer item for-syntax?)
  (match item
    [($ (List (list ($ (Identifier "prefix"))
                    ($ (Identifier prefix))
                    group)))
     (make-group-importer group for-syntax? prefix)]
    [($ (List (list ($ (Identifier "qualified"))
                    ($ path)
                    ($ (Identifier qualifier)))))
     (λ (tower)
       (define-environment (environment _ runtime) tower)
       (define module (resolve-path (flatten-path path) tower))
       (when (and for-syntax? (not (Module-meta module)))
         (shift-module! module tower))
       (augment-environment! (if for-syntax?
                                 runtime
                                 environment)
                             'modules
                             qualifier
                             module))]
    [_
     (make-group-importer item for-syntax? #f)]))

(define (make-group-importer group for-syntax? prefix)
  (match group
    [($ (List (list ($ (Identifier "only"))
                    ($ path)
                    specs ...)))
     (λ (tower)
       ((make-module-importer (resolve-path (flatten-path path) tower)
                              (make-filter specs #t)
                              for-syntax?
                              prefix)
        tower))]
    [($ (List (list ($ (Identifier "except"))
                    ($ path)
                    specs ...)))
     (λ (tower)
       ((make-module-importer (resolve-path (flatten-path path) tower)
                              (make-filter specs #t)
                              for-syntax?
                              prefix)
        tower))]
    [($ (and path (or (Dotted _ _)
                      (Identifier _))))
     (λ (tower)
       ((make-module-importer (resolve-path (flatten-path path) tower)
                              identity
                              for-syntax?
                              prefix)

        tower))]))

(define (make-module-importer module filter for-syntax? prefix)
  (define environment (if for-syntax?
                          (Module-meta module)
                          (Module-environment module)))
  (define exports
    (map (λ (export)
           (match export
             [(Export external-name internal-name namespace _)
              (list namespace external-name (Rename (make-syntax (External module internal-name)
                                                                 #f)))]))
         (Module-exports module)))
  (λ (tower)
    (define-environment (environment elaboration runtime) (if for-syntax?
                                                              tower
                                                              (stream-rest tower)))
    (map (λ (export)
           (match export
             [#f #f] ; removed by filter
             [(list namespace external-name binding)
              (augment-environment! environment
                                    namespace
                                    (if prefix
                                        external-name
                                        (string-append prefix external-name))
                                    binding)]))
         (map filter exports))
    tower))

(struct Decoded
  (flip?
   alias
   original
   namespace))

(define (decode xname namespace)
  (define (maybe-rename thing flip?)
    (match thing
      [(or ($ (Identifier name))
           ($ (and (Sugar-Template) (app make-template name))))
       (Decoded flip? name name namespace)]
      [($ (List (or (list ($ (Identifier "rename"))
                          ($ (Identifier original))
                          ($ (Identifier alias)))
                    (list ($ (Identifier "rename"))
                          ($ (and (Sugar-Template) (app make-template original)))
                          ($ (and (Sugar-Template) (app make-template alias)))))))
       (Decoded flip? original alias namespace)]))
  (match xname
    [($ (List (list ($ (Identifier "flip"))
                    rename?)))
     (maybe-rename rename? #t)]
    [else
     (maybe-rename xname #f)]))

(define (gather-decoded spec)
  (match spec
    [($ (List (list ($ (Identifier (or "type" "class" "instance")))
                    xname)))
     (list (decode xname 'types))]
    [($ (List (list ($ (Identifier (or "type" "class")))
                    xname
                    ($ (List (list xnames ...))))))
     (list (decode xname 'types)
           (map (λ (x) (decode x 'terms))
                xnames))]
    [($ (List (list ($ (Identifier "module"))
                    xname)))
     (list (decode xname 'modules))]
    [($ (List (list ($ (Identifier "pattern"))
                    xname)))
     (list (decode xname 'patterns))]))

(define (make-filter specs keep?)
  (define decoded (foldr append '() (map gather-decoded specs)))
  (λ (export)
    (match export
      [(list namespace external-name binding)
       (define found (findf (λ (d)
                              (match d
                                [(Decoded flip? original alias namespace2)
                                 (and (eqv? namespace namespace2)
                                      (not flip?)
                                      (equal? original external-name)
                                      d)]))
                            decoded))
       (if found
           (if keep?
               (list namespace (Decoded-alias found) binding)
               export)
           (if keep?
               #f
               export))])))

(define (do-import! imports tower)
  (map (λ (import)
         ((make-importer import) tower))
       imports)
  tower)

(define (do-macro! namespace name/template expression tower)
  (define expanded (expand/term expression (stream-rest tower)))
  (define-environment (environment elaboration runtime) tower)
  (define-values (elaborated context) (elaborate/transformer expanded elaboration))
  (augment-environment! environment namespace
                        name/template (Transformer (evaluate elaborated runtime)
                                                   environment))
  (stream-cons (Meta-Environment environment context runtime)
               (stream-rest tower)))

(define (defining-keyword->namespace keyword)
  (match keyword
    [(or "define/syntax" "define/sugar") 'terms]
    [(or "define/type-syntax" "define/type-sugar") 'types]
    [(or "define/pattern-syntax" "define/pattern-sugar") 'pattern]))

(define (expand/body body tower)
  (define (pass-one)
    (let loop ([items (match body
                        [($ (List items))
                         items]
                        [(list items ...)
                         items])]
               [expanded '()]
               [tower´ tower])
      (define-environment (environment _ __) tower´)
      (match items
        [(list)
         (pass-two (reverse expanded) '() tower´)]
        [(list declaration? rest ...)
         (define tentative (expand/1 declaration? tower))
         (displayln (format "tentative = ~A~%" tentative))
         (cond [(looks-like-declaration? tentative)
                (match tentative
                  [($ (List (list ($ (Identifier "splice"))
                                  subdeclarations ...)))
                   (loop (append subdeclarations items)
                         expanded
                         tower´)]
                  [($ (List (list ($ (Identifier "import"))
                                  imports ...)))
                   (loop rest
                         expanded
                         (do-import! imports tower´))]
                  [($ (List (or (list ($ (Identifier (and operator
                                                          (or "define/syntax"
                                                              "define/type-syntax"
                                                              "define/pattern-syntax"))))
                                      ($ (Identifier name))
                                      expression)
                                (list ($ (Identifier (and operator
                                                          (or "define/sugar"
                                                              "define/type-sugar"
                                                              "define/pattern-sugar"))))
                                      (and (app make-template name) (Sugar-Template))
                                      expression))))
                   (loop rest
                         expanded
                         (do-macro! (defining-keyword->namespace operator)
                                    name
                                    expression
                                    tower´))]
                  [($ (List (list ($ (Identifier "define/module"))
                                  ($ (Identifier name))
                                  exports
                                  body ...)))
                   (displayln (stream-first tower´))
                   (do-module! name exports body tower´)
                   (loop rest
                         (cons tentative expanded)
                         tower´)]
                  [else
                   (loop (cdr items)
                         (cons tentative expanded)
                         tower´)])]
               [else
                (pass-two (reverse expanded)
                          rest
                          tower´)])])))
  (define (pass-two declarations expressions tower)
    (let loop ([declarations declarations]
               [expanded '()]
               [tower tower])
      (if (null? declarations)
          (Body expanded (expand/term/list expressions tower))
          (let-values ([(declaration tower) (expand/declaration (car declarations) tower)])
            (loop (cdr declarations)
                  (cons declaration expanded)
                  tower)))))
  (pass-one))

(define (expand/maybe-type-body mtb tower)
  (match mtb
    [($ (List (list ($ (Identifier ":"))
                    type
                    body ...)))
     (values (expand/type type tower)
             (expand/define-body body tower))]))

(define (expand/define-body body tower)
  (let loop ([lambdas '()]
             [body body])
    (if (null? body)
        (reverse lambdas)
        (match (car body)
          [($ (List (list ($ (Identifier "λ"))
                          parameters
                          λbody ...)))
           (loop (cons (expand/λ parameters λbody tower) lambdas)
                 (cdr body))]
          [else
           (Body (append lambdas (expand/term/list body tower)))]))))

(define (expand/declaration declaration tower)
  (match declaration
    [($ (List (list ($ (Identifier "implicit"))
                    things ...)))
     (values (Implicit (map (λ (t)
                              (expand/type t tower))
                            things))
             tower)]
    [($ (List (list ($ (Identifier "define"))
                    ($ (Identifier name))
                    maybe-type-body ...)))
     (define-values (type body) (expand/maybe-type-body maybe-type-body tower))
     (values (Define (Identifier name)
                     type
                     body)
             tower)]
    [($ (List (list ($ (Identifier "define"))
                    ($ (List (list ($ (Identifier name))
                                   parameters ...)))
                    maybe-type-body ...)))
     (define-values (processed variables) (expand/parameters parameters tower))
     (define tower´ (augment-tower/list tower
                                        'terms
                                        variables
                                        (map Leave-Alone variables)))
     (define-values (type body) (expand/maybe-type-body maybe-type-body tower´))
     (values (Define (Identifier name)
                     type
                     (Lambda processed body))
             (augment-tower tower 'term name (Leave-Alone)))]
    [($ (List (list ($ (Identifier "define/type"))
                    type-name
                    type)))
     (define-values (name etc. variables) (uncover-type-name type-name tower))
     (define tower´ (augment-tower tower 'types name (Leave-Alone)))
     (values (Define-Type (Identifier name)
                          etc.
                          (expand/type type (augment-tower/list tower´ 'types
                                                                variables
                                                                (map Leave-Alone variables))))
             tower´)]
    [($ (List (list ($ (Identifier "define/new-type"))
                    type-name
                    ($ (List (list ($ (Identifier constructor))
                                   types ...))))))
     (define-values (name etc. variables) (uncover-type-name type-name tower))
     (values (Define-New-Type (Identifier name)
                              etc.
                              (expand/type/arguments types
                                                     (augment-tower/list tower 'types
                                                                         variables
                                                                         (map Leave-Alone
                                                                              variables))))
             (augment-tower tower 'types name (Leave-Alone)))]
    [($ (List (list ($ (Identifier "define/data"))
                    type-name
                    clauses ...)))
     (define-values (name etc. variables) (uncover-type-name type-name tower))
     (define-values (expanded constructors)
       (expand/data-clauses clauses (augment-tower/list tower 'types
                                                        variables (map Leave-Alone variables))))
     (define leaves (map Leave-Alone constructors))
     (define with-patterns (augment-tower/list tower 'patterns
                                               constructors leaves))
     (define with-functions (augment-tower/list with-patterns 'terms
                                                constructors leaves))
     (values (Define-Data (Identifier name)
                          etc.
                          expanded)
             (augment-tower with-functions 'types
                            name (Leave-Alone)))]
    [($ (List (list ($ (Identifier "define/class"))
                    type-name
                    items ...)))
     (define-values (name etc. variables) (uncover-type-name type-name tower))
     (define-values (expanded methods) (expand/class-body items tower))
     (define with-class (augment-tower tower 'types
                                       name (Leave-Alone)))
     (values (Define-Class (Identifier name)
                           etc.
                           expanded)
             (augment-tower/list with-class 'terms
                                 methods (map Leave-Alone methods)))]
    [($ (List (list ($ (Identifier "define/instance"))
                    type-name
                    items ...)))
     (define-values (name etc. variables) (uncover-type-name type-name tower))
     (define expanded (expand/body items tower))
     (values (Define-Instance (Identifier name)
                              etc.
                              expanded)
             (augment-tower tower 'types
                            name (Leave-Alone)))]
    [($ (List (list ($ (Identifier "define/module"))
                    ($ (Identifier name))
                    exports
                    body ...)))
     (values (Define-Module (Identifier name)
                            (process-exports exports #f)
                            (expand/body body tower))
             tower)]
    [($ (List (list ($ (Identifier "declare"))
                    metadata ...)))
     (values (Declare metadata)
             tower)]))

(define (expand/data-clauses clauses tower)
  4)

(define (expand/class-body items tower)
  4)

(define (uncover-type-name type-name tower)
  4)

(define (expand/type type tower)
  (match type
    [(Syntax form captured-environment free-names origin)
     (define-environment (environment _ __) tower)
     (cond [(syntactic-closure? type)
            (expand/type type
                         (replace-syntactic tower
                                            (merge-free-names environment
                                                              captured-environment
                                                              free-names)))]
           [else
            (match (unwrap form)
              [(Dotted left right)
               (expand/external 'type left right tower)]
              [(Identifier name)
               (define result (lookup-binding/where name 'types environment (λ (_) #f)))
               (match result
                 [(Transformer procedure definition-environment)
                  (make-syntactic-closure '()
                                          definition-environment
                                          (procedure type))]
                 [(Rename alias)
                  (make-syntax alias
                               (Syntax-origin type)
                               environment
                               '())]
                 [(Leave-Alone)
                  type]
                 [else
                  (augment-environment! environment 'types name (Leave-Alone))
                  type])]
              [(or (Prefixed _ _)
                   (Delimited _ _))
               (expand/term (expand/sugar type type tower)
                            tower)]
              [(List (list (and operator ($ (Identifier name))) operands ...))
               (=> oops)
               (transform type
                          (lookup-binding/where name 'types environment)
                          operands
                          tower
                          (λ ()
                            (Type-Application operator
                                              (expand/type/arguments operands tower)))
                          oops)]
              [_
               (expand/type/primitive-or-application type tower)])])]))

(define (expand/type/primitive-or-application type tower)
  (match type
    [($ (List (list ($ (Identifier "⇒"))
                    constraints ...
                    type)))
     (define-values (expanded tower´) (expand/constraints constraints tower))
     (Constrained constraints (expand/type type tower´))]
    [($ (List (list ($ (Identifier "∀"))
                    ($ (List parameters))
                    type)))
     (define-values (expanded tower´) (expand/type/parameters parameters tower))
     (For-All expanded
              (expand/type type
                           tower´))]
    [($ (List (list ($ (Identifier "record"))
                    labels ...)))
     (define-values (expanded variable) (expand/type/record labels tower))
     (when variable
       (augment-tower! tower 'types variable (Leave-Alone)))
     expanded]
    [($ (List (list operator operands ...)))
     (Type-Application (expand/type operator tower)
                       (expand/type/arguments operands tower))]))

(define (expand/type/arguments arguments tower)
  (map (λ (argument)
         (match argument
           [($ (Left-Double-Arrow t))
            (Instance-Supply (expand/type t tower))]
           [($ type)
            (expand/type type tower)]))
       arguments))

(define (expand/type/record types tower)
  (let loop ([expanded '()]
             [types types])
    (match types
      [(list)
       (values expanded #f)]
      [(list ($ (Identifier name)))
       (values expanded name)]
      [(list ($ (and (Label name) label)) type rest ...)
       (loop (cons (cons label (expand/type type tower)) expanded)
             rest)])))

(define (expand/type/parameters parameters tower)
  (let loop ([expanded '()]
             [parameters parameters]
             [tower tower])
    (if (null? parameters)
        (values (reverse expanded) tower)
        (match parameters
          [(list ($ (Identifier name)) ($ (Identifier ":")) ($ type) rest ...)
           (loop (cons (Annotated (Identifier name) (expand/type type tower))
                       expanded)
                 rest
                 (augment-tower tower 'types name (Leave-Alone)))]
          [(list ($ (Identifier name)) rest ...)
           (augment-tower! tower 'types name (Leave-Alone))
           (loop (cons (Identifier name) expanded)
                 rest
                 (augment-tower tower 'types name (Leave-Alone)))]))))

(define (expand/constraints constraints tower)
  (let loop ([expanded '()]
             [constraints constraints]
             [tower tower])
    (if (null? constraints)
        (values (reverse expanded) tower)
        (match constraints
          [(list ($ (Identifier name)) ($ (Identifier ":")) ($ type) rest ...)
           (loop (cons (Annotated (Identifier name) (expand/type type tower))
                       expanded)
                 rest
                 (augment-tower tower 'types name (Leave-Alone)))]
          [(list ($ type) rest ...)
           (loop (cons (expand/type type tower) expanded)
                 rest
                 tower)]))))

(define (expand/pattern pattern tower)
  (match pattern
    [(Syntax form captured-environment free-names origin)
     (define-environment (environment _ __) tower)
     (cond [(syntactic-closure? pattern)
            (expand/pattern pattern
                            (replace-syntactic tower
                                               (merge-free-names environment
                                                                 captured-environment
                                                                 free-names)))]
           [else
            (match (unwrap form)
              [(Dotted left right)
               (expand/external 'patterns left right tower)]
              [(Identifier name)
               (define result (lookup-binding/where name 'patterns environment (λ (_) #f)))
               (match result
                 [(Transformer procedure definition-environment)
                  (expand/pattern (make-syntactic-closure '()
                                                          definition-environment
                                                          (procedure pattern))
                                  tower)]
                 [(Rename alias)
                  (expand/pattern (make-syntax alias
                                               (Syntax-origin pattern)
                                               environment
                                               '())
                                  tower)]
                 [(Leave-Alone)
                  (values pattern '())]
                 [_
                  (values pattern (list (unwrap form)))])]
              [(or (Prefixed _ _)
                   (Delimited _ _))
               (expand/pattern (expand/sugar 'patterns pattern tower)
                               tower)]
              [(List (list ($ (and operator (Identifier name))) operands ...))
               (=> oops)
               (transform pattern
                          (lookup-binding/where name 'patterns environment)
                          operands
                          tower
                          (λ ()
                            (expand/constructor-pattern operator operands tower))
                          oops)]
              [_
               (expand/pattern/primitives pattern tower)])])]))

(define (expand/constructor-pattern operator operands tower)
  (let loop ([operands operands]
             [expanded '()]
             [variables '()])
    (if (null? operands)
        (values (Constructor-Pattern operator (reverse operands))
                variables)
        (let-values ([(e v) (expand/pattern (car operands) tower)])
          (loop (cdr operands)
                (cons e expanded)
                (set-union v variables))))))

(define (expand/pattern/primitives pattern tower)
  (match pattern
    [($ (List (list ($ (Identifier "record"))
                    labels ...)))
     (expand/pattern/record labels tower)]
    [($ (List (list ($ (Identifier "as"))
                    ($ (and v (Identifier name)))
                    pattern)))
     (define-values (expanded variables) (expand/pattern pattern))
     (values (As v expanded)
             (cons v variables))]
    [($ (List (list ($ (Identifier "where"))
                    pattern1
                    pairs ...)))
     (error "Todo")]    
    [($ (List (list operator operands ...)))
     (expand/constructor-pattern operator operands tower)]))

(define (expand/pattern/record labels tower)
  4)