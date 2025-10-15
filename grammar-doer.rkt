#lang racket

(require brag/support)
(require "grammar.rkt")
(require html-printer)

(define (unescape s)
  (regexp-replace* #rx"\\\\(.)" s "\\1"))

(define arrow (make-parameter "::="))
(define bar (make-parameter "|"))

(define (tokenize in)
  (port-count-lines! in)
  (define-lex-trans except-escaped
    (syntax-rules ()
      [(_ x)
       (:+ (:or (:~ x) (:: "\\" any-char)))]))
  (define-lex-abbrev metasyntax
    (:or "⟦" "⟧" "\"" "|" ";" "→" "∨" "…" "'" "[" "]" "{" "}" "(" ")" "," "\\" whitespace))
  (define-lex-abbrev symbol
    (except-escaped metasyntax))
  (define the-lexer
    (lexer-src-pos (","
                    (token ","))
                   ("'"
                    (token "'"))
                   (";"
                    (token ";"))
                   ("→"
                    (token "→"))
                   ("…+"
                    (token "…+" lexeme))
                   ("…"
                    (token "…" lexeme))
                   ("'"
                    (token "'"))
                   ("⟨"
                    (token "⟨"))
                   ("⟩"
                    (token "⟩"))
                   ("|"
                    (token "|"))
                   ("∨"
                    (token "∨"))
                   ("^"
                    (token "^"))
                   ("*"
                    (token "*"))
                   ("+"
                    (token "+"))
                   ("{"
                    (token "{"))
                   ("}"
                    (token "}"))
                   ("["
                    (token "["))
                   ("]"
                    (token "]"))
                   ("("
                    (token "("))
                   (")"
                    (token ")"))
                   ((:: "\"" (except-escaped "\"") "\"")
                    (token 'DOUBLE-QUOTED (trim-ends "\"" (unescape lexeme) "\"")))
                   ((:: "⟦" (except-escaped "⟧") "⟧")
                    (token 'DOUBLE-BRACKET (trim-ends "⟦" lexeme "⟧")))
                   (whitespace
                    (token 'WHITESPACE lexeme #:skip? #t))
                   (symbol
                    (token 'SYMBOL (unescape lexeme)))))
  (λ () (the-lexer in)))

(define (parse-grammar p)
  (let ([parsed (parse-to-datum p)])
    (values parsed (gather-nonterminals parsed))))

(define (gather-nonterminals grammar)
  (define (go thing)
    (match thing
      [`(section ,_ ,items ...)
       (map go items)]
      [`(rule ,name ,_ ...)
       (define-values (real-name _) (split-name name))
       real-name]))
  (match grammar
    [`(grammar ,sections ...)
     (flatten (map go sections))]))

(define (pretty-grammar grammar nonterminals)
  (match grammar
    [`(grammar ,items ...)
     `(html (head (meta [[charset "UTF-8"]])
                  (title "Nomlisp grammar")
                  (link [[rel "stylesheet"]
                         [href "gram.css"]])
                  (link [[rel "preconnect"]
                         [href "https://fonts.googleapis.com"]
                         [crossorigin ""]])
                  (link [[rel "preconnect"]
                         [href "https://fonts.gstatic.com"]
                         [crossorigin ""]])
                  (link [[href "https://fonts.googleapis.com/css2?family=Cardo:ital,wght@0,400;0,700;1,400&family=IBM+Plex+Serif:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap"]
                         [rel "stylesheet"]])
                  (link [[href "https://fonts.googleapis.com/css2?family=Cardo:ital,wght@0,400;0,700;1,400&family=IBM+Plex+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&family=IBM+Plex+Serif:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap"]
                         [rel "stylesheet"]]))
            (body (h1 "Nomlisp grammar")
                  ,@(pretty-items items 1 nonterminals)))]))

(define (heading-symbol level)
  (case level
    [(1) 'h2]
    [(2) 'h3]
    [(3) 'h4]
    [(4) 'h5]
    [(5) 'h6]
    [else 'h6]))

(define (pretty-items items level nonterminals)
  (let loop ([items items]
             [translated '()])
    (if (null? items)
        (reverse translated)
        (match items
          [`((section ,name . ,stuff) . ,rest)
           (loop (cdr items)
                 (cons `(section (,(heading-symbol level) ,name)
                                 ,@(pretty-items stuff (+ level 1) nonterminals))
                       translated))]
          [`((rule . ,_) . ,rest)
           (let inner ([items rest]
                       [rules '()])
             (match items
               [(or (list)
                    (list-rest`(section . ,_) _))
                (loop items
                      (cons `(dl [[class "rule-sequence"]]
                                 ,@(pretty-rules (reverse rules) nonterminals))
                            translated))]
               [(list-rest `(rule . ,_) _)
                (inner (cdr items)
                       (cons (car items) rules))]))]))))

(define (split-name name)
  (define split (string-split name "_"))
  (values (car split) (and (not (null? (cdr split))) (cadr split))))

(define (pretty-rules rules nonterminals)
  (append-map (λ (rule)
                (match rule
                  [`(rule ,name ,alternatives ...)
                   (define-values (real-name _) (split-name name))
                   `((dt [[class "rule"]
                          [id ,(format "~A-definition" real-name)]]
                         ,(pretty-nonterminal name))
                     (div [[class "alternatives"]]
                          ,@(pretty-alternatives alternatives nonterminals)))]))
              rules))

(define (pretty-nonterminal name)
  (define-values (real-name subscript) (split-name name))
  `(span [[class "nonterminal"]]
         (a [[href ,(format "#~A-definition" real-name)]]
            ,real-name)
         ,@(if subscript
               `((sub ,subscript))
               '())))

(define (pretty-alternatives alternatives nonterminals)
  (for/list ([alternative alternatives]
             [i (in-naturals)])
    (define separator (if (= i 0)
                          '(div [[class "arrow"]] "::=")
                          '(div [[class "bar"]] "|")))
    (match alternative
      [`(alternative ,stuff ... (comment ,text))
       `(dd [[class "alternative"]]
            ,separator
            (div [[class "expansion"]]
                 ,@(pretty-expansion stuff nonterminals #f))
            (div [[class "comment"]] ,@(pretty-element text nonterminals #f)))]
      [`(alternative . ,stuff)
       `(dd [[class "alternative"]]
            ,separator
            (div [[class "expansion"]]
                 ,@(pretty-expansion stuff nonterminals #f)))])))

(define (enlist-strings xs)
  xs)

(define (pretty-expansion stuff nonterminals more?)
  (define (go xs)
    (map ))
  (map (λ (x)
         (match x
           [(list (? string?))
            (car x)]
           [else
            x]))
       (let loop ([rest stuff]
                  [result '()])
         (match rest
           [(list)
            (reverse result)]
           [(list a)
            (loop (cdr rest)
                  (append (reverse (pretty-element a nonterminals more?))
                          result))]
           [(list-rest a _ _)
            (loop (cdr rest)
                  (append (reverse (pretty-element a nonterminals #t))
                          result))]))))

(define (pretty-element element nonterminals more?)
  (define (space-if-more x)
    (append (list x) (if more? '(" ") '(""))))
  (space-if-more (match element
                   [`(symbol ,name)
                    (define-values (real-name _) (split-name name))
                    (if (member real-name nonterminals)
                        (pretty-nonterminal name)
                        `(span [[class "literal"]] ,name))]
                   [`(quoted ,name)
                    `(span [[class "literal"]] ,name)]
                   [`(list . ,stuff)
                    `(span (span [[class "literal"]] "(")
                           ,@(pretty-expansion stuff nonterminals #f)
                           (span [[class "literal"]] ")"))]
                   [`(double-bracket ,text)
                    `(span [[class "natural-language"]]
                           ,@(pretty-brackets text))]
                   [`(postfix ,thing ,operator)
                    `(span ,@(pretty-element thing nonterminals #f)
                           (span [[class "quantification"]] ,operator))]
                   [`(optional . ,stuff)
                    `(span [[class "optional"]]
                           (span [[class "open-bracket"]] "[")
                           ,@(pretty-expansion stuff nonterminals #f)
                           (span [[class "close-bracket"]] "]"))]
                   [`(or . ,operands)
                    `(span ,@(append-map (λ (o1 o2)
                                           (displayln (cons o1 o2))
                                           (if o2
                                               (append (pretty-element o1 nonterminals #t)
                                                     (list "| "))
                                               (pretty-element o1 nonterminals #f)))
                                         operands
                                         (append (cdr operands) (list #f))))]
                   [`(group . ,stuff)
                    `(span [[class "group"]]
                           (span [[class "open-brace"]] "{")
                           ,@(pretty-expansion stuff nonterminals #f)
                           (span [[class "close-brace"]] "}"))]
                   [_
                    (printf "what ~A~%" element)])))

(define (pretty-brackets text)
  (let ([port (open-input-string text)])
    (let loop ([elements '()])
      (define c (read-char port))
      (match c
        [(? eof-object?)
         (reverse elements)]
        [#\$
         (loop (cons (read port) elements))]
        [_
         (loop (cons (string c) elements))]))))

#;(define xexpr->html5 pretty-print)

(define (do-grammar name in out)
  (define-values (grammar nonterminals) (parse-grammar (tokenize in)))
  (write-string (xexpr->html5 (pretty-grammar grammar nonterminals)
                              #:wrap 500)
                out)
  (pretty-print grammar))

(define (do-grammar-file in-name out-name)
  (call-with-input-file in-name
    (λ (in)
      (call-with-output-file out-name
        (λ (out)
          (do-grammar in-name in out))
        #:exists 'replace))))

(do-grammar-file "nom.gram" "nom-gram.html")