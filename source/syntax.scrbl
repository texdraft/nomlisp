#lang scribble/lp2/manual

@require[@for-label[(except-in scribble/lp2/manual #%module-begin)
                    scribble/manual
                    racket/control
                    racket/match
                    syntax/parse
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Syntax}

Here we define data structures to represent Nomlisp code.

@chunk[<Syntax>
       (module Syntax racket
         (provide <provisions/syntax>)
         (require <requirements/syntax>)
         <syntax>)]

Nomlisp has a Racket-like expansion model, so we really need
(at least) @emph{two} representations, one concrete and one
abstract. The concrete syntax may get mapped into Nomlisp
objects for macros, but the abstract syntax is only for
internal use after expansion.

@chunk[<syntax>
       <abstract>
       <concrete>]

@section{Names}

@chunk[<syntax>
       (struct Symbol
         (name)
         #:transparent)]

@chunk[<provisions/syntax>
       (struct-out Symbol)]

@chunk[<syntax>
       (define oblist (make-hash))
       (define (intern name)
         (hash-ref oblist name
                   (λ ()
                     (define s (Symbol name))
                     (hash-set! oblist name s)
                     s)))
       (define generate-symbol
         (let ([counter 0])
           (λ (prefix)
             (Symbol prefix))))]

@section{Abstract syntax}

After expansion the CST can be parsed into an AST
representing a fully-expanded program. Elaboration will
transform this AST further.

Actually, the AST is not quite as ‘A’ as it could be,
because some syntactic sugar remains. Function applications
have type-directed treatment, which must happen during type
checking.

@chunk[<abstract>
       (struct Node
         ())

       (struct Expression Node ())
       <expression-syntax>

       (struct Type Node ())
       <type-syntax>

       (struct Pattern Node ())
       <pattern-syntax>

       <post-elaboration-syntax>]

@subsection{Type trees}

@chunk[<type-syntax>
       (struct Type-Name Type
         (name))

       (struct External-Type Type
         (module name))]

Modules end up represented as environment objects at
runtime; for type checking, we need to know the types of the
bindings in that environment.

@chunk[<type-syntax>
       (struct Module-Type Type
         (types))]

@chunk[<type-syntax>
       (struct Type-Application Type
         (type
          argument))]

@chunk[<type-syntax>
       (struct For-All Type
         (name
          type))]

@chunk[<type-syntax>
       (struct Tuple-Type Type
         (types))]

@chunk[<type-syntax>
       (struct Constrained-Type Type
         (constraints
          type))

       (struct Constraint Type
         (name
          types))]

@chunk[<provisions/syntax>
       (struct-out Type-Name)
       (struct-out External-Type)
       (struct-out Module-Type)
       (struct-out Type-Application)
       (struct-out For-All)
       (struct-out Tuple-Type)
       (struct-out Constrained-Type)
       (struct-out Constraint)]

@subsection{Expression trees}

@chunk[<expression-syntax>
       (struct Variable Expression
         (name))

       (struct External-Variable Expression
         (module name))]

@chunk[<expression-syntax>
       (struct Constant Expression
         (value))

       (struct Unit Expression
         ())]

@chunk[<expression-syntax>
       (struct Lambda Expression
         (parameter-thing
          body))

       (struct Application Expression
         (function
          arguments))]

@chunk[<expression-syntax>
       (struct The Expression
         (expression
          type))]

@chunk[<expression-syntax>
       (struct Delimit Expression
         (expression))

       (struct Capture Expression
         (name expression))

       (struct Sequence
         (expressions))]

@chunk[<expression-syntax>
       (struct Primitive Expression
         (name))]

@chunk[<expression-syntax>
       (struct Tuple Expression
         (components))]

@chunk[<expression-syntax>
       (struct Case Expression
         (scrutinee
          cases))]

@chunk[<expression-syntax>
       (struct Let Expression
         (bindings
          body))

       (struct Let-Rec Expression
         (bindings
          body))]

@chunk[<provisions/syntax>
       (struct-out Variable)
       (struct-out External-Variable)
       (struct-out Constant)
       (struct-out Unit)
       (struct-out Lambda)
       (struct-out Application)
       (struct-out The)
       (struct-out Delimit)
       (struct-out Capture)
       (struct-out Sequence)
       (struct-out Primitive)
       (struct-out Tuple)
       (struct-out Case)
       (struct-out Let)
       (struct-out Let-Rec)]

@subsection{Patterns}

@chunk[<pattern-syntax>
       (struct Variable-Pattern Pattern
         (name))

       (struct Constant-Pattern Pattern
         (value))

       (struct Tuple-Pattern Pattern
         (components))]

@chunk[<pattern-syntax>
       (struct Constructor-Pattern Pattern
         (name
          arguments))

       (struct External-Constructor-Pattern Pattern
         (name
          arguments))

       (struct Record-Pattern Pattern
         (fields))]

@chunk[<provisions/syntax>
       (struct-out Variable-Pattern)
       (struct-out Constant-Pattern)
       (struct-out Tuple-Pattern)
       (struct-out Constructor-Pattern)
       (struct-out External-Constructor-Pattern)
       (struct-out Record-Pattern)]

@subsection{Beyond the surface language}

After expansion and elaboration some new expression trees
appear.

@chunk[<post-elaboration-syntax>
       (struct Lambdas Expression
         (lambdas))

       (struct Capture-Environment Expression
         ())

       (struct LetRec/Types/Data Expression
         (bindings
          body))]

@chunk[<provisions/syntax>
       (struct-out Lambdas)
       (struct-out Capture-Environment)
       (struct-out LetRec/Types/Data)]

@section{Concrete syntax}

In order to support hygienic macros, the expander works with
the input text in the form of syntax objects, rather than
mere trees of atoms. A syntax object combines the underlying
s-expression, the “datum”, with some metadata that might be
of use to programs manipulating Nomlisp code.

Racket is so kind as to provide location tracking for ports,
which we gladly take advantage of. The @racket[Location]
structure is a reification of the results of
@racket[port-next-location], plus a field for the file path.

@chunk[<concrete>
       (struct Location
         (line column position path))

       (struct Syntax
         (datum
          scopes
          location)
         #:transparent)]

@chunk[<provisions/syntax>
       (struct-out Syntax)
       (struct-out Location)]

But what's in a datum? Nomlisp's syntax has a few elements
that aren't present in Racket, so we don't use Racket pairs
or symbols alone.

@chunk[<concrete>
       (struct Datum
         ()
         #:transparent)

       (struct Syntax-At Datum
         (datum))

       (struct Syntax-Cons Datum
         (data))

       (struct Syntax-Nil Datum
         ())

       (struct Syntax-Symbol Datum
         (name))

       (struct Syntax-Label Datum
         (name))

       (struct Syntax-Literal Datum
         (value))

       (struct Syntax-Delimited Datum
         (delimiter name data))

       (struct Syntax-Qualified Datum
         (qualfiers name))]

@chunk[<provisions/syntax>
       (struct-out Datum)
       (struct-out Syntax-At)
       (struct-out Syntax-Cons)
       (struct-out Syntax-Nil)
       (struct-out Syntax-Symbol)
       (struct-out Syntax-Label)
       (struct-out Syntax-Literal)
       (struct-out Syntax-Delimited)
       (struct-out Syntax-Qualified)]

@chunk[<requirements/syntax>
       (submod "data.scrbl" Data)]

@subsection{Unparsing CSTs}

@chunk[<concrete>
       (define (show-symbol symbol)
         (match symbol
           [(Symbol name)
            name]))

       (define (show-delimited-list open close data)
         (define port (open-output-string))
         (write-char open port)
         (let loop ([datum (first data)]
                    [data (rest data)])
           (write-string (show datum) port)
           (if (null? data)
               (write-char close port)
               (begin (write-char #\space port)
                      (loop (first data)
                            (rest data)))))
         (get-output-string port))

       (define (show-datum datum)
         (match datum
           [(Syntax-At datum)
            (format "@~A" (show-datum datum))]
           [(Syntax-Cons data)
            (show-delimited-list #\( #\) data)]
           [(Syntax-Nil)
            "()"]
           [(Syntax-Symbol name)
            (show-symbol name)]
           [(Syntax-Label name)
            (show-symbol name)]
           [(Syntax-Literal value)
            (format "~A" value)]
           [(Syntax-Delimited (regexp #rx"(.)(.)" (list _ open-string close-string)) name data)
            (define open (string-ref open-string 0))
            (define close (string-ref close-string 0))
            (if name
                (format "#~A~A" (show name) (show-delimited-list open close data))
                (format "#~A~A" (show name) (show-delimited-list open close data)))]
           [(Syntax-Qualified qualifiers name)
            (format "~A.~A"
                    (string-join (map show-symbol qualifiers) ".")
                    (show-symbol name))]))

       (define (show-syntax syntax)
         (match syntax
           [(Syntax datum _ _)
            (show-datum datum)]))

       (define (show thing)
         (if (Syntax? thing)
             (show-syntax thing)
             (show-datum thing)))]

@chunk[<provisions/syntax>
       show-datum
       show-syntax
       show]

@subsection{The reader}

OK, we can represent s-expressions, if we could only get
them into the system in the first place. Here is a quick
reader for Nomlisp.

@chunk[<concrete>
       <reader>]

Racket has @racket[read] and @racket[read-syntax], the
latter of which preserves location data. For our purposes,
we always want the locations, so our basic @code{read}
returns a syntax object unless finding @racket[eof].

@chunk[<reader>
       (define (read in)
         (define (grab-location)
           (call-with-values (λ ()
                               (port-next-location in))
                             (λ (line column position)
                               (Location line column position (object-name in)))))
         (define (grab-syntax datum)
           (Syntax datum #f (grab-location)))
         (define (get)
           (read-char in))
         (define (peek)
           (peek-char in))
         <read/subroutines>
         (let next ()
           (skip-blanks)
           (let ([c (peek)])
             (cond [(eof-object? c) c]
                   <read/skip>
                   <read/delimiters>
                   <read/string>
                   <read/symbol>))))]

@chunk[<provisions/syntax>
       read]

@chunk[<read/subroutines>
       (define (skip-blanks)
         (define c (peek))
         (cond [(eof-object? c)
                (void)]
               [(char-whitespace? c)
                (get)
                (skip-blanks)]
               [else
                (void)]))]

@chunk[<read/skip>
       [(char=? c #\;)
        (read-line in)
        (skip-blanks)
        (next)]]

@chunk[<read/string>
       [(char=? c #\")
        (define port (open-output-string))
        (get)
        (let loop ()
          (let ([c (get)])
            (error-on-eof c "in string")
            (cond [(char=? c #\\)
                   (define c (get))
                   (error-on-eof c "in string")
                   (write-char c port)
                   (loop)]
                  [(char=? c #\")
                   (grab-syntax (Syntax-Literal (get-output-string port)))]
                  [else
                   (write-char c port)
                   (loop)])))]]

@chunk[<read/subroutines>
       (define (error-on-eof c where)
         (when (eof-object? c)
           (error (printf "End of file ~A" where))))]

@chunk[<read/delimiters>
       [(closing-delimiter? c)
        (error "Closing delimiter out of context: " c)]
       [(char=? c #\@)
        (get)
        (grab-syntax (Syntax-At (read in)))]
       [(char=? c #\#)
        (get)
        (define name (read in))
        (define delimited (read in))
        (match delimited
          [(Syntax (Syntax-Cons _) _ _)
           (grab-syntax (Syntax-Delimited "()" name delimited))]
          [(Syntax (Syntax-Delimited delimiters _ datum) _ _)
           (grab-syntax (Syntax-Delimited delimiters name datum))]
          [_
           (error "Invalid # delimited expression")])]
       [(opening-delimiter? c)
        (get)
        (define close (closing-delimiter-of c))
        (define delimiters (format "~A~A" c close))
        (let loop ([data (list)])
          (skip-blanks)
          (let ([c (peek)])
            (error-on-eof c "in delimited expression")
            (if (char=? c close)
                (begin (get)
                       (if (char=? close #\))
                           (if (null? data)
                               (grab-syntax (Syntax-Nil))
                               (grab-syntax (Syntax-Cons (reverse data))))
                           (grab-syntax (Syntax-Delimited delimiters #f (reverse data)))))
                (loop (cons (read in) data)))))]]

The full list of delimiters takes up a lot of vertical
space, so I've put it later.

@chunk[<reader>
       (define opening-delimiters <too-many-delimiters>)
       (define closing-delimiters (make-hash (map (λ (x)
                                                    (cons x #t))
                                                  (hash-values opening-delimiters))))

       (define (opening-delimiter? c)
         (hash-ref opening-delimiters c #f))
       (define (delimiter? c)
         (hash-ref opening-delimiters c #f))
       (define (closing-delimiter-of c)
         (hash-ref opening-delimiters c))
       (define (closing-delimiter? c)
         (hash-ref closing-delimiters c #f))]

All the complexity lies in symbols, of course. We also have
to deal with qualified names like @code{x.y.z}, which are
composed from several symbols.

@chunk[<read/symbol>
       [(char=? c #\:)
        (grab-syntax (Syntax-Label (read-symbol)))]
       [else
        (define thing (read-symbol-or-number))
        (cond [(number? thing)
               (grab-syntax (Syntax-Literal thing))]
              [(Syntax-Qualified? thing)
               thing]
              [else
               (grab-syntax (Syntax-Symbol thing))])]]

@chunk[<read/subroutines>
       (define (read-symbol)
         (match (read-symbol-or-number)
           [(? number? _) (error "Expecting name")]
           [(? Symbol? symbol) symbol]))
       (define (read-symbol-or-number)
         (let loop ([buffer (open-output-string)] (code:comment @#,elem{holds segment of qualified name})
                    [text (open-output-string)] (code:comment @#,elem{all text read so far})
                    [digits-only? #f] (code:comment @#,elem{have we seen only digits?})
                    [definitely-symbol? #f] (code:comment @#,elem{must we be reading a symbol?})
                    [qualifiers (list)]) (code:comment @#,elem{segments before dots (reversed)})
           (define (record c)
             (write-char c buffer)
             (write-char c text))
           (let ([c (peek)])
             (cond [(or (eof-object? c)
                        (delimiter? c)
                        (closing-delimiter? c)
                        (char=? c #\#)
                        (char-whitespace? c))
                    <read/finish-symbol-or-number>]
                   [(and (char=? c #\.)
                         (not digits-only?))
                    (loop (open-output-string)
                          text
                          #f
                          #t
                          (cons (intern (get-output-string buffer)) qualifiers))]
                   <read/symbol/escapes>
                   [else
                    (get)
                    (record c)
                    (loop buffer
                          text
                          (and digits-only? (char-numeric? c))
                          definitely-symbol?
                          qualifiers)]))))]

@chunk[<read/finish-symbol-or-number>
       (cond [definitely-symbol?
              (if (null? qualifiers)
                  (intern (get-output-string text))
                  (grab-syntax (Syntax-Qualified (reverse qualifiers) (intern (get-output-string buffer)))))]
             [(string->number (get-output-string text))]
             [else
              (intern (get-output-string text))])]

@chunk[<read/symbol/escapes>
       [(char=? c #\\)
        (get)
        (define c (get))
        (error-on-eof c "after backslash in symbol")
        (record c)
        (loop buffer
              text
              #f
              #t
              qualifiers)]
       [(char=? c #\|)
        (get)
        <read/multiple-escape>]]

@chunk[<read/multiple-escape>
       (let looop ()
         (define c (get))
         (error-on-eof c "in escape sequence in symbol")
         (cond [(char=? c #\|)
                (loop buffer
                      text
                      #f
                      #t
                      qualifiers)]
               [(char=? c #\\)
                (define c (get))
                (error-on-eof c "in escape sequence in symbol")
                (record c)
                (looop)]
               [else
                (record c)
                (looop)]))]

@subsubsection{Delimiters!}

There are a lot of delimiting characters in Unicode, as it
turns out. Here are the pairs recognized by Nomlisp (they
have a @code{Bidi_Paired_Bracket_Type} value of @code{Open}
or @code{Close}, as found in
@link["https://www.unicode.org/Public/UCD/latest/ucd/BidiBrackets.txt" (code "BidiBrackets.txt")]
in the Unicode Character Database).

@chunk[<too-many-delimiters>
       (hash #\u0028 (code:comment @#,code{LEFT PARENTHESIS})
             #\u0029 (code:comment @#,code{RIGHT PARENTHESIS})

             #\u005b (code:comment @#,code{LEFT SQUARE BRACKET})
             #\u005d (code:comment @#,code{RIGHT SQUARE BRACKET})

             #\u007b (code:comment @#,code{LEFT CURLY BRACKET})
             #\u007d (code:comment @#,code{RIGHT CURLY BRACKET})

             #\u0f3a (code:comment @#,code{TIBETAN MARK GUG RTAGS GYON})
             #\u0f3b (code:comment @#,code{TIBETAN MARK GUG RTAGS GYAS})

             #\u0f3c (code:comment @#,code{TIBETAN MARK ANG KHANG GYON})
             #\u0f3d (code:comment @#,code{TIBETAN MARK ANG KHANG GYAS})

             #\u169b (code:comment @#,code{OGHAM FEATHER MARK})
             #\u169c (code:comment @#,code{OGHAM REVERSED FEATHER MARK})

             #\u2045 (code:comment @#,code{LEFT SQUARE BRACKET WITH QUILL})
             #\u2046 (code:comment @#,code{RIGHT SQUARE BRACKET WITH QUILL})

             #\u207d (code:comment @#,code{SUPERSCRIPT LEFT PARENTHESIS})
             #\u207e (code:comment @#,code{SUPERSCRIPT RIGHT PARENTHESIS})

             #\u208d (code:comment @#,code{SUBSCRIPT LEFT PARENTHESIS})
             #\u208e (code:comment @#,code{SUBSCRIPT RIGHT PARENTHESIS})

             #\u2308 (code:comment @#,code{LEFT CEILING})
             #\u2309 (code:comment @#,code{RIGHT CEILING})

             #\u230a (code:comment @#,code{LEFT FLOOR})
             #\u230b (code:comment @#,code{RIGHT FLOOR})

             #\u2329 (code:comment @#,code{LEFT-POINTING ANGLE BRACKET})
             #\u232a (code:comment @#,code{RIGHT-POINTING ANGLE BRACKET})

             #\u2768 (code:comment @#,code{MEDIUM LEFT PARENTHESIS ORNAMENT})
             #\u2769 (code:comment @#,code{MEDIUM RIGHT PARENTHESIS ORNAMENT})

             #\u276a (code:comment @#,code{MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT})
             #\u276b (code:comment @#,code{MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT})

             #\u276c (code:comment @#,code{MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT})
             #\u276d (code:comment @#,code{MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT})

             #\u276e (code:comment @#,code{HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT})
             #\u276f (code:comment @#,code{HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT})

             #\u2770 (code:comment @#,code{HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT})
             #\u2771 (code:comment @#,code{HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT})

             #\u2772 (code:comment @#,code{LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT})
             #\u2773 (code:comment @#,code{LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT})

             #\u2774 (code:comment @#,code{MEDIUM LEFT CURLY BRACKET ORNAMENT})
             #\u2775 (code:comment @#,code{MEDIUM RIGHT CURLY BRACKET ORNAMENT})

             #\u27c5 (code:comment @#,code{LEFT S-SHAPED BAG DELIMITER})
             #\u27c6 (code:comment @#,code{RIGHT S-SHAPED BAG DELIMITER})

             #\u27e6 (code:comment @#,code{MATHEMATICAL LEFT WHITE SQUARE BRACKET})
             #\u27e7 (code:comment @#,code{MATHEMATICAL RIGHT WHITE SQUARE BRACKET})

             #\u27e8 (code:comment @#,code{MATHEMATICAL LEFT ANGLE BRACKET})
             #\u27e9 (code:comment @#,code{MATHEMATICAL RIGHT ANGLE BRACKET})

             #\u27ea (code:comment @#,code{MATHEMATICAL LEFT DOUBLE ANGLE BRACKET})
             #\u27eb (code:comment @#,code{MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET})

             #\u27ec (code:comment @#,code{MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET})
             #\u27ed (code:comment @#,code{MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET})

             #\u27ee (code:comment @#,code{MATHEMATICAL LEFT FLATTENED PARENTHESIS})
             #\u27ef (code:comment @#,code{MATHEMATICAL RIGHT FLATTENED PARENTHESIS})

             #\u2983 (code:comment @#,code{LEFT WHITE CURLY BRACKET})
             #\u2984 (code:comment @#,code{RIGHT WHITE CURLY BRACKET})

             #\u2985 (code:comment @#,code{LEFT WHITE PARENTHESIS})
             #\u2986 (code:comment @#,code{RIGHT WHITE PARENTHESIS})

             #\u2987 (code:comment @#,code{Z NOTATION LEFT IMAGE BRACKET})
             #\u2988 (code:comment @#,code{Z NOTATION RIGHT IMAGE BRACKET})

             #\u2989 (code:comment @#,code{Z NOTATION LEFT BINDING BRACKET})
             #\u298a (code:comment @#,code{Z NOTATION RIGHT BINDING BRACKET})

             #\u298b (code:comment @#,code{LEFT SQUARE BRACKET WITH UNDERBAR})
             #\u298c (code:comment @#,code{RIGHT SQUARE BRACKET WITH UNDERBAR})

             #\u298d (code:comment @#,code{LEFT SQUARE BRACKET WITH TICK IN TOP CORNER})
             #\u298e (code:comment @#,code{RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER})

             #\u298f (code:comment @#,code{LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER})
             #\u2990 (code:comment @#,code{RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER})

             #\u2991 (code:comment @#,code{LEFT ANGLE BRACKET WITH DOT})
             #\u2992 (code:comment @#,code{RIGHT ANGLE BRACKET WITH DOT})

             #\u2993 (code:comment @#,code{LEFT ARC LESS-THAN BRACKET})
             #\u2994 (code:comment @#,code{RIGHT ARC GREATER-THAN BRACKET})

             #\u2995 (code:comment @#,code{DOUBLE LEFT ARC GREATER-THAN BRACKET})
             #\u2996 (code:comment @#,code{DOUBLE RIGHT ARC LESS-THAN BRACKET})

             #\u2997 (code:comment @#,code{LEFT BLACK TORTOISE SHELL BRACKET})
             #\u2998 (code:comment @#,code{RIGHT BLACK TORTOISE SHELL BRACKET})

             #\u29d8 (code:comment @#,code{LEFT WIGGLY FENCE})
             #\u29d9 (code:comment @#,code{RIGHT WIGGLY FENCE})

             #\u29da (code:comment @#,code{LEFT DOUBLE WIGGLY FENCE})
             #\u29db (code:comment @#,code{RIGHT DOUBLE WIGGLY FENCE})

             #\u29fc (code:comment @#,code{LEFT-POINTING CURVED ANGLE BRACKET})
             #\u29fd (code:comment @#,code{RIGHT-POINTING CURVED ANGLE BRACKET})

             #\u2e22 (code:comment @#,code{TOP LEFT HALF BRACKET})
             #\u2e23 (code:comment @#,code{TOP RIGHT HALF BRACKET})

             #\u2e24 (code:comment @#,code{BOTTOM LEFT HALF BRACKET})
             #\u2e25 (code:comment @#,code{BOTTOM RIGHT HALF BRACKET})

             #\u2e26 (code:comment @#,code{LEFT SIDEWAYS U BRACKET})
             #\u2e27 (code:comment @#,code{RIGHT SIDEWAYS U BRACKET})

             #\u2e28 (code:comment @#,code{LEFT DOUBLE PARENTHESIS})
             #\u2e29 (code:comment @#,code{RIGHT DOUBLE PARENTHESIS})

             #\u2e55 (code:comment @#,code{LEFT SQUARE BRACKET WITH STROKE})
             #\u2e56 (code:comment @#,code{RIGHT SQUARE BRACKET WITH STROKE})

             #\u2e57 (code:comment @#,code{LEFT SQUARE BRACKET WITH DOUBLE STROKE})
             #\u2e58 (code:comment @#,code{RIGHT SQUARE BRACKET WITH DOUBLE STROKE})

             #\u2e59 (code:comment @#,code{TOP HALF LEFT PARENTHESIS})
             #\u2e5a (code:comment @#,code{TOP HALF RIGHT PARENTHESIS})

             #\u2e5b (code:comment @#,code{BOTTOM HALF LEFT PARENTHESIS})
             #\u2e5c (code:comment @#,code{BOTTOM HALF RIGHT PARENTHESIS})

             #\u3008 (code:comment @#,code{LEFT ANGLE BRACKET})
             #\u3009 (code:comment @#,code{RIGHT ANGLE BRACKET})

             #\u300a (code:comment @#,code{LEFT DOUBLE ANGLE BRACKET})
             #\u300b (code:comment @#,code{RIGHT DOUBLE ANGLE BRACKET})

             #\u300c (code:comment @#,code{LEFT CORNER BRACKET})
             #\u300d (code:comment @#,code{RIGHT CORNER BRACKET})

             #\u300e (code:comment @#,code{LEFT WHITE CORNER BRACKET})
             #\u300f (code:comment @#,code{RIGHT WHITE CORNER BRACKET})

             #\u3010 (code:comment @#,code{LEFT BLACK LENTICULAR BRACKET})
             #\u3011 (code:comment @#,code{RIGHT BLACK LENTICULAR BRACKET})

             #\u3014 (code:comment @#,code{LEFT TORTOISE SHELL BRACKET})
             #\u3015 (code:comment @#,code{RIGHT TORTOISE SHELL BRACKET})

             #\u3016 (code:comment @#,code{LEFT WHITE LENTICULAR BRACKET})
             #\u3017 (code:comment @#,code{RIGHT WHITE LENTICULAR BRACKET})

             #\u3018 (code:comment @#,code{LEFT WHITE TORTOISE SHELL BRACKET})
             #\u3019 (code:comment @#,code{RIGHT WHITE TORTOISE SHELL BRACKET})

             #\u301a (code:comment @#,code{LEFT WHITE SQUARE BRACKET})
             #\u301b (code:comment @#,code{RIGHT WHITE SQUARE BRACKET})

             #\ufe59 (code:comment @#,code{SMALL LEFT PARENTHESIS})
             #\ufe5a (code:comment @#,code{SMALL RIGHT PARENTHESIS})

             #\ufe5b (code:comment @#,code{SMALL LEFT CURLY BRACKET})
             #\ufe5c (code:comment @#,code{SMALL RIGHT CURLY BRACKET})

             #\ufe5d (code:comment @#,code{SMALL LEFT TORTOISE SHELL BRACKET})
             #\ufe5e (code:comment @#,code{SMALL RIGHT TORTOISE SHELL BRACKET})

             #\uff08 (code:comment @#,code{FULLWIDTH LEFT PARENTHESIS})
             #\uff09 (code:comment @#,code{FULLWIDTH RIGHT PARENTHESIS})

             #\uff3b (code:comment @#,code{FULLWIDTH LEFT SQUARE BRACKET})
             #\uff3d (code:comment @#,code{FULLWIDTH RIGHT SQUARE BRACKET})

             #\uff5b (code:comment @#,code{FULLWIDTH LEFT CURLY BRACKET})
             #\uff5d (code:comment @#,code{FULLWIDTH RIGHT CURLY BRACKET})

             #\uff5f (code:comment @#,code{FULLWIDTH LEFT WHITE PARENTHESIS})
             #\uff60 (code:comment @#,code{FULLWIDTH RIGHT WHITE PARENTHESIS})

             #\uff62 (code:comment @#,code{HALFWIDTH LEFT CORNER BRACKET})
             #\uff63 (code:comment @#,code{HALFWIDTH RIGHT CORNER BRACKET}))]