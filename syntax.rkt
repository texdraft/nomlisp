#lang racket

(require racket/struct
         "ast.rkt")

(require racket/stxparam)

(provide (all-defined-out))

(struct Origin
  (line column)
  #:transparent)

(struct Free-Names
  (synspaces)) ; list of (cons synspace name)

(struct Syntax
  (form
   syntactic-environment
   free-names
   origin)
  #:methods gen:custom-write
  [(define (write-proc s out _)
     (fprintf out "$~A" (~v (Syntax-form s))))])

(define (unwrap s)
  (match s
    [(Syntax form #f '() _)
     (unwrap form)]
    [else
     s]))

(define (make-syntax form origin [environment #f] [free '()])
  (Syntax form environment free origin))

(define (syntactic-closure? syntax)
  (match syntax
    [(Syntax _ (not #f) _ _)
     #t]
    [else
     #f]))

(define (make-syntactic-closure environment free expression [origin #f])
  (match expression
    [(Syntax expression´ #f (Free-Names '()) origin´)
     (Syntax expression´ environment free (or origin origin´))]
    [else
     (Syntax expression environment free origin)]))

(define opening-delimiter-table
   (hash #\u0028 #\u0029 ; {LEFT, RIGHT} PARENTHESIS
         #\u005b #\u005d ; {LEFT, RIGHT} SQUARE BRACKET
         #\u007b #\u007d ; {LEFT, RIGHT} CURLY BRACKET
         #\u0f3a #\u0f3b ; TIBETAN MARK GUG RTAGS {GYON, GYAS}
         #\u0f3c #\u0f3d ; TIBETAN MARK ANG KHANG {GYON, GYAS}
         #\u169b #\u169c ; OGHAM FEATHER MARK, OGHAM REVERSED FEATHER MARK
         #\u2045 #\u2046 ; {LEFT, RIGHT} SQUARE BRACKET WITH QUILL
         #\u207d #\u207e ; SUPERSCRIPT {LEFT, RIGHT} PARENTHESIS
         #\u208d #\u208e ; SUBSCRIPT {LEFT, RIGHT} PARENTHESIS
         #\u2308 #\u2309 ; {LEFT, RIGHT} CEILING
         #\u230a #\u230b ; {LEFT, RIGHT} FLOOR
         #\u2329 #\u232a ; {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET
         #\u2768 #\u2769 ; MEDIUM {LEFT, RIGHT} PARENTHESIS ORNAMENT
         #\u276a #\u276b ; MEDIUM FLATTENED {LEFT, RIGHT} PARENTHESIS ORNAMENT
         #\u276c #\u276d ; MEDIUM {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET ORNAMENT
         #\u276e #\u276f ; HEAVY {LEFT-POINTING, RIGHT-POINTING} ANGLE QUOTATION MARK ORNAMENT
         #\u2770 #\u2771 ; HEAVY {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET ORNAMENT
         #\u2772 #\u2773 ; LIGHT {LEFT, RIGHT} TORTOISE SHELL BRACKET ORNAMENT
         #\u2774 #\u2775 ; MEDIUM {LEFT, RIGHT} CURLY BRACKET ORNAMENT
         #\u27c5 #\u27c6 ; {LEFT, RIGHT} S-SHAPED BAG DELIMITER
         #\u27e6 #\u27e7 ; MA#\u005b #\u005d ; {LEFT, RIGHT} SQUARE BRACKET
         #\u007b #\u007d ; {LEFT, RIGHT} CURLY BRACKET
         #\u0f3a #\u0f3b ; TIBETAN MARK GUG RTAGS {GYON, GYAS}
         #\u0f3c #\u0f3d ; TIBETAN MARK ANG KHANG {GYON, GYAS}
         #\u169b #\u169c ; OGHAM FEATHER MARK, OGHAM REVERSED FEATHER MARK
         #\u2045 #\u2046 ; {LEFT, RIGHT} SQUARE BRACKET WITH QUILL
         #\u207d #\u207e ; SUPERSCRIPT {LEFT, RIGHT} PARENTHESIS
         #\u208d #\u208e ; SUBSCRIPT {LEFT, RIGHT} PARENTHESIS
         #\u2308 #\u2309 ; {LEFT, RIGHT} CEILING
         #\u230a #\u230b ; {LEFT, RIGHT} FLOOR
         #\u2329 #\u232a ; {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET
         #\u2768 #\u2769 ; MEDIUM {LEFT, RIGHT} PARENTHESIS ORNAMENT
         #\u276a #\u276b ; MEDIUM FLATTENED {LEFT, RIGHT} PARENTHESIS ORNAMENT
         #\u276c #\u276d ; MEDIUM {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET ORNAMENT
         #\u276e #\u276f ; HEAVY {LEFT-POINTING, RIGHT-POINTING} ANGLE QUOTATION MARK ORNAMENT
         #\u2770 #\u2771 ; HEAVY {LEFT-POINTING, RIGHT-POINTING} ANGLE BRACKET ORNAMENT
         #\u2772 #\u2773 ; LIGHT {LEFT, RIGHT} TORTOISE SHELL BRACKET ORNAMENT
         #\u2774 #\u2775 ; MEDIUM {LEFT, RIGHT} CURLY BRACKET ORNAMENT
         #\u27c5 #\u27c6 ; {LEFT, RIGHT} S-SHAPED BAG DELIMITER
         #\u27e6 #\u27e7 ; MATHEMATICAL {LEFT, RIGHT} WHITE SQUARE BRACKET
         #\u27e8 #\u27e9 ; MATHEMATICAL {LEFT, RIGHT} ANGLE BRACKET
         #\u27ea #\u27eb ; MATHEMATICAL {LEFT, RIGHT} DOUBLE ANGLE BRACKET
         #\u27ec #\u27ed ; MATHEMATICAL {LEFT, RIGHT} WHITE TORTOISE SHELL BRACKET
         #\u27ee #\u27ef ; MATHEMATICAL {LEFT, RIGHT} FLATTENED PARENTHESIS
         #\u2983 #\u2984 ; {LEFT, RIGHT} WHITE CURLY BRACKET
         #\u2985 #\u2986 ; {LEFT, RIGHT} WHITE PARENTHESIS
         #\u2987 #\u2988 ; Z NOTATION {LEFT, RIGHT} IMAGE BRACKET
         #\u2989 #\u298a ; Z NOTATION {LEFT, RIGHT} BINDING BRACKET
         #\u298b #\u298c ; {LEFT, RIGHT} SQUARE BRACKET WITH UNDERBAR
         #\u298d #\u298e ; {LEFT, RIGHT} SQUARE BRACKET WITH TICK IN TOP CORNER
         #\u298f #\u2990 ; {LEFT, RIGHT} SQUARE BRACKET WITH TICK IN BOTTOM CORNER
         #\u2991 #\u2992 ; {LEFT, RIGHT} ANGLE BRACKET WITH DOT
         #\u2993 #\u2994 ; {LEFT, RIGHT} ARC LESS-THAN BRACKET
         #\u2995 #\u2996 ; DOUBLE {LEFT, RIGHT} ARC GREATER-THAN BRACKET
         #\u2997 #\u2998 ; {LEFT, RIGHT} BLACK TORTOISE SHELL BRACKET
         #\u29d8 #\u29d9 ; {LEFT, RIGHT} WIGGLY FENCE
         #\u29da #\u29db ; {LEFT, RIGHT} DOUBLE WIGGLY FENCE
         #\u29fc #\u29fd ; {LEFT-POINTING, RIGHT-POINTING} CURVED ANGLE BRACKET
         #\u2e22 #\u2e23 ; TOP {LEFT, RIGHT} HALF BRACKET
         #\u2e24 #\u2e25 ; BOTTOM {LEFT, RIGHT} HALF BRACKET
         #\u2e26 #\u2e27 ; {LEFT, RIGHT} SIDEWAYS U BRACKET
         #\u2e28 #\u2e29 ; {LEFT, RIGHT} DOUBLE PARENTHESIS
         #\u2e55 #\u2e56 ; {LEFT, RIGHT} SQUARE BRACKET WITH STROKE
         #\u2e57 #\u2e58 ; {LEFT, RIGHT} SQUARE BRACKET WITH DOUBLE STROKE
         #\u2e59 #\u2e5a ; TOP HALF {LEFT, RIGHT} PARENTHESIS
         #\u2e5b #\u2e5c ; BOTTOM HALF {LEFT, RIGHT} PARENTHESIS
         #\u3008 #\u3009 ; {LEFT, RIGHT} ANGLE BRACKET
         #\u300a #\u300b ; {LEFT, RIGHT} DOUBLE ANGLE BRACKET
         #\u300c #\u300d ; {LEFT, RIGHT} CORNER BRACKET
         #\u300e #\u300f ; {LEFT, RIGHT} WHITE CORNER BRACKET
         #\u3010 #\u3011 ; {LEFT, RIGHT} BLACK LENTICULAR BRACKET
         #\u3014 #\u3015 ; {LEFT, RIGHT} TORTOISE SHELL BRACKET
         #\u3016 #\u3017 ; {LEFT, RIGHT} WHITE LENTICULAR BRACKET
         #\u3018 #\u3019 ; {LEFT, RIGHT} WHITE TORTOISE SHELL BRACKET
         #\u301a #\u301b ; {LEFT, RIGHT} WHITE SQUARE BRACKET
         #\ufe59 #\ufe5a ; SMALL {LEFT, RIGHT} PARENTHESIS
         #\ufe5b #\ufe5c ; SMALL {LEFT, RIGHT} CURLY BRACKET
         #\ufe5d #\ufe5e ; SMALL {LEFT, RIGHT} TORTOISE SHELL BRACKET
         #\uff08 #\uff09 ; FULLWIDTH {LEFT, RIGHT} PARENTHESIS
         #\uff3b #\uff3d ; FULLWIDTH {LEFT, RIGHT} SQUARE BRACKET
         #\uff5b #\uff5d ; FULLWIDTH {LEFT, RIGHT} CURLY BRACKET
         #\uff5f #\uff60 ; FULLWIDTH {LEFT, RIGHT} WHITE PARENTHESIS
         #\uff62 #\uff63 ; HALTHEMATICAL {LEFT, RIGHT} WHITE SQUARE BRACKET
         #\u27e8 #\u27e9 ; MATHEMATICAL {LEFT, RIGHT} ANGLE BRACKET
         #\u27ea #\u27eb ; MATHEMATICAL {LEFT, RIGHT} DOUBLE ANGLE BRACKET
         #\u27ec #\u27ed ; MATHEMATICAL {LEFT, RIGHT} WHITE TORTOISE SHELL BRACKET
         #\u27ee #\u27ef ; MATHEMATICAL {LEFT, RIGHT} FLATTENED PARENTHESIS
         #\u2983 #\u2984 ; {LEFT, RIGHT} WHITE CURLY BRACKET
         #\u2985 #\u2986 ; {LEFT, RIGHT} WHITE PARENTHESIS
         #\u2987 #\u2988 ; Z NOTATION {LEFT, RIGHT} IMAGE BRACKET
         #\u2989 #\u298a ; Z NOTATION {LEFT, RIGHT} BINDING BRACKET
         #\u298b #\u298c ; {LEFT, RIGHT} SQUARE BRACKET WITH UNDERBAR
         #\u298d #\u298e ; {LEFT, RIGHT} SQUARE BRACKET WITH TICK IN TOP CORNER
         #\u298f #\u2990 ; {LEFT, RIGHT} SQUARE BRACKET WITH TICK IN BOTTOM CORNER
         #\u2991 #\u2992 ; {LEFT, RIGHT} ANGLE BRACKET WITH DOT
         #\u2993 #\u2994 ; {LEFT, RIGHT} ARC LESS-THAN BRACKET
         #\u2995 #\u2996 ; DOUBLE {LEFT, RIGHT} ARC GREATER-THAN BRACKET
         #\u2997 #\u2998 ; {LEFT, RIGHT} BLACK TORTOISE SHELL BRACKET
         #\u29d8 #\u29d9 ; {LEFT, RIGHT} WIGGLY FENCE
         #\u29da #\u29db ; {LEFT, RIGHT} DOUBLE WIGGLY FENCE
         #\u29fc #\u29fd ; {LEFT-POINTING, RIGHT-POINTING} CURVED ANGLE BRACKET
         #\u2e22 #\u2e23 ; TOP {LEFT, RIGHT} HALF BRACKET
         #\u2e24 #\u2e25 ; BOTTOM {LEFT, RIGHT} HALF BRACKET
         #\u2e26 #\u2e27 ; {LEFT, RIGHT} SIDEWAYS U BRACKET
         #\u2e28 #\u2e29 ; {LEFT, RIGHT} DOUBLE PARENTHESIS
         #\u2e55 #\u2e56 ; {LEFT, RIGHT} SQUARE BRACKET WITH STROKE
         #\u2e57 #\u2e58 ; {LEFT, RIGHT} SQUARE BRACKET WITH DOUBLE STROKE
         #\u2e59 #\u2e5a ; TOP HALF {LEFT, RIGHT} PARENTHESIS
         #\u2e5b #\u2e5c ; BOTTOM HALF {LEFT, RIGHT} PARENTHESIS
         #\u3008 #\u3009 ; {LEFT, RIGHT} ANGLE BRACKET
         #\u300a #\u300b ; {LEFT, RIGHT} DOUBLE ANGLE BRACKET
         #\u300c #\u300d ; {LEFT, RIGHT} CORNER BRACKET
         #\u300e #\u300f ; {LEFT, RIGHT} WHITE CORNER BRACKET
         #\u3010 #\u3011 ; {LEFT, RIGHT} BLACK LENTICULAR BRACKET
         #\u3014 #\u3015 ; {LEFT, RIGHT} TORTOISE SHELL BRACKET
         #\u3016 #\u3017 ; {LEFT, RIGHT} WHITE LENTICULAR BRACKET
         #\u3018 #\u3019 ; {LEFT, RIGHT} WHITE TORTOISE SHELL BRACKET
         #\u301a #\u301b ; {LEFT, RIGHT} WHITE SQUARE BRACKET
         #\ufe59 #\ufe5a ; SMALL {LEFT, RIGHT} PARENTHESIS
         #\ufe5b #\ufe5c ; SMALL {LEFT, RIGHT} CURLY BRACKET
         #\ufe5d #\ufe5e ; SMALL {LEFT, RIGHT} TORTOISE SHELL BRACKET
         #\uff08 #\uff09 ; FULLWIDTH {LEFT, RIGHT} PARENTHESIS
         #\uff3b #\uff3d ; FULLWIDTH {LEFT, RIGHT} SQUARE BRACKET
         #\uff5b #\uff5d ; FULLWIDTH {LEFT, RIGHT} CURLY BRACKET
         #\uff5f #\uff60 ; FULLWIDTH {LEFT, RIGHT} WHITE PARENTHESIS
         #\uff62 #\uff63)) ; HALFWIDTH {LEFT, RIGHT} CORNER BRACKET

(define closing-delimiter-table
  (hash-map/copy opening-delimiter-table (lambda (key value)
                                           (values value key))))

(define (opening-delimiter? c)
  (hash-ref opening-delimiter-table c #f))

(define (closing-delimiter? c)
  (hash-ref closing-delimiter-table c #f))

(define (closing-delimiter-for c)
  (opening-delimiter? c))

(define (delimiter-string open)
  (format "~A~A" open (closing-delimiter-for open)))