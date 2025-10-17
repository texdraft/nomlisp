#lang racket

(require "syntax.rkt"
         "ast.rkt"
         racket/control)

(provide parse-file)

(struct Token
  (type ; 'open, 'close, '@, '⇐, '\., 'identifier, 'label, 'integer, 'string, '\#
   data
   origin)
  #:transparent)

(define (non-constituent? c)
  (or (char-whitespace? c)
      (opening-delimiter? c)
      (closing-delimiter? c)
      (member c '(#\@ #\⇐ #\# #\; #\“ #\‘ #\.))))

(define (constituent? c)
  (not (non-constituent? c)))

(define (digit? c [radix 10])
  (define digits (substring "0123456789abcdefghijklmnopqrstuvwxyz" 0 radix))
  (string-find digits (string c)))

(define (hexadecimal-digit? c)
  (digit? c 16))

(define (decimal-digit? c)
  (digit? c 10))

(define (read-characters-while in
                               test?
                               end-of-file-ok?
                               [bad (λ ()
                                      (error "Unexpected end of file"))])
  (define (oops) (if (string? bad)
                     (error bad)
                     (bad)))
  (let loop ([out (open-output-string)])
    (define c (peek-char in))
    (cond [(and (eof-object? c)
                (not end-of-file-ok?))
           (error)]
          [(eof-object? c)
           (get-output-string out)]
          [(test? c)
           (read-char in)
           (write-char c out)
           (loop out)]
          [else
           (get-output-string out)])))

(define (get-symbol-or-number in line column)
  (define text (read-characters-while in constituent? #t))
  (define end (- (string-length text) 1))
  (define (try-number no)
    (let loop ([i 0]
               [radix #f]
               [value #f]
               [state 'start]) ; start, seen-digits, seen-radix
      (if (= i end)
          (match state
            ['start
             (no)]
            ['seen-radix
             (no)]
            ['seen-digits
             (Token 'integer value (Origin line column))])
          (match* (state (string-ref text i))
            [('start (app decimal-digit? (and v (not #f))))
             (loop (+ i 1)
                   #f
                   v
                   'seen-digits)]
            [('start c)
             (no)]
            [('seen-digits #\r)
             (loop (+ i 1)
                   value
                   0
                   'seen-radix)]
            [('seen-digits c)
             (no)]
            [('seen-radix (app (λ (c)
                                 (digit? c radix))
                               (and v (not #f))))
             (loop (+ i 1)
                   radix
                   (+ (* value radix) v)
                   'seen-radix)]
            [(_ _)
             (no)]))))
  (try-number (λ ()
                (Token 'identifier text (Origin line column)))))

(define (get-string opening in line column)
  (define closing (match opening
                    [#\“ #\”]
                    [#\‘ #\’]))
  (let loop ([out (open-output-string)])
    (define c (read-char in))
    (match c
      [(? eof-object?)
       (error "End of file in string")]
      [closing
       (Token 'string (get-output-string out) (Origin line column))]
      [#\\
       (define c2 (read-char in))
       (match c2
         [(? eof-object?)
          (error "End of file in string escape sequence")]
         [(? digit?)
          (define digits
            (string-append (string c2)
                           (read-characters-while in
                                                  hexadecimal-digit?
                                                  #f
                                                  "End of file in string escape sequence")))
          (write-char (integer->char (string->number digits 16)))]
         [else
          (write-char c2 out)])
       (loop out)]
      [else
       (write-char c out)
       (loop out)])))

(define (next-token in)
  (define-values (line column _) (port-next-location in))
  (define c (peek-char in))
  (define (make-token type data)
    (Token type data (Origin line column)))
  (match c
    [(? eof-object?)
     eof]
    [(? char-whitespace?)
     (read-char in)
     (next-token in)]
    [#\;
     (read-line in)
     (next-token in)]
    [#\.
     (read-char in)
     (make-token '\. #f)]
    [#\:
     (read-char in)
     (define c2 (peek-char in))
     (if (constituent? c2)
         (make-token 'label (string-append (string c2)
                                           (read-characters-while in constituent? #t)))
         (make-token 'identifier ":"))]
    [#\@
     (read-char in)
     (make-token '@ #f)]
    [#\⇐
     (read-char in)
     (make-token '⇐ #f)]
    [#\#
     (read-char in)
     (make-token '\# #f)]
    [(or #\” #\’)
     (read-char in)
     (error (format "Extraneous ‘~A’" c))]
    [(or #\“ #\‘)
     (read-char in)
     (get-string c in line column)]
    [(? opening-delimiter?)
     (read-char in)
     (make-token 'open c)]
    [(? closing-delimiter?)
     (read-char in)
     (make-token 'close c)]
    [_
     (get-symbol-or-number in line column)]))

(define (parse-file in)
  (define pending '())
  (define (next!)
    (if (null? pending)
        (next-token in)
        (begin0 (car pending)
                (set! pending (cdr pending)))))
  (define (go/delimited close)
    (let loop ([items '()])
      (define t (next!))
      (match t
        [(? eof-object?)
         (error "End of file before delimited list ended")]
        [(Token 'close close2 _)
         (if (char=? close close2)
             (reverse items)
             (error "Closing delimiter out of context"))]
        [else
         (loop (cons (go/1 t) items))])))
  (define (go/1 [t (next!)])
    (match t
      [(? eof-object?)
       eof]
      [(Token 'identifier data origin)
       (define identifier (make-syntax (Symbol data) origin))
       (define dot? (next!))
       (match dot?
         [(Token '\. _ _)
          (make-syntax (Dotted identifier (go/1)) origin)]
         [else
          (set! pending (cons dot? pending))
          identifier])]
      [(Token 'open open origin)
       (make-syntax (if (char=? open #\()
                        (List (go/delimited #\)))
                        (Delimited (format "~A~A"
                                           open
                                           (closing-delimiter-for open))
                                   (go/delimited (closing-delimiter-for open))))
                    origin)]
      [(Token 'close close2 origin)
       (error "Closing delimiter out of context")]
      [(Token '@ _ origin)
       (make-syntax (At (go/1)) origin)]
      [(Token '⇐ _ origin)
       (make-syntax (Left-Double-Arrow (go/1) origin))]
      [(Token 'label data origin)
       (make-syntax (Label data) origin)]
      [(Token 'integer data origin)
       (make-syntax (Constant data) origin)]
      [(Token 'string data origin)
       (make-syntax (Constant data) origin)]
      [(Token '\# _ origin)
       (define prefix (next!))
       (make-syntax (Prefixed prefix (go/1)))]))
  (for/list ()
    (define result (go/1))
    #:break (eof-object? result)
    result))

