#lang racket/base

(require racket/list)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require racket/match
         racket/pretty
         (for-syntax racket/base
                     syntax/parse))

(define-tokens python-tokens (number identifier string))

(define-empty-tokens python-empty-tokens
  [eof space tab newline
   end-of-line-comment
   continue-line
   left-bracket right-bracket
   left-paren right-paren
   left-brace right-brace
   colon])

(define-syntax (define-token? stx)
  (syntax-parse stx
    [(_ name)
     (define name? (datum->syntax #'name (string->symbol
                                           (format "token-~a?"
                                                   (symbol->string
                                                     (syntax->datum #'name))))
                                  #'name))
     (with-syntax ([name? name?])
       #'(begin
           (provide name?)
           (define (name? token)
             (equal? 'name (token-name token)))))]))

(define-syntax-rule (define-tokens? name ...)
                    (begin
                      (define-token? name) ...))

(define-tokens? eof number identifier 
                end-of-line-comment
                string
                continue-line
                ;block-comment parse-error
                left-paren right-paren
                space tab newline
                left-bracket right-bracket
                left-brace right-brace
                colon
                ;semicolon
                )

(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev number (:: (:+ digit) (:? (:: "." (:+ digit)))))
(define-lex-abbrev identifier-first-character (:or (:/ #\a #\z)
                                                   (:/ #\A #\Z)
                                                   "_"))
(define-lex-abbrev identifier-character (:or identifier-first-character
                                             digit))
(define-lex-abbrev identifier (:: identifier-first-character
                                  (:* identifier-character)))
(define-lex-abbrev space " ")
(define-lex-abbrev tab "\t")
(define-lex-abbrev newline "\n")

(define-lex-abbrev string-character-double
                   (:or (:: #\\ any-char)
                        (:~ #\")))

(define-lex-abbrev string-character-single
                   (:or (:: #\\ any-char)
                        (:& (:~ #\\) (:~ #\'))))

(define-lex-abbrev string-character-triple
                   (complement (:: #\" #\" #\"))
                   #;
                   (:or (:: #\\ any-char)
                        (:~ (:: #\" #\" #\"))))

(define-lex-abbrev string (:or
                            (:: #\" #\" #\" (:* string-character-triple)
                                #\" #\" #\")
                            (:: #\" (:* string-character-double) #\")
                            (:: #\' (:* string-character-single) #\')))

(define-lex-abbrev regex (:: #\r
                            (:: #\" (:* string-character-double) #\")))

(define-lex-abbrev operator (:or "!=" "==" "=" "+=" ">=" "+" "%" "*" "%" "<" ">" "-"))

(define-lex-abbrev line-comment (:: (:or "#")
                                    (:* (:~ "\n"))
                                    ;; we might hit eof before a \n
                                    (:? "\n" "\r")))

(define (replace-escapes string)
  (define replacements '([#px"\\\\n" "\n"]
                         [#px"\\\\t" "\t"]))
  (for/fold ([string string])  
            ([replace replacements])
            (define pattern (car replace))
            (define with (cadr replace))
            (regexp-replace* pattern string with)))


(define python-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [number (token-number (string->number lexeme))]
    [regex (let ()
              (define raw (substring (substring lexeme 2)
                                     0 (- (string-length lexeme) 3)))
              (token-string (replace-escapes raw)))]
    [identifier (token-identifier (string->symbol lexeme))]
    [space (token-space)]
    [tab (token-tab)]
    [string (let ()
              (define raw (substring (substring lexeme 1)
                                     0 (- (string-length lexeme) 2)))
              (token-string (replace-escapes raw)))]
    ["#" (token-end-of-line-comment)]
    ["." (token-identifier '%dot)]
    ["," (token-identifier '%comma)]
    [#\\ (token-continue-line)]
    ["[" (token-left-bracket)]
    ["]" (token-right-bracket)]
    ["(" (token-left-paren)]
    [")" (token-right-paren)]
    ["{" (token-left-brace)]
    ["}" (token-right-brace)]
    [":" (token-colon)]
    [newline (token-newline)]
    [operator (token-identifier (string->symbol lexeme))]
    ))

;; returns #t if an entire comment was read (with an ending newline)
(define (read-until-end-of-line input)
  (define (finish? what)
    (or (eof-object? what)
        (= (char->integer #\newline) what)))
  ;; #t if read a #\newline, otherwise #f
  (define (clean-end? what)
    (if (eof-object? what)
      #f
      (= (char->integer #\newline) what)))
  (let loop ()
    (define what (read-byte input))
    (if (not (finish? what))
      (loop)
      (clean-end? what))))

(define (python-read-port port)
  (let loop ([tokens '()])
    (define next (python-lexer port))
    ; (debug "Lexed ~a\n" (position-token-token next))
    (match next
      [(struct* position-token ([token (? token-end-of-line-comment?)]
                                [start-pos start]
                                [end-pos end]))
        (read-until-end-of-line port)
        (loop (cons (position-token (token-newline) #f #f) tokens))]

      [(struct* position-token ([token (? token-eof?)]
                                [start-pos start]
                                [end-pos end]))
       (reverse (cons next tokens))]
      [else (loop (cons next tokens))])))

(define (plain-token? token)
  (or (token-number? token)
      (token-string? token)
      (token-identifier? token)))

(define-syntax-rule (debug x ...)
                    #;
                    (printf x ...)
                    (void))

(define (tokens->datum tokens)
  (for/list ([token tokens])
    (define what (position-token-token token))
    (cond
      [(plain-token? what) (token-value what)]
      [(token-space? what) 'space]
      [(token-left-paren? what) 'lparen]
      [(token-right-paren? what) 'rparen]
      [(token-left-bracket? what) '|[|]
      [(token-right-bracket? what) '|]|]
      [(token-colon? what) ':]
      [(token-newline? what) 'newline]
      [(token-eof? what) 'eof]
      [else (token-value what)])))

(define (search pass find tokens)
  (let loop ([tokens tokens])
    (if (null? tokens)
      #f
      (let ()
        (define current (car tokens))
        (cond
          [(for/fold ([ok #f])
                     ([what pass])
                     (or ok (what (position-token-token current))))
           (loop (cdr tokens))]
          [(find (position-token-token current))
           (debug "Found ~a at ~a\n" find (tokens->datum tokens))
           tokens]
          [else #f])))))

(define (next-non-empty-line tokens)
  (define end-of-line 
    (search (list token-tab? token-space?)
            token-newline?
            tokens))
  (if (and end-of-line
           (token-newline? (position-token-token (car end-of-line))))
    (let ()
      (define next (search (list token-tab? token-space?)
                           (lambda (i)
                             (not (or (token-tab? i)
                                      (token-space? i))))
                           (cdr end-of-line)))
      (if (and next (token-newline? (position-token-token (car next))))
        (next-non-empty-line next)
        end-of-line))
    (if end-of-line
      end-of-line
      tokens)))

;; returns a tree and an unparsed tree
(define (parse tokens [delimiter #f] [indent-level 0])
  (let loop ([tree '()]
             [tokens tokens])
    (debug "Parse ~a. Tree is ~a\n" (tokens->datum tokens) tree)
    (cond
      ;; start of a line, so check the indentation level
      [(and (not delimiter)
            (null? tree)
            ; if its an empty line just keep going
            (and (not (null? tokens))
                 (not (token-newline? (position-token-token (car tokens)))))
            (let ()
              (define next-non-space (search (list token-space?)
                                             (lambda (i)
                                               (not (token-space? i)))
                                             tokens))
              (when next-non-space
                (debug "Next non-space at ~a\n" (tokens->datum next-non-space))
                (debug "Check indent new ~a old ~a for ~a\n"
                       (- (length tokens) (length next-non-space))
                       indent-level
                       (tokens->datum tokens)))
              (and next-non-space
                   (not (= (- (length tokens) (length next-non-space))
                           indent-level)))))
       (values tree tokens)]
       [(null? tokens) (values tree '())]
       [else 
         (let ()
           (define skip-space 
             (search (list token-space?)
                     (lambda (i)
                       (not (token-space? i)))
                     tokens))

           (define current (first skip-space))
           ; (debug "Token ~a\n" (token-value (position-token-token current)))
           (match current
             [(struct* position-token ([token (and token (? plain-token?))]
                                       [start-pos start]
                                       [end-pos end]))
              (loop (append tree (list (token-value token)))
                    (cdr skip-space))]

             [(struct* position-token ([token (? token-colon?)]
                                       [start-pos start]
                                       [end-pos end]))

              ;; we might be in an array-splicing operation
              ;; a[2:5]
              ;; or inside a hash {1:2}
              (if (or (eq? delimiter 'brackets)
                      (eq? delimiter 'brace))
                (loop (append tree (list '%colon))
                      (cdr skip-space))

                ;; otherwise we are in some sort of block
                ;; if x == 1:
                ;;    ....
                (let ()
                  ;; check to see if there are other tokens on this line besides
                  ;; just whitespace
                  (define single-line? (search (list token-tab? token-space?)
                                               (lambda (t)
                                                 (not (token-newline? t)))
                                               (cdr skip-space)))
                  (if single-line?
                    (let ()
                      ;; for a single line just treat it like anything else
                      ;; the parser will figure out if the thing after the colon
                      ;; should be an expression for lambda
                      (loop (append tree (list '%colon))
                            (cdr skip-space)))
                    (let ()
                      (define next-line (next-non-empty-line (cdr skip-space)))
                      (define spaces (search (list token-space?)
                                             (lambda (i)
                                               (not (token-space? i)))
                                             (cdr next-line)))
                      (define new-level (- (length (cdr next-line)) (length spaces)))
                      (debug "New level ~a\n" new-level)
                      (define-values (sub-tree rest) (parse (cdr next-line) delimiter new-level))
                      ;; create the block
                      (define this (append tree (list '%colon `(%block ,@sub-tree))))

                      ;; then keep parsing with the same indentation level
                      (define-values (lines more) (parse rest #f indent-level))
                      (values (append this lines) more)
                      #;
                      (loop this more)))))]

             [(struct* position-token ([token (? token-left-paren?)]
                                       [start-pos start]
                                       [end-pos end]))
              (define-values (sub-tree unparsed) (parse (cdr skip-space) 'parens 0))
              (loop (append tree (list `(#%parens ,@sub-tree)))
                    unparsed)]

             [(struct* position-token ([token (? token-continue-line?)]
                                       [start-pos start]
                                       [end-pos end]))
              (define next (search (list token-space? token-tab?)
                                   token-newline?
                                   (cdr skip-space)))
              (if next
                (loop tree (cdr next))
                (loop tree (cdr skip-space)))]

             [(struct* position-token ([token (? token-right-paren?)]
                                       [start-pos start]
                                       [end-pos end]))
              (when (not (eq? delimiter 'parens))
                (error 'read "unexpected `)' seen"))
              (values tree (cdr skip-space))]

             [(struct* position-token ([token (? token-left-brace?)]
                                       [start-pos start]
                                       [end-pos end]))
              (define-values (sub-tree unparsed) (parse (cdr skip-space) 'brace 0))
              (loop (append tree (list `(#%braces ,@sub-tree)))
                    unparsed)]

             [(struct* position-token ([token (? token-right-brace?)]
                                       [start-pos start]
                                       [end-pos end]))
              (when (not (eq? delimiter 'brace))
                (error 'read "unexpected `}' seen"))
              (values tree (cdr skip-space))]

             [(struct* position-token ([token (? token-left-bracket?)]
                                       [start-pos start]
                                       [end-pos end]))
              (define-values (sub-tree unparsed) (parse (cdr skip-space) 'brackets 0))
              (loop (append tree (list `(#%brackets ,@sub-tree)))
                    unparsed)]

             [(struct* position-token ([token (? token-right-bracket?)]
                                       [start-pos start]
                                       [end-pos end]))
              (when (not (eq? delimiter 'brackets))
                (error 'read "unexpected `]' seen"))
              (values tree (cdr skip-space))]

             [(struct* position-token ([token (? token-space?)]
                                       [start-pos start]
                                       [end-pos end]))

              (loop tree (cdr skip-space))]

             [(struct* position-token ([token (or (? token-newline?)
                                                  (? token-eof?))]
                                       [start-pos start]
                                       [end-pos end]))
              ; (define this-line `(%line ,@tree))
              (define this-line tree)
              (if delimiter
                (loop tree (cdr skip-space))
                (let ()
                  (define-values (lines more)
                                 (parse (cdr skip-space)
                                        delimiter indent-level))
                  (values (append (list this-line) lines)
                          more)))
              #;
              (values (list `(%line ,@tree)) (cdr skip-space))
              #;
              (define-values (sub-tree more) (parse (cdr skip-space) delimiter indent-level))
              #;
              (values (append (list `(%line ,@tree)) sub-tree)
                      more)]

             [(struct* position-token ([token (and token (or (? token-tab?)))]
                                       [start-pos start]
                                       [end-pos end]))
              (loop tree (cdr skip-space))]
             [else (error 'read "unknown token ~a" current)]))])))

;; empty lines end up being just the empty list, '(), so remove those
(define (remove-empty what)
  (match what
    [(list x ...)
     (reverse
     (for/fold ([all '()])
               ([x x])
       (if (null? x)
         all
         (cons (remove-empty x) all))))]
    [else what]))

(provide python-read)
(define (python-read [port (current-input-port)])
  ; (port-count-lines! port)
  (let loop ([tree '()]
             [unparsed (python-read-port port)])
    (if (null? unparsed)
      (remove-empty tree)
      (let ()
        (define-values (parsed rest)
                       (parse unparsed))
        (debug "Parsed ~a\n" (pretty-format parsed))
        (loop (append tree parsed)
              rest)))))

(provide python-read-string)
(define (python-read-string string)
  (python-read (open-input-string string)))

(module+ test
  (require rackunit)

  (check-equal? 1 1)

  (check-equal? (python-read-string "1") 
                '((1)))

  (check-equal? (python-read-string "x")
                '((x)))

  (check-equal? (python-read-string #<<HERE
x = 5
HERE
)
                '((x = 5)))

(check-equal? (python-read-string #<<HERE
(x)
HERE
)

                '(((#%parens x))))

  (check-equal? (python-read-string #<<HERE
x = [for blah(z) in burger]
HERE
)

                '((x = (#%brackets for blah (#%parens z) in burger))))

  (check-equal? (python-read-string #<<HERE
def foo(x):
    return x + 1
HERE
)

                '(def foo (#%parens x) %colon
                      (%block (return x + 1))))

  (check-equal? (python-read-string #<<HERE
def foo(x):
    return x + 1
foo(5)
bar(8)
HERE
)

                '(def foo (#%parens x) %colon
                      (%block (return x + 1))
                  (foo (#%parens 5))
                  (bar (#%parens 8))))

  )
