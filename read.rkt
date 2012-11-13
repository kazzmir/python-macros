#lang racket/base

(require racket/list)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-tokens python-tokens (number identifier string))

(define-empty-tokens python-empty-tokens
  [eof space tab newline
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
                ;end-of-line-comment number string
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

(define-lex-abbrev operator (:or "="))

(define python-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [number (token-number (string->number lexeme))]
    [identifier (token-identifier (string->symbol lexeme))]
    [space (token-space)]
    [tab (token-tab)]
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

(define (python-read [port (current-input-port)])
  (let loop ([tokens '()])
    (define next (python-lexer port))
    (match next
      [(struct* position-token ([token (? token-eof?)]
                                [start-pos start]
                                [end-pos end]))
       (reverse tokens)]
      [else (loop (cons next tokens))])))

(define (plain-token? token)
  (or (token-number? token)
      (token-identifier? token)))

;; returns a tree and an unparsed tree
(define (parse tokens [delimiter #f])
  (let loop ([tree '()]
             [tokens tokens])
    (if (null? tokens)
      (values tree '())
      (let ()
        (define current (first tokens))
        ; (printf "Token ~a\n" (token-value (position-token-token current)))
        (match current
          [(struct* position-token ([token (and token (? plain-token?))]
                                    [start-pos start]
                                    [end-pos end]))
           (loop (append tree (list (token-value token)))
                 (cdr tokens))]
          [(struct* position-token ([token (? token-left-paren?)]
                                    [start-pos start]
                                    [end-pos end]))
           (define-values (sub-tree unparsed) (parse (cdr tokens) 'parens))
           (loop (append tree (list `(#%parens ,@sub-tree)))
                 unparsed)]

          [(struct* position-token ([token (? token-right-paren?)]
                                    [start-pos start]
                                    [end-pos end]))
           (when (not (eq? delimiter 'parens))
             (error 'read "unexpected `)' seen"))
           (values tree (cdr tokens))]

          [(struct* position-token ([token (? token-left-bracket?)]
                                    [start-pos start]
                                    [end-pos end]))
           (define-values (sub-tree unparsed) (parse (cdr tokens) 'bracket))
           (loop (append tree (list `(#%brackets ,@sub-tree)))
                 unparsed)]

          [(struct* position-token ([token (? token-right-bracket?)]
                                    [start-pos start]
                                    [end-pos end]))
           (when (not (eq? delimiter 'bracket))
             (error 'read "unexpected `]' seen"))
           (values tree (cdr tokens))]

          [(struct* position-token ([token (and token (or (? token-newline?)
                                                          (? token-space?)
                                                          (? token-tab?)))]
                                    [start-pos start]
                                    [end-pos end]))
           (loop tree (cdr tokens))])))))

(define (python-read-string string)
  (define-values (tree unparsed)
                 (parse (python-read (open-input-string string))))
  (when (not (null? unparsed))
    (error 'read "unparsed tree" unparsed))
  tree)

(module+ test
  (require rackunit)
  (check-equal? 1 1)

  (check-equal? (python-read-string "1") 
                '(1))

  (check-equal? (python-read-string "x")
                '(x))

  (check-equal? (python-read-string #<<HERE
x = 5
HERE
)
                '(x = 5))

(check-equal? (python-read-string #<<HERE
(x)
HERE
)

                '((#%parens x)))

  (check-equal? (python-read-string #<<HERE
x = [for blah(z) in burger]
HERE
)

                '(x = (#%brackets for blah (#%parens z) in burger)))

  )
