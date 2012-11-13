#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-tokens python-tokens (number identifier string))

(define-empty-tokens python-empty-tokens
  [eof])

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
                left-parens right-parens
                ;left-bracket right-bracket
                ;left-brace right-brace
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

(define python-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [number (token-number (string->number lexeme))]
    [identifier (token-identifier (string->symbol lexeme))]
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

(define (parse tokens)
  (for/list ([next tokens])
    (match next
      [(struct* position-token ([token (and token (? plain-token?))]
                                [start-pos start]
                                [end-pos end]))
       (token-value token)]))) 

(define (python-read-string string)
  (parse (python-read (open-input-string string))))

(module+ test
  (require rackunit)
  (check-equal? 1 1)

  (check-equal? (python-read-string "1") 
                '(1))

  (check-equal? (python-read-string "x")
                '(x))

  )
