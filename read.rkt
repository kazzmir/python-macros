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

(define-lex-abbrev operator (:or "=" "+"))

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
       (reverse (cons next tokens))]
      [else (loop (cons next tokens))])))

(define (plain-token? token)
  (or (token-number? token)
      (token-identifier? token)))

(define (search pass find tokens)
  (let loop ([tokens tokens])
    (if (null? tokens)
      #f
      (let ()
        (define current (car tokens))
        (cond
          [(for/fold ([ok #t])
                     ([what pass])
                     (and ok (what (position-token-token current))))
           (loop (cdr tokens))]
          [(find (position-token-token current)) tokens]
          [else #f])))))

;; returns a tree and an unparsed tree
(define (parse tokens [delimiter #f] [indent-level 0])
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

          [(struct* position-token ([token (? token-colon?)]
                                    [start-pos start]
                                    [end-pos end]))

           ;; we might be in an array-splicing operation
           ;; a[2:5]
           (if (eq? delimiter 'brackets)
             (loop (append tree (list '%colon))
                   (cdr tokens))

             ;; otherwise we are in some sort of block
             ;; if x == 1:
             ;;    ....
             (let ()
               ;; check to see if there are other tokens on this line besides
               ;; just whitespace
               (define single-line? (search (list token-tab? token-space?)
                                            (lambda (t)
                                              (not (token-newline? t)))
                                            (cdr tokens)))
               (if single-line?
                 (error 'parse "handle single line")
                 (let ()
                   (define next-line (search (list token-tab? token-space?)
                                             token-newline?
                                             (cdr tokens)))
                   (define spaces (search (list token-space?)
                                          (lambda (i)
                                            (not (token-space? i)))
                                          (cdr next-line)))
                   (define-values (sub-tree rest) (parse spaces #f (- (length next-line) (length spaces))))
                   (loop (append tree (list '%colon `(%block ,@sub-tree)))
                         rest)))))]

          [(struct* position-token ([token (? token-left-paren?)]
                                    [start-pos start]
                                    [end-pos end]))
           (define-values (sub-tree unparsed) (parse (cdr tokens) 'parens indent-level))
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
           (define-values (sub-tree unparsed) (parse (cdr tokens) 'bracket indent-level))
           (loop (append tree (list `(#%brackets ,@sub-tree)))
                 unparsed)]

          [(struct* position-token ([token (? token-right-bracket?)]
                                    [start-pos start]
                                    [end-pos end]))
           (when (not (eq? delimiter 'bracket))
             (error 'read "unexpected `]' seen"))
           (values tree (cdr tokens))]

          [(struct* position-token ([token (? token-space?)]
                                    [start-pos start]
                                    [end-pos end]))
           (loop tree (cdr tokens))]

          [(struct* position-token ([token (or (? token-newline?)
                                               (? token-eof?))]
                                    [start-pos start]
                                    [end-pos end]))
           (define-values (sub-tree more) (parse (cdr tokens) delimiter indent-level))
           (values (append (list `(%line ,@tree)) sub-tree)
                   more)]

          [(struct* position-token ([token (and token (or (? token-tab?)))]
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
                '((%line 1)))

  (check-equal? (python-read-string "x")
                '((%line x)))

  (check-equal? (python-read-string #<<HERE
x = 5
HERE
)
                '((%line x = 5)))

(check-equal? (python-read-string #<<HERE
(x)
HERE
)

                '((%line (#%parens x))))

  (check-equal? (python-read-string #<<HERE
x = [for blah(z) in burger]
HERE
)

                '((%line x = (#%brackets for blah (#%parens z) in burger))))

  (check-equal? (python-read-string #<<HERE
def foo(x):
    return x + 1
HERE
)

                '(def foo (#%parens x) %colon
                      (%block (%line return x + 1))))

  )
