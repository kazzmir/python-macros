#lang racket/base

(require "read.rkt"
         racket/match
         racket/cmdline)

(struct parsed (data))
(struct operator (precedence association binary unary postfix?))

(define (binary-operator precedence association binary [unary #f] [postfix? #f])
    (operator precedence association binary unary postfix?))
                  
(define (unary-operator precedence unary [postfix? #f])
    (operator precedence 'left #f unary postfix?))

(define (environment-value environment name)
    (hash-ref environment name (lambda () #f)))

(define (is-operator? what environment)
    (operator? (environment-value environment what)))

(define python-=
  (binary-operator 0.1 'left (lambda (left right)
                               (parsed `(assign ,left ,right)))))

(define (add-operator! environment name value)
  (hash-set! environment name value))

(define (base-environment)
  (define environment (make-hash))
  (add-operator! environment '= python-=)
  environment)

(define (remove-parsed what)
  (match what
    [(struct parsed (data))
     (remove-parsed data)]
    [(list x ...)
     (for/list ([x x])
       (remove-parsed x))]
    [x x]))

(define-syntax-rule (debug x ...)
                    (printf x ...)
                    #;
                    (void))

(define (parse-all line environment)
  (define-values (result rest)
                 (enforest line environment))
  (debug "Parse all: result = ~a rest = ~a\n" result rest)
  result)

(define (enforest input environment)
  (define (parse input precedence left current)
    (match input
      [(struct parsed (what))
       (values (left input) #f)]
      [(list 'def (and name (? symbol?))
             (list '#%parens args ...) '%colon
             (list '%block body ...)
             rest ...)
       (define out (parsed `(def ,name ,args (unparsed ,@body))))
       (values out rest)]
      
      [(list (and (? (lambda (i)
                       (is-operator? i environment)))
                  operator-symbol) rest ...)
       (define operator (environment-value environment operator-symbol))
       (define new-precedence (operator-precedence operator))
       (define association (operator-association operator))
       (define binary-transformer (operator-binary operator))
       (define unary-transformer (operator-unary operator))
       (define postfix? (operator-postfix? operator))
       (define higher
         (case association
           [(left) >]
           [(right) >=]))

       (if (higher new-precedence precedence)
         (let-values ([(parsed unparsed)
                       (parse rest new-precedence
                              (lambda (stuff)
                                (define right (parse-all stuff environment))
                                (define output
                                  (if current
                                    (if binary-transformer
                                      (binary-transformer (parse-all current environment) right)
                                      (if (and postfix? unary-transformer)
                                        (unary-transformer (list current))
                                        (error 'binary "cannot be used as a binary operator in ~a" operator)))
                                    (if unary-transformer
                                      (unary-transformer right)
                                      (error 'unary "cannot be used as a unary operator in ~a" operator))))

                                output)
                              #f)])
           (parse unparsed precedence left parsed))
         (if unary-transformer
           (if current
             (values (left current) input)
             (error 'low-precedence-unary "implement"))
           (values (left current) input)))]

      [(list (and id (? symbol?)) rest ...)
       (if current
         (values (left current) input)
         (parse rest precedence left (parsed id)))]

      [(list (and number (? number?)) rest ...)
       (if current
         (values (left current) input)
         (parse rest precedence left (parsed number)))]

      [(list) (values (left current) input)]

      #;
      [(list (list line ...) rest ...)
       (values (parse-all line environment) rest)]
      [else (error 'enforest "don't know how to enforest ~a" input)]
      ))
  (parse input 0 (lambda (x) x) #f))

(define (add-lexical! environment name)
  (hash-set! environment name 'lexical))

;; python ast
;;  (import stuff ...)
;;  (class name super stuff ...)
;;  (def name (arg ...) stuff ...)
;;  (if condition then else)
;;  (for z in blah stuf ...)
;;  (try blah catch (exception) blah)
;;  (unparsed stuff ...)
;;  (unparsed-class stuff ...)
(define (expand tree environment)
  (debug "Expand ~a\n" tree)
  (match tree
    [(list 'assign left right)
     (define left* (expand left environment))
     (define right* (expand right environment))
     `(assign ,left* ,right*)]

    [(? number?) tree]

    [(list 'unparsed stuff ...)
     (match stuff
       ;; it might be a list of statements
       [(list (list first ...) rest ...)
        (define result (parse-all first environment))
        (define out1 (expand (remove-parsed result) environment))
        (define out2 (if (null? rest)
                       '()
                       (expand `(unparsed ,@rest) environment)))
        `(,out1 ,@out2)]
       [else
        ;; or just one statement, like a def
        (define-values (result unparsed)
                       (enforest stuff environment))
        (define out1 (expand (remove-parsed result) environment))
        (define out2 (if (null? unparsed)
                       '()
                       (expand `(unparsed ,@unparsed) environment)))
        `(,out1 ,@out2)])]

    [(and id (? symbol?)) id]

    [(list 'def name (list args ...) body)
     (debug "Expand def body ~a\n" body)
     (define body* (expand body environment))
     (add-lexical! environment name)
     `(def ,name (,@args) ,body*)]

    [else (error 'expand "can't expand ~a" tree)]))

(define (expand-python file)
  (define tree (with-input-from-file file python-read))
  (expand `(unparsed ,@tree) (base-environment)))

(define file (command-line #:args (file) file))
(expand-python file)
