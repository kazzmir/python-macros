#lang racket/base

(require "read.rkt"
         racket/match
         racket/cmdline)

(define (base-environment)
  (make-hash))

(struct parsed (data))

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
      [(list 'def (and name (? symbol?))
             (list '#%parens args ...) '%colon
             (list '%block body ...)
             rest ...)
       (define out (parsed `(def ,name ,args (unparsed ,@body))))
       (values out rest)]
      [(list (and id (? symbol?)) rest ...)
       (if current
         (values (left current) input)
         (parse rest precedence left (parsed id)))]
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
  (match tree
    [(list 'unparsed stuff ...)
     (define-values (result unparsed)
                    (enforest stuff environment))
     (define out1 (expand (remove-parsed result) environment))
     (define out2 (if (null? unparsed)
                    '()
                    (expand `(unparsed ,@unparsed) environment)))
     `(,out1 ,@out2)]

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
