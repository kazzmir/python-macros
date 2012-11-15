#lang racket/base

(require "read.rkt"
         racket/match
         racket/cmdline)

(define (base-environment)
  (make-hash))

(struct parsed (data))

(define (enforest input environment)
  (define (parse input precedence left current)
    (match input
      [(list 'def (and name (? symbol?))
             (list '#%parens args ...) '%colon
             (list '%block body ...)
             rest ...)
       (define out (parsed `(def ,name ,args (unparsed ,@body))))
       (values out rest)]))
  (parse input 0 (lambda (x) x) #f))

;; python ast
;;  (import stuff ...)
;;  (class name super stuff ...)
;;  (def name (arg ...) stuff ...)
;;  (if condition then else)
;;  (for z in blah stuf ...)
;;  (try blah catch (e Except
;;  (unparsed stuff ...)
;;  (unparsed-class stuff ...)
(define (expand tree environment)
  (match tree
    [(list 'unparsed stuff ...)
     (define-values (parsed unparsed)
                    (enforest stuff environment))
     (expand `(,parsed (unparsed ,@unparsed)) environment)]
    [else (error 'expand "can't expand ~a" tree)]))

(define (expand-python file)
  (define tree (with-input-from-file file python-read))
  (expand `(unparsed ,@tree) (base-environment)))

(define file (command-line #:args (file) file))
(expand-python file)
