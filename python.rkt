#lang racket/base

(require "read.rkt"
         racket/pretty
         racket/list
         racket/match
         racket/cmdline)

(struct parsed (data))
(struct operator (precedence association binary unary postfix?))

(define (binary-operator precedence association binary [unary #f] [postfix? #f])
    (operator precedence association binary unary postfix?))
                  
(define (unary-operator precedence unary [postfix? #f])
    (operator precedence 'left #f unary postfix?))

(define (copy-environment environment)
    (hash-copy environment))

(define (environment-value environment name)
    (hash-ref environment name (lambda () #f)))

(define (is-operator? what environment)
    (operator? (environment-value environment what)))

(define python-and
  (binary-operator 0.5 'left (lambda (left right)
                               (parsed `(op and ,left ,right)))))

(define python-or
  (binary-operator 0.5 'left (lambda (left right)
                               (parsed `(op or ,left ,right)))))


(define python-in
  (binary-operator 0.7 'left (lambda (left right)
                               (parsed `(op in ,left ,right)))))


(define python-is
  (binary-operator 0.5 'left (lambda (left right)
                               (parsed `(op is ,left ,right)))))

(define python-not
  (unary-operator 1.3 (lambda (left)
                        (parsed `(un-op not ,left)))))

(define python-+
  (binary-operator 1 'left (lambda (left right)
                               (parsed `(op + ,left ,right)))))

(define python-%
  (binary-operator 2 'left (lambda (left right)
                               (parsed `(op % ,left ,right)))))

(define python->=
  (binary-operator 0.9 'left (lambda (left right)
                               (parsed `(op >= ,left ,right)))))

(define python-==
  (binary-operator 0.2 'left (lambda (left right)
                               (parsed `(op == ,left ,right)))))

(define python-=
  (binary-operator 0.1 'left (lambda (left right)
                               (parsed `(assign ,left ,right)))))

(define python-+=
  (binary-operator 0.1 'left (lambda (left right)
                               ;; Use assign+
                               (parsed `(assign ,left ,right)))))

(define python-dot
  (binary-operator 100 'left (lambda (left right)
                               (parsed `(dot ,left ,right)))))

(define (add-operator! environment name value)
  (hash-set! environment name value))

(define (base-environment)
  (define environment (make-hash))
  (add-operator! environment '= python-=)
  (add-operator! environment '== python-==)
  (add-operator! environment '+ python-+)
  (add-operator! environment '% python-%)
  (add-operator! environment '>= python->=)
  (add-operator! environment '+= python-+=)
  (add-operator! environment 'and python-and)
  (add-operator! environment 'in python-in)
  (add-operator! environment 'or python-or)
  (add-operator! environment 'not python-not)
  (add-operator! environment 'is python-is)
  (add-operator! environment '%dot python-dot)
  (add-lexical! environment 'python-make-list)
  (add-lexical! environment 'list)
  (add-lexical! environment 'print)
  (add-lexical! environment 'None)
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

(define function-call-precedence 99)
(define list-ref-precedence 80)

(define (parse-all line environment)
  (define-values (result rest)
                 (enforest line environment))
  #;
  (debug "Parse all: result = ~a rest = ~a\n" (parsed-data result) rest)
  (when (and rest (not (null? rest)))
    (error 'parse-all "could not parse entire expression ~a" rest))
  result)

(define (parse-args args environment)
  (define-values (arg rest)
                 (enforest args environment))
  (match rest
    [(list '%comma more ...)
     (cons arg (parse-args more environment))]
    [(list)
     (if arg
       (list arg)
       '())]))

(define (get-args args environment)
  (match args
    [(list) '()]
    [(list '%comma rest ...) (get-args rest environment)]
    [(list (and id (? symbol?)) '= more ...)
     (define-values (expr rest) (enforest more environment))
     ;; TODO: handle expr
     (cons id (get-args rest environment))]
    [(list (and id (? symbol?)) rest ...)
     (cons id (get-args rest environment))]))

;; could be just a plain expression or an array splicing operation
(define (parse-list-ref stuff environment)
  (match stuff
    [(list '%colon more ...)
     (define-values (right rest) (enforest more environment))
     ;; list splicing but nothing on the right side
     (parsed `(array-splice 0 ,right))]
    [(list '%colon)
     (parsed `(array-splice))]
    [(list thing more ...)
     (define-values (left more2) (enforest stuff environment))
     (match more2
       [(list) left]
       [(list '%colon)
        (parsed `(array-splice ,left))]
       [(list '%colon more3 ...)
        (define right (parse-all more3 environment))
        (parsed `(array-splice ,left ,right))])]))

(define (parse-comma-ids what)
  (filter (lambda (i)
            (not (eq? what '%comma)))
          what))

(define (parse-tuple stuff environment)
  (define-values (expr rest) (enforest stuff environment))
  (match rest
    [(list '%comma more ...)
     (cons expr (parse-tuple more environment))]
    [(list) (list expr)]))

(define (parse-excepts input environment)
  (let loop ([all '()]
             [rest input])
    (match rest
      [(list 'except more ...)
       (define-values (first rest2)
                      (enforest more environment))
       (match rest2
         [(list '%comma rest3 ...)
          (define-values (second rest4)
                         (enforest rest3 environment))
          (match rest4
            [(list '%colon (list '%block body ...) rest5 ...)
             (values (cons (parsed `(except ,first ,second (unparsed ,@body)))
                           all)
                     rest5)])]
         [(list '%colon (list '%block body ...) rest3 ...)
          (loop (cons (parsed `(except '() ,first (unparsed ,@body)))
                      all)
                  rest3)]
         )]
      [else (values (reverse all) rest)])))

;; raise URLError, ('ftp error: %s' % msg), sys.exc_info()[2]
(define (parse-raise input environment)
  (let loop ([args '()]
             [input input])
    (match input
      [(list) (values (reverse args) '())]
      [(list '%comma more ...)
       (loop args more)]
      [(list stuff ...)
       (define-values (first rest)
                      (enforest stuff environment))
       (loop (cons first args) rest)])))

(define (parse-elses input environment)
  (let loop ([all '()]
             [input input])
    (match input
      [(list 'elif more ...)
       (define-values (condition rest) (enforest more environment))
       (match rest
         [(list '%colon (list '%block body ...) rest2 ...)
          (define out (parsed `(elif ,condition (unparsed ,@body))))
          (loop (cons out all) rest2)])]
      [(list 'else more ...)
       (define-values (condition rest) (enforest more environment))
       (match rest
         [(list '%colon (list '%block body ...) rest2 ...)
          (define out (parsed `(else ,condition (unparsed ,@body))))
          (loop (cons out all) rest2)])]
      [else
        (values (reverse all) input)])))

(define (enforest input environment)
  (define (parse input precedence left current)
    (match input
      [(struct parsed (what))
       (values (left input) #f)]

      [(list '%colon rest ...)
       (values (left current) input)]

      [(list 'def (and name (? symbol?))
             (list '#%parens args ...) '%colon
             (list '%block body ...)
             rest ...)
       (define real-args (get-args args environment))
       (define out (parsed `(def ,name ,real-args (unparsed ,@body))))
       (values out rest)]

      [(list 'class (and name (? symbol?))
             '%colon
             (list '%block body ...)
             rest ...)
       (define out (parsed `(class ,name () (unparsed ,@body))))
       (values out rest)]

      [(list 'class (and name (? symbol?))
             (list #%parens super ...) '%colon
             (list '%block body ...)
             rest ...)
       (define out (parsed `(class ,name ,super (unparsed ,@body))))
       (values out rest)]

      [(list 'for (and iterator (? symbol?)) 'in more ...)
       (define-values (stuff rest1) (enforest more environment))
       (match rest1
         [(list '%colon (list '%block body ...) rest2 ...)
          (define out (parsed `(for (,iterator) ,stuff (unparsed ,@body))))
          (values out rest2)])]

      [(list 'for
             (and iterator1 (? symbol?)) '%comma
             (and iterator2 (? symbol?))
             'in more ...)
       (define-values (stuff rest1) (enforest more environment))
       (match rest1
         [(list '%colon (list '%block body ...) rest2 ...)
          (define out (parsed `(for (,iterator1 ,iterator2) ,stuff (unparsed ,@body))))
          (values out rest2)])]


      [(list 'if more ...)
       (debug "If\n")
       (define-values (condition rest1) (enforest more environment))
       (match rest1
         [(list '%colon (list '%block inside ...) rest2 ...)
          (define-values (elses rest3) (parse-elses rest2 environment))
          (debug "Enforest elses ~a\n" elses)
          (define out (parsed `(if ,condition (unparsed ,@inside) ,elses)))
          (values out rest3)])]

      [(list 'raise more ...)
       (define-values (args rest) (parse-raise more environment))
       (define out (parsed `(raise ,@args)))
       (values out #f)]

      ;; FIXME: hack to support
      ;;   a, b = 1 + 2
      [(list (and id1 (? symbol?))
             '%comma
             (and id2 (? symbol?))
             '=
             right-side ...)
       (define-values (right rest) (enforest right-side environment))
       (define out (parsed `(assign (,id1 ,id2)
                                    ,right)))
       (values out rest)]

      [(list (and id1 (? symbol?))
             '%comma
             (and id2 (? symbol?))
             '%comma
             (and id3 (? symbol?))
             '=
             right-side ...)
       (define-values (right rest) (enforest right-side environment))
       (define out (parsed `(assign (,id1 ,id2 ,id3)
                                    ,right)))
       (values out rest)]

      [(list 'global (and x (? symbol?)))
       (define out (parsed `(global ,x)))
       (values out #f)]

      [(list 'try '%colon (list '%block try-body ...) rest ...)

       ;; get all the except blocks
       (define-values (excepts rest*)
                      (parse-excepts rest environment))

       (define out (parsed `(try (unparsed ,@try-body) ,@excepts)))
       (values out rest*)]

      [(list 'import (and name (? symbol?)) rest ...)
       (define out (parsed `(import ,name)))
       (values out rest)]

      [(list 'from (and name (? symbol?)) 'import stuff ...)
       (match stuff
         [(list (list '#%parens names ...) rest ...)
          (define names* (filter (lambda (i)
                                   (not (eq? i '%comma)))
                                 (flatten names)))
          (define out (parsed `(import-from ,name ,@names*)))
          (values out rest)]
         [else
           (define imports (parse-comma-ids stuff))
           (define out (parsed `(import-from ,name ,@imports)))
           (values out #f)]
         [else (error 'enforest "handle from ... import ...")])]

      [(list 'print stuff ...)
       (define-values (arg1 rest) (enforest stuff environment))
       (define out `(call print ,arg1))
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

      [(list (list '#%braces inside ...) rest ...)
       (define out `(make-hash ,@inside))
       (parse rest precedence left out)]

      [(list (list '#%brackets inside ...) rest ...)
       (if current
         (let ()
           (define inside* (parse-list-ref inside environment))
           (if (> list-ref-precedence precedence)
             (let ()
               (define out (parsed `(list-ref ,current ,inside*)))
               (parse rest precedence left out))
             (let ()
               (define current* (left current))
               (define out (parsed `(list-ref ,current* ,inside*)))
               (values out rest))))
         (let ()
           ;; TODO: handle list comprehension syntax
           (define inside* (parse-all inside environment))
           (define out (parsed `(call python-make-list ,inside*)))
           (values (left out) rest)))]


       [(list (list '#%parens args ...) more ...)
        (debug "Maybe function call with ~a at ~a\n" current precedence)
        (if current 
          (if (> precedence function-call-precedence)
            (let ()
              (define function (left current))
              (define parsed-args (parse-args args environment))
              (parse more function-call-precedence (lambda (x) x)
                     (parsed `(call ,function ,@parsed-args))))
            (let ()
              (define parsed-args (parse-args args environment))
              (parse more precedence left (parsed `(call ,current ,@parsed-args)))))
          ;; not a function call, just parenthesizing an expression
          (let ()
            (define inner (parse-tuple args environment))
            (parse more precedence left (parsed `(tuple ,@inner)))))]

      [(list 'return stuff ...)
       (if current
         (values (left current) input)
         (let ()
           (define-values (returned rest)
                          (enforest stuff environment))
           (define out (parsed `(return ,returned)))
           (values out rest)))]

      [(list (and id (? symbol?)) rest ...)
       (if current
         (values (left current) input)
         (parse rest precedence left (parsed id)))]

      [(list (and str (? string?)) rest ...)
       (if current
         (values (left current) input)
         (parse rest precedence left (parsed str)))]

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
     (define right* (expand right environment))
     (define left*
       (match left
         [(and (? symbol?)
               (? (lambda (i)
                    (not (eq? (environment-value environment i) 'lexical)))))
          ;; undefined variable, so add it to the lexical scope
          (add-lexical! environment left)
           left]
         [(list ids ...)
          (for ([id ids])
            (add-lexical! environment id))
          ids]
         [else
           ;; defined variable or some other kind of expression, so expand it
           (expand left environment)]))

     `(assign ,left* ,right*)]

    [(list 'op op left right)
     (define left* (expand left environment))
     (define right* (expand right environment))
     `(op ,op ,left* ,right*)]

    [(list 'un-op op what)
     (define what* (expand what environment))
     `(un-op ,op ,what)]

    [(list 'class name super body)
     (add-lexical! environment name)
     (define body-environment (copy-environment environment))
     (define body* (expand body body-environment))
     `(class ,name ,super ,body*)]

    [(list 'raise args ...)
     (define args* (for/list ([arg args])
                     (expand arg environment)))
     `(raise ,@args*)]

    [(list 'tuple stuff ...)
     (define stuff* (for/list ([x stuff])
                      (expand x environment)))
     `(tuple ,@stuff*)]

    [(list 'import-from lib names ...)
     (for ([name names])
       (add-lexical! environment name))
     `(import-from ,lib ,@names)]

    [(list 'list-ref left right)
     (define left* (expand left environment))
     (define right* (expand right environment))
     `(list-ref ,left* ,right*)]

    [(list 'array-splice bottom)
     (define bottom* (expand bottom environment))
     `(array-splice ,bottom)]

    [(list 'global name)
     (add-lexical! environment name)
     `(global ,name)]

    [(list 'try try-block excepts ...)
     (define try-block* (expand try-block environment))
     (define excepts*
       (for/list ([except excepts])
         (expand except environment)))
     `(try ,try-block* ,@excepts*)]

    [(list 'except type arg body)
     ;; TODO: handle when except binds a variable
     (define arg* (expand arg environment))
     (define body-environment (copy-environment environment))
     (add-lexical! body-environment arg)
     (define body* (expand body body-environment))
     `(except ,type ,arg ,body*)]

    [(list 'array-splice bottom top)
     (define bottom* (expand bottom environment))
     (define top* (expand top environment))
     `(array-splice ,bottom ,top)]

    [(list 'for iterator expr body)
     (define new-environment (copy-environment environment))
     (add-lexical! new-environment iterator)
     (define expr* (expand expr environment))
     (define body* (expand body new-environment))
     `(for ,iterator ,expr* ,body*)]

    [(list 'if condition body elses)
     (define condition* (expand condition environment))
     (define body* (expand body environment))
     (define elses* (for/list ([else elses])
                     (expand else environment)))
     `(if ,condition* ,body* ,elses*)]

    [(list 'elif condition body)
     (define condition* (expand condition environment))
     (define body* (expand body environment))
     `(elif ,condition* ,body*)]

    [(list 'else condition body)
     (define condition* (if condition
                          (expand condition environment)
                          condition))
     (define body* (expand body environment))
     `(else ,condition* ,body*)]

    [(list 'call function args ...)
     (define function* (expand function environment))
     (define args* (for/list ([arg args])
                     (expand arg environment)))
     `(call ,function* ,@args*)]

    [(list 'dot left right)
     (define left* (expand left environment))
     ;; dont expand the right side because its a reference
     ;; to an attribute which we don't know about
     `(dot ,left* ,right)]

    [(or (? number?)
         (? string?)) tree]

    [(list 'import what)
     (add-lexical! environment what)
     tree]

    [(list 'return what)
     (define out* (expand what environment))
     `(return ,out*)]

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

    [(and id (? symbol?))
     #;
     (when (not (eq? (environment-value environment id)
                     'lexical))
       (error 'expand "unbound identifier ~a" id))
     id]

    [(list 'def name (list args ...) body)
     (debug "Expand def body ~a\n" body)
     (add-lexical! environment name)
     (define body-environment (copy-environment environment))
     (for ([arg args])
       (add-lexical! body-environment arg))
     (define body* (expand body body-environment))
     `(def ,name (,@args) ,body*)]

    [else (error 'expand "can't expand ~a" tree)]))

(define (expand-python file)
  (define tree (with-input-from-file file python-read))
  (expand `(unparsed ,@tree) (base-environment)))

(define file (command-line #:args (file) file))
(pretty-print (expand-python file))
