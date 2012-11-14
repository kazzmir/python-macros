#lang racket

(require "read.rkt" racket/cmdline)

(define file (command-line
               #:args (file)
               file))

(with-input-from-file file python-read)
