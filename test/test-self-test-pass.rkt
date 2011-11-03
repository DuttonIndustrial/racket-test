#lang racket/base


(require (planet okcomps/racket-test))

(define-test self-test-pass
  (printf "here is a message from selt-test-pass~n")
  (test-log "hey look another test!"))