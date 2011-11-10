#lang racket/base


(require (planet okcomps/racket-test))

(define-unit-test self-test-pass
  (printf "here is a message from selt-test-pass~n")
  (test-log "hey look another test!")
  (sleep .25)
  (test-log "and another test!")
  (sleep .25)
  (test-log "goobye!"))
  