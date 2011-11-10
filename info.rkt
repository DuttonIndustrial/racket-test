#lang setup/infotab
(define name "racket-test")
(define raco-commands '(("test" (planet okcomps/racket-test/raco/test) "run unit tests" 100)
                        ("perf" (planet okcomps/racket-test/raco/perf) "run performance tests" 99)
                        ("stress" (planet okcomps/racket-test-raco-stress) "run stress test" 98)))