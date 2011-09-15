#lang racket/base


(require "perf.rkt")


(define-perf-test build-list1 
      (build-list 1 (λ (x) x)))

(define-perf-test build-list10 
  (build-list 10 (λ (x) x)))

(define-perf-test build-list100
  (build-list 100 (λ (x) x)))

(define-perf-test build-list1000
  (build-list 1000 (λ (x) x)))

(define-perf-test build-list10000
  (build-list 10000 (λ (x) x)))

(define-perf-test build-list100000
  (build-list 100000 (λ (x) x)))

(define-perf-test build-list1000000
  (build-list 1000000 (λ (x) x)))

(define-perf-test build-list5000000
  (build-list 5000000 (λ (x) x)))
