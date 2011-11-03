#lang racket/base

(require racket/list
         "perf.rkt")


(define (time-process command . args)
  (define start (current-inexact-milliseconds))
  (define-values (sub in out err) (apply subprocess #f #f #f command args))
    
  (let loop ([log empty])
    (let ([next (sync sub in)])
      (cond [(and (equal? next in) (not (port-closed? in)))
             (loop (let ([res (read in)])
                     (if (equal? eof res)
                         log
                         (cons res log))))]
            [else
             (close-input-port in)
             (close-output-port out)
             (close-input-port err)
             (unless (equal? (subprocess-status sub) 0)
               (error 'time-process "exited with code ~v" (subprocess-status sub)))
             (values (-(current-inexact-milliseconds) start) (reverse log))]))))


(define (time-multiple count command . args)
  (let loop ([count count]
             [results empty])
    (if (> count 0)
        (loop (sub1 count) (cons (apply time-process command args) results))
        results)))
  

(define-values (time logs) (time-process "C:\\Program Files\\Racket\\racket.exe" "-t" "perf.rkt" "-m" "build-list.rkt"))

(map time-log (split-log logs))

  #|
(define base-times (time-multiple 10 "C:\\Program Files (x86)\\Racket\\racket.exe" "exit-base.rkt"))
  

(define racket-times (time-multiple 10 "C:\\Program Files (x86)\\Racket\\racket.exe" "exit-racket.rkt"))


(define (avg . args)
  (/ (apply + args) (length args)))

(define (compute times)
  (values (* 1.0 (apply avg times)) (apply min times) (apply max times)))
  


(compute base-times)


(compute racket-times)
|#

  
  
