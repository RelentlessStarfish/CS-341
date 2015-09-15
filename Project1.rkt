#lang racket
(define my-sum
  (lambda (n)
    (if (<= n 0)
        0
        (+ n (my-sum(- n 1))))))

(define my-square-sum
  (lambda (n)
    (if (<= n 0)
        0
        (+ (sqr n) (my-square-sum(- n 1))))))

(define my-better-sum
  (lambda (func n)
    (if (<= n 0)
        0
        (+ (func n) (my-better-sum func (- n 1))))))

(define test2
  (lambda (func func2 n)
    (if (<= n 0)
        '()
        (cons (func func2 n) (test2 func func2 (- n 1))))))

(define my-generic-sum
  (lambda (func)
    (lambda (n)
        (if (<= n 0)
            0
            (+ (func n) ((my-generic-sum func) (- n 1)))))))
