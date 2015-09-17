#lang racket
(define smallest
  (λ (ls less?)
    (letrec ((smaller
              (λ (ls less?)
                (cond ((and (<= (length ls) 1) (not (null? ls))) (car ls))
                      ((null? ls) '())
                      ((less? (car ls) (cadr ls)) (smaller (cons (car ls) (cdr (cdr ls))) less?))
                      (else (smaller (cdr ls) less?))))))
      (smaller ls less?))))

(define selection-sort
  (λ (ls less?)
    (letrec ((sort
              (λ (ls less?)
                (cond ((null? ls) '())
                      ((cons (smallest ls less?) (selection-sort (remove (smallest ls less?) ls) less?)))))))
      (sort ls less?))))

; Nick you can ignore this part if you want. I don't care. It was fun anyways. Plus I don't want to do circuits!!!

(define accumulate
  (λ (op base-value func ls)
    (cond ((null? ls) base-value)
          (else (op (func (car ls)) (accumulate op base-value func (cdr ls)))))))

(define frequency
  (λ (item ls)
    (accumulate + 0 (λ (x) (if (= x item) 1 0)) ls)))

; I will leave the last one to you.

; P.s. "λ" looks much better than "lambda" lol 
