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

(define accumulate
  (λ (op base-value func ls)
    (cond ((null? ls) base-value)
          (else (op (func (car ls)) (accumulate op base-value func (cdr ls)))))))

(define frequency
  (λ (item ls)
    (accumulate + 0 (λ (x) (if (equal? x item) 1 0)) ls)))

(define frequency-sort
  (λ (ls)
    (selection-sort ls (λ (x y) (< (frequency x ls) (frequency y ls))))))
