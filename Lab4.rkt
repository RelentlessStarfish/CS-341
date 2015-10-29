#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Streams (infinite lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/stream)

; defining a stream (infinite list) of natural numbers
; uses "lazy" evaluation
(define naturals
  (let helper ((n 0))
    (stream-cons n
                 (helper (+ n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mutators for stream and list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-stream->list
; constructs a list of the first n items from a stream
;
(define my-stream->list
  (lambda (str n)
    (cond ((stream-empty? str) '())
          ((zero? n) '())
          (else (cons (stream-first str)
                      (my-stream->list (stream-rest str) (- n 1)))))))

; my-list->stream
; constructs a stream from a list
;
(define my-list->stream
  (lambda (list)
    (cond ((null? list) empty-stream)
          (else (stream-cons (car list)
                             (my-list->stream (cdr list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; other functions on streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; adding two streams
(define stream-add
  (lambda (str1 str2)
    (cond ((stream-empty? str1) empty-stream)
          ((stream-empty? str2) empty-stream)
          (else (stream-cons (+ (stream-first str1) (stream-first str2))
                             (stream-add (stream-rest str1) (stream-rest str2)))))))

; map function on streams
(define my-stream-map
  (lambda (func str)
    (cond ((stream-empty? str) empty-stream)
          (else (stream-cons (func (stream-first str)) (my-stream-map func (stream-rest str)))))))

  
  ; filter function on streams
  (define my-stream-filter
    (lambda (test? str)
      (cond ((stream-empty? str) empty-stream)
            ((not (test? (stream-first str))) (my-stream-filter test? (stream-rest str)))
            (else (stream-cons (stream-first str) (my-stream-filter test? (stream-rest str)))))))


  ; filter function on streams other version!
  ;(define my-stream-filter
  ;  (lambda (test? str)
  ;    (cond ((stream-empty? str) empty-stream)
  ;          ((not (test? (stream-first str))) (stream-cons '() (my-stream-filter test? (stream-rest str))))
  ;          (else (stream-cons (stream-first str) (my-stream-filter test? (stream-rest str)))))))
  
  
  ; defining a stream of fibonacci numbers
  ;  FIB(n) = FIB(n-1) + FIB(n-2) if n > 1
  ;  FIB(1) = 1
  ;  FIB(0) = 0
  (define fibonaccis
    (let helper ((n 0))
      (stream-cons (fibonacci-function n) (helper (+ n 1)))))

  (define fibonacci-function
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fibonacci-function (- n 1)) (fibonacci-function (- n 2)))))))
  
  ; defining a stream of (positive) rational numbers
  ; a "rational" number is a pair (a . b) where a and b are positive integers
  ; note: the stream should not contain any repeats
  ; example:
  ;  (1 . 1) (1 . 2) (2 . 1) (1 . 3) (3 . 1) (1 . 4) (2 . 3) (3 . 2) (4 . 1) (1 . 5) ... 
  (define rationals
    (let helper ((a 1) (b 1))
      (cond ((= b 0) (stream-cons (list 1 a) (helper 1 (+ a 1))))
            (else (stream-cons (list a b) (helper (+ a 1) (- b 1)))))))
  
  ;;;;;;;;;;;
  ; test jig
  ;;;;;;;;;;;
  
  (define test-me
    (lambda (n)
      (list (my-stream->list rationals n) 
            (my-stream->list fibonaccis n)
            (my-stream->list (my-stream-map sqr naturals) n)
            (my-stream->list (my-stream-filter even? fibonaccis) n))))
