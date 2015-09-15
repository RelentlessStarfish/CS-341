#lang racket

(define whos-on-first-loop 
	  (lambda (old-context)
	    (let ((costellos-line (read)))
	      (let ((new-context (get-context costellos-line old-context)))
	        (let ((strong-reply (try-strong-cues costellos-line)))
	          (let ((weak-reply (try-weak-cues costellos-line new-context))) 
	               (cond ((not (null? strong-reply))
	                      (writeln strong-reply)
	                      (whos-on-first-loop (get-context strong-reply new-context)))
	                     ((not (null? weak-reply))
	                      (writeln weak-reply)
			      (whos-on-first-loop (get-context weak-reply new-context)))
	                     ((wants-to-end? costellos-line)
	                      (wrap-it-up))
	                     (else 
	                      (writeln (hedge))
	                      (whos-on-first-loop new-context)))))))))
	                      
(define cue-part
  (lambda (pair)
    (car pair)))

(define response-part
  (lambda (pair)
    (cadr pair)))

(define try-strong-cues
  (lambda (sentence)
    (try-strong-cues-helper *strong-cues* sentence)))

(define try-strong-cues-helper
  (lambda (list-of-pairs sentence)
    (cond ((null? list-of-pairs) '())
          ((any-good-fragments? sentence(cue-part(car list-of-pairs)))
           (select-at-random(response-part(car list-of-pairs))))
          (else (try-strong-cues-helper(cdr list-of-pairs) sentence)))))

(define *strong-cues*
	  '( ( ((the names) (their names))
	       ((whos on first whats on second i dont know on third)
 	        (whats on second whos on first i dont know on third)) )

	     ( ((suppose) (lets say) (assume))
	       ((okay) (why not) (sure) (it could happen)) )

	     ( ((i dont know))
	       ((third base) (hes on third)) )
	   ))

(define get-context
  (lambda (sentence old-context)
    old-context))

(define *weak-cues*
  '( ( ((who) (whos) (who is))
       ((first-base)
           ((thats right) (exactly) (you got it)
	    (right on) (now youve got it)))
       ((second-base third-base)
           ((no whos on first) (whos on first) (first base))) )
     ( ((what) (whats) (what is))
       ((first-base third-base)
	   ((hes on second) (i told you whats on second)))
       ((second-base)
	   ((right) (sure) (you got it right))) )
     ( ((whats the name))
       ((first-base third-base)
	   ((no whats the name of the guy on second)
	    (whats the name of the second baseman)))
       ((second-base)
	((now youre talking) (you got it))))
   ))

(define wants-to-end?
  (lambda (sentence)
    '()))

(define wrap-it-up
  (lambda ()
    (writeln '(have a good day))))

(define writeln
  (lambda (ls)
    (write ls)
    (newline)))

(define hedge
  (lambda ()
    (select-at-random *hedges*)))

(define *hedges*
  '((its like im telling you)
    (now calm down)
    (take it easy)
    (its elementary lou)
    (im trying to tell you)
    (but you asked)))

(define select-at-random
  (lambda (ls)
    (list-ref ls (random(length ls)))))

(define test-who
  (lambda ()
    (whos-on-first-loop '())))

(define any-good-fragments?
  (lambda (sentence list-of-cues)
    (cond ((null? list-of-cues) #f)
          ((my-subsequence? (car list-of-cues) sentence) #t)
          (else (any-good-fragments? sentence (cdr list-of-cues))))))

(define my-subsequence?
  (lambda (ls1 ls2)
    (cond ((null? ls1) #t)
          ((null? ls2) #f)
          ((equal? (car ls1) (car ls2)) (my-prefix? (cdr ls1) (cdr ls2)))
          (else (my-subsequence? ls1 (cdr ls2))))))

(define my-prefix?
  (lambda (ls1 ls2)
    (cond ((null? ls1) #t)
          ((null? ls2) #f)
          ((equal? (car ls1) (car ls2)) (my-prefix? (cdr ls1) (cdr ls2)))
          (else #f))))


