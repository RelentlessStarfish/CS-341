#lang racket

(define file->stream 
  (lambda (filename)
    (let ((in-port (open-input-file filename)))
      (letrec
        ((build-input-stream
          (lambda ()
            (let ((ch (read-char in-port)))
              (if (eof-object? ch)
                  (begin
                    (close-input-port in-port)
                    (stream))
                  (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

(define stream->file
  (lambda (stream)
    '()))

(define right-justify
  (lambda (line-length)
    '()))

(define remove-extra-spaces
  (lambda ()
      '()))

(define remove-newlines
  (lambda (line-length)
    '()))


(define formatter 
  (lambda (input-filename output-filename line-length)
    (stream->file output-filename
      (right-justify line-length
        (insert-newlines line-length
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-filename))))))))

; first, change spaces to new lines
; 2nd, change multiple spaces to one space
; trawl stream and reinsert new lines before the word that breaks the limit
; ignore right justify at first
; then justify right
; finally send it to file

;one way to do the justify is to run through each line
;and add another space after the original spaces
;Do this until the length is too long


;theres a problem with an extra space in this code.
(define insert-newlines 
  (lambda (line-length str)
    (letrec
      ((insert 
        (lambda (str count)
	  (if (stream-empty? str)
	      str
	      (let ((n (count-chars-to-next-space str)))
	        (if (and (< count line-length) 
		         (<= (+ n count) line-length))
		    (stream-cons
		      (stream-first str)
		      (insert (stream-rest str) (+ count 1)))
		    (stream-cons
		      #\newline
		      (insert (trim-spaces str) 0))))))))
      (insert (trim-spaces str) 0))))

(define trim-spaces 
  (lambda (str)
    (cond ((stream-empty? str) (stream))
	  ((char=? (stream-first str) #\space)
	   (trim-spaces (stream-rest str)))
	  (else str))))

(define count-chars-to-next-space 
  (lambda (str)
    (letrec
      ((count-ahead
        (lambda (str count)
	  (cond ((stream-empty? str) count)
	        ((char=? (stream-first str) #\space) count)
	        (else (count-ahead (stream-rest str) (+ count 1)))))))
      (count-ahead str 0))))

(define test-me
  (lambda (n)
    (formatter "hollow.txt" "out.txt" n)))
