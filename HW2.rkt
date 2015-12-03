#lang racket

(define stream-show
  (lambda (str number-of-items)
    (cond ((stream-empty? str) '())
          ((zero? number-of-items) '())
          (else (cons (stream-first str)
                      (stream-show (stream-rest str) (- number-of-items 1)))))))

(define test-me
  (lambda (n)
    (formatter "hollow.txt" "out.txt" n)))

(define formatter 
  (lambda (input-filename output-filename line-length)
    (stream->file output-filename
      (right-justify line-length
        (insert-newlines line-length
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-filename))))))))

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
  (lambda (filename stream)
    (let ((out-port (open-output-file filename #:exists 'replace)))
      (letrec
          ((build-output-stream
            (lambda (stream)
              (if (stream-empty? stream)
                  (begin
                    (close-output-port out-port) 
                    stream)
                  (begin
                    (write-char (stream-first stream) out-port)
                    (build-output-stream (stream-rest stream)))))))
        (build-output-stream stream)))))

; first, change new lines to spaces
; 2nd, change multiple spaces to one space
; trawl stream and reinsert new lines before the word that breaks the limit
; ignore right justify at first
; then justify right
; finally send it to file

;one way to do the justify is to run through each line
;and add another space after the original spaces
;Do this until the length is too long

(define right-justify
  (lambda (line-length stream)
    (letrec
        ((operate
          (lambda (stream count)
            (cond ((stream-empty? stream) stream)
                  ((and (char=? (stream-first stream) #\space)
                        (< (+ count 1) line-length))
                   (stream-cons (stream-first stream) (operate (stream-cons (stream-rest stream) #\space) (+ count 2))))
                  ;this right here is what isn't working... it says that I am passing it a stream when it expects a char
                  ;in the newline? function. HOWEVER... I am passing it the first character in the stream... The stream-first function
                  ;works everywhere else
                  ((newline? (stream-first stream)) (stream-cons (stream-first stream) (operate (stream-rest stream) 0)))
                  (else (stream-cons (stream-first stream) (operate (stream-rest stream) 0)))))))
      (operate stream 0))))
                    
                    

(define remove-extra-spaces
  (lambda (stream)
      (letrec
          ((helper
            (lambda (stream)
              (cond ((stream-empty? (stream-rest stream)) stream)
                    ((and (char=? (stream-first stream) #\space)
                          (char=? (stream-first (stream-rest stream)) #\space))
                     (stream-cons (stream-first stream) (helper (stream-rest (stream-rest stream)))))
                    (else (stream-cons (stream-first stream) (helper (stream-rest stream))))))))
        (helper stream))))

(define newline?
  (lambda (char)
    (char=? #\newline char)))

(define remove-newlines
  (lambda (stream)
    (letrec
        ((helper
          (lambda (stream)
            (cond ((stream-empty? stream) stream)
                  ((newline? (stream-first stream)) (stream-cons #\space (helper (stream-rest stream))))
                  (else (stream-cons (stream-first stream) (helper (stream-rest stream))))))))
      (helper stream))))

(define insert-newlines 
  (lambda (line-length str)
    (letrec
      ((insert 
        (lambda (str count)
	  (if (stream-empty? str)
	      str
	      (let ((n (count-chars-to-next-space str)))
	        (if (and (< count line-length) 
		         (< (+ n count) line-length))
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

(define chars-to-next-newline 
  (lambda (str)
    (letrec
      ((count-ahead
        (lambda (str count)
	  (cond ((stream-empty? str) count)
	        ((char=? (stream-first str) #\newline) count)
	        (else (count-ahead (stream-rest str) (+ count 1)))))))
      (count-ahead str 0))))
