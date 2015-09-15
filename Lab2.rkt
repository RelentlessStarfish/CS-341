#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; modular graphics defining functions
;
(define make-frame
  (lambda (title)
    (make-object frame% title)))

(define make-canvas
  (lambda (frame)
    (make-object canvas% frame)))

(define get-drawing-context
  (lambda (canvas)
    (send canvas get-dc)))

(define clear-screen
  (lambda (dc)
    (send dc clear)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; colors
;
(define white (make-object color% "white"))
(define black (make-object color% "black"))
(define red   (make-object color% "red"))
(define blue  (make-object color% "blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pens and their colors
;
(define red-drawing-pen   (make-object pen% red 4 'solid))
(define blue-drawing-pen  (make-object pen% blue 4 'solid))
(define white-drawing-pen (make-object pen% white 4 'solid))

(define no-pen       (instantiate pen%   ("BLACK" 1 'transparent)))
(define white-pen    (instantiate pen%   ("WHITE" 2 'solid)))
(define black-pen    (instantiate pen%   ("BLACK" 1 'solid)))
(define red-pen      (instantiate pen%   ("RED" 2 'solid)))
(define blue-pen     (instantiate pen%   ("BLUE" 2 'solid)))
(define yellow-pen   (instantiate pen%   ("LIGHTGREY" 1 'solid)))

(define no-brush     (instantiate brush% ("BLACK" 'transparent)))
(define white-brush  (instantiate brush% ("WHITE" 'solid)))
(define red-brush    (instantiate brush% ("RED" 'solid)))
(define blue-brush   (instantiate brush% ("BLUE" 'solid)))
(define yellow-brush (instantiate brush% ("YELLOW" 'solid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define set-background-color
  (lambda (drawing-context color)
    (send drawing-context set-background (make-object color% color))))

(define set-foreground-color
  (lambda (drawing-context color)
    (send drawing-context set-pen (instantiate pen% (color 1 'solid)))))

(define draw-point
  (lambda (drawing-context x1 y1)
    (send drawing-context draw-point x1 y1)))

(define draw-line
  (lambda (drawing-context x1 y1 x2 y2)
    (send drawing-context draw-line x1 y1 x2 y2)))

(define draw-rectangle
  (lambda (drawing-context x1 y1 x2 y2 color)
    (send drawing-context set-pen (instantiate pen% (color 1 'solid)))
    (send drawing-context set-brush (instantiate brush% ("black" 'transparent)))
    (send drawing-context draw-rectangle x1 y1 x2 y2)))

(define draw-solid-rectangle
  (lambda (drawing-context x1 y1 x2 y2 color)
    (send drawing-context set-pen (instantiate pen% (color 1 'solid)))
    (send drawing-context set-brush (instantiate brush% (color 'solid)))
    (send drawing-context draw-rectangle x1 y1 x2 y2)))

(define draw-ellipse
  (lambda (drawing-context x y r1 r2 color)
    (set-foreground-color drawing-context color)
    (send drawing-context set-brush (instantiate brush% (color 'solid)))
    (send drawing-context draw-ellipse x y r1 r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-window
;
(define make-window
  (lambda (frame canvas x-size y-size foreground-color background-color)
    (let ((dc (get-drawing-context canvas)))
      (send canvas min-width x-size)
      (send canvas min-height y-size)
      (send canvas stretchable-width #f)
      (send canvas stretchable-height #f)
      (set-background-color dc background-color)
      (set-foreground-color dc foreground-color)
      (send frame show #t)
      (sleep/yield 0.5)
      (clear-screen dc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test jigs
;
(define *my-frame* (make-frame "My Frame"))
(define *my-canvas* (make-canvas *my-frame*))
(define *my-dc* (get-drawing-context *my-canvas*))

; set up a window
(define set-up-window
  (lambda (size)
    (make-window *my-frame* *my-canvas* size size "blue" "black")))

; moving rectangle (decreasing size) towards the origin
(define animate
  (lambda (n speed)
    (let helper ((n n))
      (cond ((zero? n) '())
            (else
             (draw-solid-rectangle *my-dc* (+ 10 n) (+ 10 n) (+ 20 n) (+ 20 n) "blue")
             (sleep/yield speed)
             (draw-solid-rectangle *my-dc* (+ 10 n) (+ 10 n) (+ 20 n) (+ 20 n) "black")           
             (helper (- n 1)))))))

(define home?
  (lambda (x y xHome yHome)
    (cond ((and (equal? x xHome) (equal? y yHome)) #t)
        (else #f))))

(define randomNum
  (lambda ()
    (cond ((equal? (random 2) 1) 1)
          (else -1))))

(define randomColor
  (lambda (ls)
    (list-ref ls (random (length ls)))))

(define boundaryCheck
  (lambda (x max)
    (cond ((< x 0) 0)
          ((> x max) max)
          (else x))))

(define walk!
  (lambda (x y xHome yHome speed)
    (letrec ((helper (lambda (x y)
                       (cond ((home? x y xHome yHome) '())
                             (else
                              (draw-solid-rectangle *my-dc* (+ 10 (boundaryCheck x xHome)) (+ 10 (boundaryCheck y yHome)) 1 1 "blue")
                              ;(sleep/yield speed)
                              (draw-solid-rectangle *my-dc* (+ 10 (boundaryCheck x xHome)) (+ 10 (boundaryCheck y yHome)) 1 1 (randomColor (list "blue" "white" "red")))
                              (helper (+ x (randomNum)) (+ y (randomNum))))))))
    (helper x y))))

(define goHarryGo!
  (lambda (func1 func2)
    func1
    func2))

(define test-me
  (lambda (n)
    (set-up-window (+ n 20))
    (walk! 0 0 n n .01)))
