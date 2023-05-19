#lang racket/gui

(require racket/gui)
(require racket/draw)
(require racket/gui/base)
(require "PDC.rkt")


(define frame (new frame%
                   [label "KnightTour"]
                   [width 1100]
                   [height 800]))
(define target (make-bitmap 1100 800))
(define image-file "chess-knight.png")
(define image (read-bitmap image-file))
(define texto-input (make-parameter ""))
(define matrix 0)
(define (button-callback b e)
  (let ((text matrix) )
  
  (PDC-Paint 5 '((1 14 9 20 3)(24 19 2 15 10)(13 8 23 4 21)(18 25 6 11 16)(7 12 17 22 5)))))

(define dc (new bitmap-dc% [bitmap target]))
(new button% [parent frame]
             [label "Check"]
             [callback button-callback]
             )


; matrix chessboard generator
(define color 0)

;                           __________________________________________________________________________
;__________________________/ PDC-Paint

(define (PDC-Paint num userSol)
 
  
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc clear)
                (display num)
                (set! matrix num)
                (let ([Y 20] [X 20] [color 0] [num matrix] [colorChess matrix])
                (send dc set-scale 4 4)
                (send dc set-text-foreground "Sea Green")
                (for ([i (range 1 (+ num 1))])
                  (for ([a (range 1 (+ num 1))])
                   (cond
                      [(= 1 color)
                       (send dc set-brush "Dark Sea Green" 'solid)
                       (send dc set-pen "Dark Sea Green" 1 'solid)
                       (send dc draw-rectangle (* X a) (* Y i) 20 20)
                       (set! color 0)]
                      [(= 0 color)
                       (send dc set-brush "white" 'solid)
                       (send dc draw-rectangle (* X a) (* Y i) 20 20)
                       (set! color 1)]
                      ))
                  (cond
                    [(equal? (zero? (modulo colorChess 2)) (zero? (- color 0)))
                     (set! color 1)]
                    [else
                     (set! color 0)
                     ])
                  ))
                (define WIDTH 400)
                (send dc draw-text "Knight's Tour" 0 0)
                (define finalResult (getMov num userSol))
                (adjustSol finalResult dc)
            )]))



;adjust  solution to specific canvas place

(define (adjustSol sol dc)
   (let ((newSol
         (map (lambda (fila)
                (map (lambda (elemento)
                           (* elemento 20))
                     fila))
              sol)))
     (display newSol)
    (animation newSol dc)))


;knight tour solution

(define x-af 20)
(define y-af 20)
(define x-be 0)
(define y-be 0)

(define(animation sol dc)
  (send dc set-pen "Dark Gray" 2 'solid)
  (cond[(null? sol)
        (display "finished")
       ][(= 1 (length sol))
          (set! x-af x-be)
          (set! y-af y-be)
          (set! x-be (list-ref (list-ref sol 0)0))
          (set! y-be (list-ref (list-ref sol 0)1))
          (send dc draw-line (+ 10 x-be) (+ 10 y-be) (+ 10 x-af) (+ 10 y-af))
          ]
       [else

          (display (list-ref (list-ref sol 0)0))
          (display (list-ref (list-ref sol 0)1))
          (set! x-be (list-ref (list-ref sol 0)0))
          (set! y-be (list-ref (list-ref sol 0)1))
          (set! x-af (list-ref (list-ref sol 1)0))
          (set! y-af (list-ref (list-ref sol 1)1))
          (send dc draw-line (+ 10 x-be) (+ 10 y-be) (+ 10 x-af) (+ 10 y-af)) 
          ;(sleep/yield 0.5)
          
          (animation (cdr sol) dc)
          ]
       )
  )

(define msg (new message% [parent frame]
                          [label "submit "]))

 
; Show the frame 
(send frame show #t)