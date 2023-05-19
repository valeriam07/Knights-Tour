#lang racket

;                   ______________________________________________________________________________
;__________________/ PDC-sol

;size(int) = size x size, initial = (x, y)
(define (PDC-sol size initial)
  ;(sol_aux size (list initial) (possibleMoves initial size (list initial)))
  (solutionBoard size (sol_aux size (list initial) (possibleMoves initial size (list initial))) '() '() 1 1)) ;lista de movimientos -> 1.posicion inicial (initial)

(define (sol_aux size mov pmoves)
  (cond((equal? (getsize mov) (* size size)) mov) ;Se recorrio todo el tablero 
       ((null? mov) "No hay solucion, fin del tour")
       ((null? pmoves)
        (display "vacio") 
        (sol_aux size mov (cdr pmoves)))
       (else
        (sol_aux size (append mov (list (car (quicksort pmoves size mov)))) (possibleMoves (car (quicksort pmoves size mov)) size (append mov (list (car (quicksort pmoves size mov))))))
       )))

;Quicksort -> organiza la matriz de posibles movimientos segun el numero de vecinos de cada uno 
(define (quicksort lista size mov) ;lista = pmoves
  (cond ((null? lista) '())
        (else (append (quick_menores lista (neighbour-count lista size mov) size mov)
                      (list(car lista))
                      (quick_mayores lista (neighbour-count lista size mov) size mov)))))

(define (quick_menores lista pivote size mov)
  (cond ((null? lista) '())
        ((< (neighbour-count lista size mov) pivote) (quicksort (cons (car lista) (quick_menores (cdr lista) pivote size mov)) size mov))
        (else (quick_menores (cdr lista) pivote size mov)) ))

(define (quick_mayores lista pivote size mov)
  (cond ((null? lista) '())
        ((> (neighbour-count lista size mov) pivote) (quicksort (cons (car lista) (quick_mayores (cdr lista) pivote size mov)) size mov))
        (else (quick_mayores (cdr lista) pivote size mov)) ))
  
;Asignar numero de vecinos de cada posible movimiento 
(define (neighbour-count pmoves size mov)
  (getsize (possibleMoves (car pmoves) size mov))) ;retorna numero de vecinos
  
;Ultimo elemento de la lista mov = posicion actual
(define (last-element lst)
  (cond ((null? (cdr lst)) (car lst))
        (else (last-element (cdr lst)))))

  
;Generar lista de posibles movimientos a partir de una posicion
(define (possibleMoves pos size mov)
  (visited (validate (list (cons (- (car pos) 2) (cons (+ (cadr pos) 1) '()))
                           (cons (- (car pos) 2) (cons (- (cadr pos) 1) '()))
                           (cons (- (car pos) 1) (cons (+ (cadr pos) 2) '()))
                           (cons (- (car pos) 1) (cons (- (cadr pos) 2) '()))
                           (cons (+ (car pos) 1) (cons (+ (cadr pos) 2) '()))
                           (cons (+ (car pos) 1) (cons (- (cadr pos) 2) '()))
                           (cons (+ (car pos) 2) (cons (+ (cadr pos) 1) '()))
                           (cons (+ (car pos) 2) (cons (- (cadr pos) 1) '()))) size) mov))
       
;Eliminar movimientos invalidos/fuera del tablero
(define (validate pmoves size)
  (cond ((null? pmoves) '())
        ((or(< (caar pmoves) 1) (> (caar pmoves) size) (< (cadar pmoves) 1) (> (cadar pmoves) size))(validate (cdr pmoves) size))
        (else (cons (car pmoves) (validate (cdr pmoves) size)))))

;Eliminar movimientos hacia casillas ya visitadas
(define (visited pmoves mov)
  (cond ((null? pmoves) '()) ;se recorrio todo pmoves -> fin, no se repite
        ((eq? (miembro (car pmoves) mov) #t) (visited (cdr pmoves) mov)) ;se encontro una coincidencia = eliminar y seguir
        (else (cons (car pmoves) (visited (cdr pmoves) mov)))))

(define (miembro pos mov) ;pos = elemento en pmoves, mov = matriz de movimientos
  (cond ((null? mov) #f)
        ((and (equal? (car pos) (caar mov)) (equal? (cadr pos) (cadar mov))) #t)
        (else (miembro pos (cdr mov)))))

;Obtener numero de filas de una matriz
(define (getsize lst)
  (getsize_aux lst 0))

(define (getsize_aux lst i)
  (cond((null? lst) i)
       (else (getsize_aux (cdr lst) (add1 i)))))

(define (solutionBoard size sol row board i j)
  (cond((> i size) (printMatrix board))
       ((> j size) (solutionBoard size sol '() (append board (list row)) (add1 i) 1))
       (else (solutionBoard size sol (append row (list (getSequence (list i j) sol 1))) board i (add1 j)))))

;Retorna posicion dentro de la secuencia de movimientos
(define (getSequence ele matrix i)
  (cond((null? matrix) '()) ;no se encontro el elemento
       ((equal? ele (car matrix)) i)
       (else (getSequence ele (cdr matrix) (add1 i)))))

(define (printMatrix matrix)
  (cond((null? matrix) " -☆- ")
       (else (displayln (car matrix))
             (printMatrix (cdr matrix)))))
 

;                   ______________________________________________________________________________
;__________________/ PDC-test

(define (PDC-test size sol)
  (cond((equal? (getsize sol) size) (test_aux size (getMov size sol) (list (car sol))))
       (else "No es solucion el tamaño no calza")
  ))

(define (test_aux size sol mov)
  (cond((or (null? mov) (null? sol)) "No es solucion")
       ((equal? (getsize sol) 1) "si es una posible solucion") ;llego al ultimo movimiento
       ((eq? (miembro (cadr sol) (possibleMoves (car sol) size mov)) #t) (test_aux size (cdr sol) (append mov (list(cadr sol)))))
       (else "No es solucion")))

(define (getMov size sol)
  (getMov_aux size sol '() 1)
  )

(define (getMov_aux size sol mov i)
  (cond((equal? (getsize mov) (* size size)) mov)
       ((null? (getPos i sol)) '()) ;no se encontro el siguiente elemento -> no es solucion 
       (else (getMov_aux size sol (append mov (list (getPos i sol))) (add1 i)))))

;Retona la posicion en la que se encuentra un elemento -> par ordenado
(define (getPos ele matrix)
  (getPos_aux ele matrix 1))

(define (getPos_aux ele matrix i)
  (cond((null? matrix) '()) ;no se encontro el elemento
       ((> (searchRow ele (car matrix) 1) 0) (append (list i) (list (searchRow ele (car matrix) 1))))
       (else (getPos_aux ele (cdr matrix) (add1 i)))))

(define (searchRow ele row j)
  (cond ((null? row) 0)
        ((equal? ele (car row)) j)
        (else (searchRow ele (cdr row) (add1 j)))))

        
  


;;; (PDC-sol 5 '(1 1))

;;; #|(PDC-test 5 '((1 16 21 10 7)
;;;               (22 11 8 15 20)
;;;               (17 2 23 6 9)
;;;               (12 25 4 19 14)
;;;               (3 18 13 24 5)))

;;; (PDC-test 5 '((1 14 9 20 3)
;;;               (24 19 2 15 10)
;;;               (13 8 23 4 21)
;;;               (18 25 6 11 16)
;;;               (7 12 17 22 5))

;;; (PDC-test 5 '((1 16 21 10 7)
;;;               (22 11 8 15 20)
;;;               (17 2 23 6 9)
;;;               (12 25 4 19 14)
;;;               (3 18 13 24 4)) |#

;                   ______________________________________________________________________________
;__________________/ PDC-Todas

;PDC-Todas

;Function that returns 5 possible solutions, size is the board size and the position is the initial position

(define (PDC-Todas size position)
(getFive (PDC-TodasAux size position) 5))


(define (PDC-TodasAux size position)
  (cond((validInput? size position)
        (getSteps(getMoves position size '() ) size (list position) (current-seconds)))
  (else(
        (current-output-port)
        "Valor de entrada inválido"))))

        ;Gets the first five elements of a list
(define (getFive lst n)
  (cond
    [(or (null? lst) (zero? n)) '()]
    [else (cons (car lst) (getFive (cdr lst) (- n 1)))]))

;in_bounds?, verifies is the position is inside the board limits
;size: boardsize
;x-y: ordered pair position on board
(define (inBoard? x y size)
  (and (and (> x 0)
            (> y 0))
       (and (<= x size )
            (<= y size ))))

;euclidean distance
(define (euclidean point1 point2)
  (sqrt(+(expt (- (car point1) (car point2)) 2)
         (expt (- (cadr point1) (cadr point2)) 2))))

;checks if an element is inside a list
(define (contains? _list element)
  (cond
    ((null? _list) #f)
    ((equal? (car _list) element) #t)
    (else (contains? (cdr _list) element))
    )
  )

;Checks if the knights mov is allowed (neighbor cell)
(define (neighbor point1 point2)
  (equal? (euclidean point1 point2)
          (sqrt 5)))

;;Checks if the solution is valid, if not, returns the last correct movement
(define (solution? solution size)
  (solution_aux solution size '()))

;recursive auxilary function for solution?
(define (solution_aux solution size visited)
  (cond
    ((null? solution)
     (list #f solution))
    ((null? (cdr solution))
     (cond
       ((and (inBoard? (caar solution) (cadar solution) size)
             (equal? (+ (length visited) 1) (expt size 2)))
        (list #t (append visited (list (car solution)))))
     (else (list #f visited))))
    ((and (and (inBoard? (caar solution) (cadar solution) size)
               (neighbor (car solution) (cadr solution)))
          (not (contains? visited (car solution))))
     (solution_aux (cdr solution) size (append visited (list (car solution)))))
    (else (list #f visited))))

;Verifies if the parameters of a function is valid 
(define (validInput? size start)
    (and
      (and (integer? size) (positive? size))
      (and (and (integer? (car start)) (integer? (cadr start)))
           (inBoard? (car start) (cadr start) size))))

;getSteps backtracks in order to solve the problem, moves is the list with possible moves (neighbors) and the path is the tour trveled by the knight
(define(getSteps moves size paths time)
  (cond
    [(> (- (current-seconds) time) 60) (list)];the time parameter is set as a timeout mechanism to terminate the recursion
    [(and (empty? moves) (= (length  paths) (* size size))) (list paths)]
    [(empty? moves) '()]
    [(and (empty? (getMoves (car moves) size paths)) (= (length paths) (- (* size size) 1))) (getSteps (cdr moves) size (append paths (list(car moves))) time)]
    [(empty? (getMoves (car moves) size paths)) (getSteps (cdr moves) size paths time)]
    [else (append (getSteps (getMoves (car moves) size (cons (car moves) paths)) size (append paths (list (car moves))) time)
                  (getSteps (cdr moves) size paths time))]))

;generates a list with coordinates and deletes the ones already visited
(define (getMoves position size paths)
  (deletePaths paths (moveScope size (buildListMoves (movUp_Right (car position) (cadr position))
                                                     (movUp_Left (car position) (cadr position))
                                                     (movDown_Right (car position) (cadr position))
                                                     (movDown_Left (car position) (cadr position))
                                                     (movRight_Top (car position) (cadr position))
                                                     (movRight_Bott(car position) (cadr position))
                                                     (movLeft_Top (car position) (cadr position))
                                                     (movLeft_Bott (car position) (cadr position))))))



;deletes the coordinate already visited and looks pathe in order to verify if any new position was already visited as well and deletes it too.
(define (deletePaths paths moves)
  (cond
    [(empty? moves) '()]
    [(lookingPaths (car moves) paths ) (deletePaths paths (cdr moves))]
    [else (cons (car moves) (deletePaths paths (cdr moves)))]))

;lookingPaths, verifica si la coordenada se encuentra entre los recorridos(paths)
;verifica si la posicion brindada por la lista moves ya se encuentra en el camino del caballo y si es asi la elimina

;Verifies if the coordinate is already in the tour, if its already on it, deletes it.
(define (lookingPaths position paths)
  (cond
    [(= (length paths) 0) #f]
    [(equal? (car paths) position) #t]
    [else (lookingPaths position ( cdr paths ))]))


;Creates a list with only the movements inside the board and using movInside checks and deletes unnecesary positions
(define (moveScope size moves)
  (cond
    [(empty? moves) '()]
    [(movInside? (car moves) size ) (cons (car moves) (moveScope size (cdr moves)))]
    [else (moveScope size (cdr moves))]))


;checks if the position is inside or not the board bounds
(define (movInside? position size)
  (cond
    [(and (and (<= (car position) size)  (<= (cadr position) size))  (and (> (car position) 0) (> (cadr position) 0))) #t]
    [else #f]))

;creates a list with all the coordinates of possible moves according to the allowed Knight movement.
(define (buildListMoves UR UL DR DL RT RB LT LB)
  (list UR UL DR DL RT RB LT LB))

;gets the coordinate up and right
(define (movUp_Right row column)
  (list (- row 2) (+ column 1)))
;gets the coordinate up and left
(define (movUp_Left row column)
  (list (- row 2) (- column 1)))
;gets the coordinate down and right
(define (movDown_Right row column)
  (list (+ row 2) (+ column 1)))
;gets the coordinate down and left
(define (movDown_Left row column)
  (list (+ row 2) (- column 1)))
;gets the coordinate right and up
(define (movRight_Top row column)
  (list (- row 1) (+ column 2)))
;gets the coordinate right and down
(define (movRight_Bott row column)
  (list (+ row 1) (+ column 2)))
;gets the coordinate left and up
(define (movLeft_Top row column)
  (list (- row 1) (- column 2)))
;gets the coordinate left and down
(define (movLeft_Bott row column)
  (list (+ row 1) (- column 2)))





(getFive '(1 2 3 4 5 6 7 8 9 10) 5)

(PDC-Todas 5 '(1 1))