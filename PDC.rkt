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

        
  


(PDC-sol 5 '(1 1))

#|(PDC-test 5 '((1 16 21 10 7)
              (22 11 8 15 20)
              (17 2 23 6 9)
              (12 25 4 19 14)
              (3 18 13 24 5)))

(PDC-test 5 '((1 14 9 20 3)
              (24 19 2 15 10)
              (13 8 23 4 21)
              (18 25 6 11 16)
              (7 12 17 22 5))

(PDC-test 5 '((1 16 21 10 7)
              (22 11 8 15 20)
              (17 2 23 6 9)
              (12 25 4 19 14)
              (3 18 13 24 4)) |#

;                   ______________________________________________________________________________
;__________________/ PDC-Todas


(define (PDC-todas size initial)
  (todas_aux size initial '() (PDC-sol size initial)))

(define (todas_aux size initial soluciones sol) ;soluciones = matriz de matrices (soluciones), sol = matriz solucion actual
  (cond((= (getsize soluciones) 5) soluciones)
       ((eq? (miembro sol soluciones) #f)
        (displayln sol)
        (todas_aux size initial (append soluciones sol) (PDC-sol3 size initial)))
       (else (todas_aux size initial soluciones ((sol_aux3 size (list initial) (possibleMoves initial size (list initial)) initial))))))

;Function that generates a third possible solution
(define (PDC-sol3 size initial)
  (sol_aux3 size (list initial) (possibleMoves initial size (list initial)) initial))

;Auxiliary function for recursion
(define (sol_aux3 size mov pmoves random)
  (cond((equal? (getsize mov) (* size size)) mov) 
       ((null? mov) "No hay solucion, fin del tour")
       ((null? pmoves)
        ;(display "tercer vacio") 
        (sol_aux3 size mov (eliminar pmoves random) (randomElement (eliminar pmoves random))))
       ((> (getsize mov) 4) (sol_aux size (append mov (list (car (quicksort pmoves size mov)))) (possibleMoves (car (quicksort pmoves size mov)) size (append mov (list (car (quicksort pmoves size mov)))))))
       (else
        (sol_aux3 size (append mov (list random)) (possibleMoves random size (append mov (list random))) (randomElement (possibleMoves random size (append mov (list random)))))
       )))


;Function that picks a random element from a list
(define (randomElement list)
  (display (getsize list))
  (cond((equal? (getsize list) 0) '(0,0))
       (else (getElement list (random (getsize list))))
))

;Function that gets an element of a list given an index
(define (getElement list index)
  (cond ((null? list) (display "Index out of bounds"))
        ((= index 0) (car list))
        (else (getElement (cdr list) (- index 1)))))

(define (eliminar lista ele)
  (cond ((null? lista) '())
        ((number? lista) (list'()))
        ((equal? ele (car lista)) (eliminar (cdr lista) ele))
        (else (cons (car lista) (eliminar (cdr lista) ele)))))

;(PDC-todas 5 '(1 1))