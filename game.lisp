(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
(defparameter *columnas* 0)
(defparameter *renglones* 0)
(defparameter *porcentaje* 0)
(defparameter *mundo* '())
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *hilos* 4)
(defparameter *width* 1920)
(defparameter *height* 1080)
(defparameter *porcentajeUnos* 0)
(defparameter *unosGeneracion* 0)
(defparameter *generaciones* 0)
(defparameter X 0)

(defun porcentaje(x)
  "tiene la probailidad x de regresar un 1"
  (cond ((< (- 100 x) (random 100)) 1)
	(t 0)
	)
  )

(defun creaMundo(x y p)
  "crea el mundo tamaño xy y lo llena de 1 o 
cero de acuerdo a p (p se pasa a la funcion procentaje)"
  (setq *mundo* (make-array  (cons x (cons y nil))))
  (setq *columnas* x)
  (setq *renglones* y)
  (setq *porcentaje* p)

					;  (setq incremento (ceiling *columnas* *hilos*))
  (loop for i from 0 to (1- *columnas*)
     do(loop for j from 0 to (1- *renglones*)
	  do (setf (aref *mundo* j i) (porcentaje p))
	    )
       );(sb-thread:make-thread (lambda () (llenamundo incremento inicio p h)))
  p
  )


(defun llenaMundo(cuantos inicio p hilo)
  (loop for i from inicio to (1- (+ inicio cuantos))
     do(loop for j from 0 to (1- *renglones*)
	  do (setf (aref *mundo* j i) (porcentaje p))
	    )
       )
  )

(defun print-thread-info ()
  (let* ((curr-thread sb-thread:*current-thread*)
         (curr-thread-name (sb-thread:thread-name curr-thread))
         (all-threads (sb-thread:list-all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)


(defun convierte (x y)
  "se encarga de convertir el arreglo de *mundo*
en un mapa continuo"
  (let ((cordenada '()))
    (if (< x 0) (setq cordenada (cons (1- *columnas*) cordenada))
	(if (>= x *columnas*) (setq cordenada (cons 0 cordenada))
	    (setq cordenada(cons x cordenada)))
	)
    (if (< y 0) (setq cordenada (cons (1- *renglones*) cordenada))
	(if (>= y *renglones*) (setq cordenada (cons 0 cordenada))
	    (setq cordenada (cons y cordenada)))
	)    
    cordenada)
  )

(defun ventana(x y)
  "obtiene la ventana de 3x3 de la variable *mundo* 
la cual se usara para aplicar las reglas del juego"
  (let ((cord '())
	(vent (make-array '(3 3)))
	(aux 0)
	(auy 0))
    (setq *x* x)
    (setq *y* y)
    (loop for i from (1- x) to (1+ x)
       do(loop for j from (1- y) to (1+ y)
	    do(setq cord (convierte j i))
	      (setf (aref vent aux auy) (aref *mundo* (first cord)(second cord)))
	      (setq auy (1+ auy)))
	 (setq aux (1+ aux))
	 (setq auy 0)
	 )
    vent)
  )

(defun NumVecinos (x)
  "cuenta los vecinos de una ventana de 3x3 ignorando la celula que se encuentra en el centro"
  (let ((contador 0))
    (loop for i from 0 to 2
       do(loop for j from 0 to 2
	    do(if (or (equal (aref x j i) 1) (equal (aref x j i) 'M)) (setq contador (1+ contador)))
	      )
	 )
    (if (or (equal 1 (aref x 1 1)) (equal (aref x 1 1) 'M)) (setq contador (1- contador)))
    contador)
  )

(defparameter *reglas*
  '((1 ((< x 2) M) ((eq (>= x 2)(<= x 3)) 1) ( (> x 3) M))
    (0 ((= x 3) V)))
  )

(defun  obtenRegla(ventana)
  (let ((centro (aref ventana 1 1))
	(bandera nil)
	(auxReglas *reglas*))
    (setq X (numvecinos ventana))
    (loop while (and (not bandera)(> (length auxReglas) 0))
       do(if (equal centro (caar auxReglas))   
	     (setq bandera t) (setq auxReglas (rest auxReglas)))	 
	 )
    (setq bandera nil)
    (loop for a in (cdar auxReglas)
       while (not bandera)
       do(if (eval (car a)) (progn (setq bandera t)(setf (aref *mundo* *x* *y*) (cadr a))))
	 ))
  )




(defun paso()
  (setq *generaciones* (1+ *generaciones*))
  (loop for i from 0 to (1- *columnas*)
     do(loop for j from 0 to  (1- *renglones*)
	  do(obtenRegla (ventana j i))
	    )
       )
  (limpiaestado)
  )

(defun limpiaEstado()
  (loop for i from 0 to (1- *columnas*)
     do(loop for j from 0 to (1- *renglones*)
	  if  (or (eq (aref *mundo* j i) 'M) (eq (aref *mundo* j i) 'V))
	  do
	    (cond
	      ((eq (aref *mundo* j i) 'M) (setf (aref *mundo* j i) 0)
	       (sdl-gfx:draw-box
		(sdl:rectangle
		 :x (* i (floor *width* *renglones*))
		 :y (* j (floor *height* *columnas*))
		 :w (floor *width* *renglones*)
		 :h (floor *height* *columnas*))
		:color sdl:*black*)
	       )
	      ((eq (aref *mundo* j i) 'V) (setf (aref *mundo* j i) 1)
	       (sdl-gfx:draw-box
		(sdl:rectangle
		 :x (* i (floor *width* *renglones*))
		 :y (* j (floor *height* *columnas*))
		 :w (floor *width* *renglones*)
		 :h (floor *height* *columnas*))
		:color sdl:*white*)
	       )	      
	      (t )
	      )	    
	    )
       )
  )


(defun dibuja()
  (loop for i from 0 to (1- *columnas*)do
       (loop for j from 0 to (1- *renglones*)do
	    (if (= (aref *mundo* j i) 0)
		(sdl-gfx:draw-box
		 (sdl:rectangle
		  :x (* i (floor *width* *renglones*))
		  :y (* j (floor *height* *columnas*))
		  :w (1- (floor *width* *columnas*))
		  :h (1- (floor *height* *renglones*)))
		 :color sdl:*black*)
		(sdl-gfx:draw-box
		 (sdl:rectangle
		  :x (* i (floor *width* *renglones*))
		  :y (* j (floor *height* *columnas*))
		  :w (1- (floor *width* *columnas*))
		  :h (1- (floor *height* *renglones*)))
		 :color sdl:*white*)
		)
	    )
       )
  )

(defun escala()
  
  )

(defun gameoflife ()
  (setq *generaciones* 0)
  (sdl:with-init ()
    (sdl:window *width* *height* :title-caption "Juego de la vida")
					;    (setf (sdl:frame-rate) 1)
    (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
    (dibuja)
    (paso)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
					;      (:MOUSE-BUTTON-UP-EVENT ())
      (:idle () (sdl:update-display)(paso))
      )))


(creaMundo 1900 1000 50)
(gameoflife)


