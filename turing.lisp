(defparameter *cinta* nil)
(defparameter *Q* '())     ;states
(defparameter *blank* nil) 
(defparameter *Q0* nil)    ;initial state
(defparameter *F* nil)     ;final state
(defparameter *cabeza* nil);head


(defun knownState()
  "starts the variables for the turing machine"
  (setq *cinta* '(1))
  (setq *Q*
	'((A (0 - R C) (1 X R B) (X - L A))
	  (B (0 X L A) (1 - R B) (X - R B))
	  (C (0 - R D) (X 1 R C))
	  (D )
	  )
	)
  (setq *Blank* '0)
  (setq *Q0* 'A)
  (setq *F* '(D))
  (setq *cabeza* 0)
  )

(defun reset-all()
  (setq *cinta* '())
  (setq *cabeza* '())
  )

(defun getOp(cabeza)
  "gets the operation that will execute"
  (let ((aux 1)
	(operacion nil) )
    (loop for x in *Q*
       while (= aux 1)	 
       if (equalp *Q0* (car x))
       do ;(format t "~A~%" (rest x))
	 (loop for y in (cdr x)
	    while (= aux 1)
	    if (equal (car y) cabeza)
	    do ;(format t "~A~%" y)
	      (setq aux 1)
	      (setq operacion y)  
	      )	 
	 )    
    (rest operacion))  
  )

(defun aumentaCinta(direccion)
  "increases the tape of the machine to the left or right when is needed, this allows to have a very big tape"
  (cond ((equal direccion 'R) (setf *cinta* (append *cinta* (cons *Blank* nil))))
	((equal direccion 'L) (setf *cinta* (append (cons *Blank* nil) *cinta*)))
	)
  )

(defun estadoFinal? (estado)
  "checks if the final state is reached"
  (let ((aux 1))
    (loop for x in *F*
       while (= aux 1)
       if (equal estado x)
       do (setq aux 0)
	 )
    (cond ((equal 0 aux))) )
  )

(defun aplicaOperacion(operacion)
  "aplies the operation"
  (cond ((not (equal '- (car operacion))) (setf (nth *cabeza* *cinta*) (car operacion))) )
  (cond ((equal (cadr operacion) 'R)
	 (cond ((>= (1+ *cabeza*) (length *cinta*)) (aumentacinta 'R) (setq *cabeza* (1+ *cabeza*)))
	       (t (setq *cabeza* (1+ *cabeza*)))
	       )
	 )
	((equal (cadr operacion) 'L)
	 (cond ((= *cabeza* 0) (aumentacinta 'L))
	       (t (setq *cabeza* (1- *cabeza*)))
	       )
	 )
	)
  (setq *Q0* (caddr operacion))  )

(defun turing()
  "main function"
  (let ((tempOp nil))
    (loop while (not (estadoFinal? *Q0*))
       do(setq tempOp (getOp (nth *cabeza* *cinta*)))
	 (format t "q0:~A, op:~A, cinta:~A" *Q0* tempOp *cinta* )
	 (aplicaOperacion tempOp)
	 (format t " -> res: ~A~%" *cinta*);(read-char)
	 )
    )
  )

(knownState)
(turing )


