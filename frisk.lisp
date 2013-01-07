(defclass pais ()
  ((nombre
     :initarg :nombre
     :initform (error "Debe ingresar el nombre del país")
     :reader nombre)
   (duenio
     :initform nil
     :accessor duenio)
   (ejercitos
     :initform 0
     :accessor ejercitos)
   (fronteras
     :initform (make-hash-table)
     :reader fronteras)))

(defclass jugador ()
  ((nombre
     :initarg :nombre
     :initform (error "Debe ingresar el nombre del jugador")
     :reader nombre)
   (paises
     :initform (make-hash-table)
     :reader paises)))

(defun read-map-from-file (file)
  (let ((paises (make-hash-table)) 
        (filecontents (read-from-file file)))
    (dolist (pais (getf filecontents :paises))
      (setf (gethash (car pais) paises)
            (make-instance 'pais :nombre (cdr pais))))
    paises))

(let ((graph nil (loop for frontera in fronteras)) do (setf graph (adjoin frontera graph (setf graph (adjoin (cons (cdr)) frontera (car frontera graph graph)))))))

