(defvar table-fields (make-hash-table))
(defvar table-inheritance (make-hash-table))
(defvar table-index (make-hash-table))


;(defmacro while (test &body body)
 ;   ((do ()
  ;      ((not ,test))
   ;     ,@body)))

(defun turn-to-list (l)
    (cond ((not (listp l)) (list l))
        (t l)))

(defun list-to-2d-array (list)
  (map 'array #'identity list))  
  
(defun unnest (x)
  (labels ((rec (x acc)
    (cond ((null x) acc)
      ((atom x) (cons x acc))
      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
  
(defun get-batata (x)
    (print "yey")
    (vector (symbol-name x)  (unnest (gethash x table-fields)))) 
  
(defun has_inheritance (lst) (not (equal (list-length lst) 1)))

(defun size-fields (cl) (list-length (gethash cl table-fields)))
  
(defmacro def-class (classname &rest args)
 (let ((puta 1)
        (class-list (turn-to-list classname))
        (args-concat args))
        
    ; set fields from a class to table
    (setf (gethash (car class-list) table-fields) args) 
    
    ; getting all keyword args ready && inheritance table ready
    (cond ((has_inheritance class-list) (setf (gethash (car class-list) table-inheritance) (cdr class-list))
                                        (dolist (el (cdr class-list)) 
                                            (setf args-concat (append args-concat (gethash el table-fields))))))
    `(progn

        ;make
        (defun ,(intern (concatenate 'string "MAKE-"
                                    (symbol-name (car class-list))))
                        (&key ,@args-concat)
            
            (vector ,(symbol-name (car class-list)) ,@args-concat ))

        
        ;getClass
        (defun ,(intern (concatenate 'string (symbol-name (car class-list)) "-CLASS")) (person)
            (aref person 0))
        
        ;instance
        (defun ,(intern (concatenate 'string (symbol-name (car class-list)) "?")) (person)
            (cond ((arrayp person) 
                    (cond ((equal (car (array-dimensions person)) (+ (list-length ',args) 1)) 
                    (equalp ,(symbol-name (car class-list)) (aref person 0)))
                    ('t nil)))
                ('t nil)))
                
        
        ;getters       
        ,(dolist (el args-concat)
            ;(print (symbol-name el))
            ;(print (symbol-name (car class-list)))
            (eval `(defun ,(intern (concatenate 'string (symbol-name (car class-list)) "-" (symbol-name el))) (person)
                (aref person ,puta)))
            (incf puta))
        )))

(def-class person age name)
(defvar p (make-person :age 10 :name "ola"))
(def-class (ist person) id)
(defvar i (make-ist :id 1 :age 20 :name "matos"))



; (defmacro def-class12 (&body args)
;  (setf symbol (gensym "mysymbol"))
;   (setf symbol (make-hash-table-fields))
;   (setf (gethash "ola" (car args)) (car (rest args)))
;   )

; (defmacro def-class (x)
;  `(setf ab ,x))
;
; (defmacro def-class (&body args)
; `(progn
;    (defvar ,(car args) nil)
;    (setf ,(car args) '(,(cdar args)))))
; (defun flatten (l)
;   (cond ((null l) nil)
;         ((atom (car l)) (cons (car l) (flatten (cdr l))))
;         (t (append (flatten (car l)) (flatten (cdr l))))))
