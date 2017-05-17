(defvar table (make-hash-table))

(defmacro def-class (classname &rest args)
 (let ((puta 1))
 (setf (gethash classname table) (mapcar 'symbol-name args))
`(progn
  (defun ,(intern (concatenate 'string "MAKE-"
                               (symbol-name classname)))
                  (&key ,@args)
    (vector ,(symbol-name classname) ,@args))
    (defun ,(intern (concatenate 'string (symbol-name classname) "-CLASS")) (person)
        (aref person 0))
    (defun ,(intern (concatenate 'string (symbol-name classname) "?")) (person)
        (cond ((arrayp person) 
                (cond ((equal (car (array-dimensions person)) (+ (list-length ',args) 1)) 
                   (equalp ,(symbol-name classname) (aref person 0)))
                ('t nil)))
            ('t nil)))
        
    ,(dolist (el args)
        (print (symbol-name el))
        (print (symbol-name classname))
        (eval `(defun ,(intern (concatenate 'string (symbol-name classname) "-" (symbol-name el))) (person)
            (aref person ,puta)))
        (incf puta))
    )))

(def-class person age name)
(defvar p (make-person :age 10 :name "ola"))



; (defmacro def-class12 (&body args)
;  (setf symbol (gensym "mysymbol"))
;   (setf symbol (make-hash-table))
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
