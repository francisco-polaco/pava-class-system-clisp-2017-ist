
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
(defun flatten (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defmacro def-class (ba &rest args)
`(progn
  (defun ,(intern (concatenate 'string "MAKE-" (symbol-name ba))) (&key ,@args)
    (vector ,@args))))
