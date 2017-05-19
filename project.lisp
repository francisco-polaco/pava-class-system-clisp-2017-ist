(defvar table-fields (make-hash-table))
(defvar table-inheritance (make-hash-table))
(defvar table-index (make-hash-table))
(defvar table-size (make-hash-table))

(defun turn-to-list (l)
    (cond ((not (listp l)) (list l))
        (t l)))

(defun list-to-2d-array (list)
  (map 'array #'identity list))

(defun without-last(l)
    (reverse (cdr (reverse l))))

(defun unnest (x)
  (labels ((rec (x acc)
    (cond ((null x) acc)
      ((atom x) (cons x acc))
      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;(defun get-batata (x)
;    (vector (symbol-name x)  (unnest (gethash x table-fields))))

(defun has_inheritance (lst) (not (equal (list-length lst) 1)))

(defun size-fields (cl) (list-length (gethash cl table-fields)))

(defun get-class-to-tb-defined (l) (car l))


(defun get-keyargs-of-class (cl)
    (gethash cl table-fields))

(defun get-all-keyargs (class-list)
    (let ((keyargs nil))
        (dolist (el (cdr class-list) keyargs)
            (setf keyargs (append keyargs (get-keyargs-of-class el))))))
            ;(setf keyargs (append keyargs (get-all-keyargs (gethash el table-inheritance)))))))


(defun get-supers (cl) (gethash cl table-inheritance))

(defun get-flatten-supers (cl-lst)
    (cond
        ((equal cl-lst nil) nil)
        ;((equal (get-supers (first cl-lst)) nil) (list (first cl-lst)))
        ('t (append (list (first cl-lst)) (get-flatten-supers (get-supers (first cl-lst))) (get-flatten-supers (rest cl-lst))))))


(defun get-full-size (cl)
    (let ((acum (size-fields cl)))
        (dolist (c (get-supers cl) acum)
            (setf acum (+ acum (get-full-size c))))))

(defun get-index-class (cl inh-cl)
    (let ((pos 0)
            (cnt 1)) ; classname
    (dolist (c (get-flatten-supers (list cl)) pos)

        (cond ((equal c inh-cl) (setf pos cnt))
              ('t (setf cnt (+ cnt (size-fields c))))))))
        ;(print pos)
    ;(nth pos (gethash cl table-index))))

(defmacro def-class (classname &rest args)
 (let ((field-index 1)
        (class-list (turn-to-list classname))
        (all-keyword-args args))

    ; set fields from a class to table
    (setf (gethash (get-class-to-tb-defined class-list) table-fields) args)

    ; writing #fields table
    (setf (gethash (get-class-to-tb-defined class-list) table-size) (size-fields (get-class-to-tb-defined class-list)))

    ;inheritance table ready
    (setf (gethash (get-class-to-tb-defined class-list) table-inheritance) (cdr class-list))

    (setf (gethash (get-class-to-tb-defined class-list) table-index) nil)

    ; getting all keyword args ready && index ready
    (cond ((has_inheritance class-list)
                (setf all-keyword-args (unnest (mapcar 'get-keyargs-of-class (get-flatten-supers (list (car class-list))))))
                (print "Keywords")
                (print all-keyword-args)
                (let   ((acum 1) ; taking into account class name field
                        (list-index-inh nil))
                    (dolist (cl (get-flatten-supers (list (car class-list))))
                        (setf acum (+ acum (gethash cl table-size)))
                        (setf list-index-inh (append list-index-inh (list acum))))
                    (setf list-index-inh (without-last list-index-inh))

                (setf (gethash (get-class-to-tb-defined class-list) table-index) list-index-inh))))

    `(progn

        ;make
        (defun ,(intern (concatenate 'string "MAKE-"
                                    (symbol-name (get-class-to-tb-defined class-list))))
                        (&key ,@all-keyword-args)

            (vector ,(symbol-name (get-class-to-tb-defined class-list)) ,@all-keyword-args ))


        ;get-class
        (defun ,(intern "GET-CLASS") (object)
            (aref object 0))

        ;instance TODO
        (defun ,(intern (concatenate 'string (symbol-name (get-class-to-tb-defined class-list)) "?")) (object)
            (cond ((arrayp object)
                    (cond (;(equal (car (array-dimensions object)) (+ (list-length ',args) 1))
                    (equalp ,(symbol-name (get-class-to-tb-defined class-list)) (aref object 0)) 't)
                    ('t nil)))
                ('t nil)))


        ;getters
        ,(dolist (el all-keyword-args)
            ;(print (symbol-name el))
            ;(print (symbol-name (get-class-to-tb-defined class-list)))
            (eval `(defun ,(intern (concatenate 'string (symbol-name (get-class-to-tb-defined class-list)) "-" (symbol-name el))) (object)
                (let ((cls-name (get-class object)))
                ;(print cls-name)
                (cond ((not (equal cls-name ,(symbol-name (get-class-to-tb-defined class-list))))
                                                        ; (print "ola")
                            ; (print (intern cls-name))
                            ; (print (intern ,(symbol-name (get-class-to-tb-defined class-list))))
                            ; (print (get-index-class (intern cls-name)  (intern ,(symbol-name (get-class-to-tb-defined class-list)))))

                            (let ((size (get-full-size (intern ,(symbol-name (get-class-to-tb-defined class-list)))))
                                  (l-2-array '(,(symbol-name (get-class-to-tb-defined class-list))))
                                  (base-index 0))
                                  
                              (setf base-index (get-index-class (intern cls-name)  (intern ,(symbol-name (get-class-to-tb-defined class-list)))))
                              (print base-index)
                              (print size)
                              (dotimes (i size)
                                  (setf l-2-array (append l-2-array (list (aref object (+ base-index i))))))
                              (print l-2-array)    
                              (aref (list-to-2d-array l-2-array) ,field-index)))
                            
                      ('t (aref object ,field-index))))))
            (incf field-index)))))

        ;'(cond ((> field-index (size-fields (get-class-to-be-defined class-list)))))
                ;`(,(intern (concatenate 'string (symbol-name (get-class-to-tb-defined class-list)) "-CLASS")) object)

;(print "PERSON CLASS")
;(def-class person age name)
;(defvar p (make-person :age 10 :name "ola"))
;(print "IST CLASS")
;(def-class (ist person) id)
;(defvar i (make-ist :id 1 :age 20 :name "matos"))

;(def-class animal peso altura)
;(def-class (mamifero animal) pelo leite)
;(def-class musico genero)
;(def-class (croc animal musico) nome)
;(defvar c (make-croc :nome "SuperCroc" :peso 10 :altura 20 :genero "Jazz"))
;(def-class (kanguru mamifero musico) nome)
;(defvar k (make-kanguru :nome "PernaDePau" :pelo "curto" :leite "talvez" :peso 12 :altura 10 :genero "Rock"))
;(print "PHD CLASS")
;(def-class (phd ist) thesis)
;(defvar phd (make-phd :thesis "opah" :id 13 :age 21 :name "ler"))


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
