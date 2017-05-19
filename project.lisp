; group 01 alameda
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

(defun has_inheritance (lst) (not (equal (list-length lst) 1)))

(defun size-fields (cl) (list-length (gethash cl table-fields)))

(defun get-class-to-be-defined (l) (car l))

(defun get-keyargs-of-class (cl)
    (gethash cl table-fields))

(defun get-all-keyargs (class-list)
    (let ((keyargs nil))
        (dolist (el (cdr class-list) keyargs)
            (setf keyargs (append keyargs (get-keyargs-of-class el))))))


(defun get-supers (cl) (gethash cl table-inheritance))

(defun get-flatten-supers (cl-lst)
    (cond
        ((equal cl-lst nil) nil)
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

(defun instance-of-p (c to-test)
    (and (arrayp to-test)
         (or (equalp (symbol-name c) (aref to-test 0))
            (not (equal (member c (get-flatten-supers (list (intern (aref to-test 0))))) nil)))))
    
(defmacro def-class (classname &rest args)
 (let ((field-index 1)
        (class-list (turn-to-list classname))
        (all-keyword-args args))

    ; set fields from a class to table
    (setf (gethash (get-class-to-be-defined class-list) table-fields) args)

    ; writing #fields table
    (setf (gethash (get-class-to-be-defined class-list) table-size) (size-fields (get-class-to-be-defined class-list)))

    ;inheritance table ready
    (setf (gethash (get-class-to-be-defined class-list) table-inheritance) (cdr class-list))

    (setf (gethash (get-class-to-be-defined class-list) table-index) nil)

    ; getting all keyword args ready && index ready
    (cond ((has_inheritance class-list)
                (setf all-keyword-args (unnest (mapcar 'get-keyargs-of-class (get-flatten-supers (list (car class-list))))))
                ;(print "Keywords")
                ;(print all-keyword-args)
                (let   ((acum 1) ; taking into account class name field
                        (list-index-inh nil))
                    (dolist (cl (get-flatten-supers (list (car class-list))))
                        (setf acum (+ acum (gethash cl table-size)))
                        (setf list-index-inh (append list-index-inh (list acum))))
                    (setf list-index-inh (without-last list-index-inh))

                (setf (gethash (get-class-to-be-defined class-list) table-index) list-index-inh))))

    `(progn

        ;make
        (defun ,(intern (concatenate 'string "MAKE-"
                                    (symbol-name (get-class-to-be-defined class-list))))
                        (&key ,@all-keyword-args)

            (vector ,(symbol-name (get-class-to-be-defined class-list)) ,@all-keyword-args ))


        ;get-class
        (defun ,(intern "GET-CLASS") (object)
            (aref object 0))

        ;instance
        (defun ,(intern (concatenate 'string (symbol-name (get-class-to-be-defined class-list)) "?")) (object)
            (let ((c ',(get-class-to-be-defined class-list)))
                (cond ((and (arrayp object)
                            (or (equalp (symbol-name c) (aref object 0))
                                (not (equal (member c (get-flatten-supers (list (intern (aref object 0))))) nil)))) 't)
                    ('t nil))))



        ;getters
        ,(dolist (el all-keyword-args)
            ;(print (symbol-name el))
            ;(print (symbol-name (get-class-to-be-defined class-list)))
            (eval `(defun ,(intern (concatenate 'string (symbol-name (get-class-to-be-defined class-list)) "-" (symbol-name el))) (object)
                (let ((cls-name (get-class object)))
                ;(print cls-name)
                (cond ((not (equal cls-name ,(symbol-name (get-class-to-be-defined class-list))))
                                                        ; (print "ola")
                            ; (print (intern cls-name))
                            ; (print (intern ,(symbol-name (get-class-to-be-defined class-list))))
                            ; (print (get-index-class (intern cls-name)  (intern ,(symbol-name (get-class-to-be-defined class-list)))))

                            (let ((size (get-full-size (intern ,(symbol-name (get-class-to-be-defined class-list)))))
                                  (l-2-array '(,(symbol-name (get-class-to-be-defined class-list))))
                                  (base-index 0))
                                  
                              (setf base-index (get-index-class (intern cls-name)  (intern ,(symbol-name (get-class-to-be-defined class-list)))))
                              ;(print base-index)
                              ;(print size)
                              (dotimes (i size)
                                  (setf l-2-array (append l-2-array (list (aref object (+ base-index i))))))
                              ;(print l-2-array)    
                              (aref (list-to-2d-array l-2-array) ,field-index)))
                            
                      ('t (aref object ,field-index))))))
            (incf field-index)))))

