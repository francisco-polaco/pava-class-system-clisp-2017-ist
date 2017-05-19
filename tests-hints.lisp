(load "load.lisp")

(defmacro make-assertions (&body assertions)
  `(progn ,@(mapcar #'(lambda (x) `(assert ,x))
		    assertions)))

(def-class person
  name
  age)

(def-class researcher
  group)

(def-class (student person)
  course)

(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))

(def-class (phd-student ist-student researcher)
    thesis)

(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
  (make-assertions
   (print "pessoa-nome")
   (equal (person-name s) "Paul")
   (print "st-course")
   (equal (student-course s) "Informatics")))

(let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
  (make-assertions
   (ist-student? m)
   (student? m)
   (sportsman? m)
   (print "ist-st-name")
   (equal (ist-student-name m) "Maria")
   (print "person-name")
   (equal (person-name m) "Maria")
   (print "sportsman activity")
   (equal (sportsman-activity m) "Tennis")
   (print "ist activity")
   (equal (ist-student-activity m) "Tennis")))

(let ((b (make-phd-student :name "Brian" :age 28 :course "Informatics" :activity "Soccer" :group "ESW" :thesis "Code Migration")))
  (make-assertions
   (researcher? b)
   (person? b)
   (student? b)
   (sportsman? b)
   (phd-student? b)
   (print "phd tese")
   (equal (phd-student-thesis b) "Code Migration")
   (print "student name")
   (equal (student-name b) "Brian")
   (print "phd student group")
   (equal (phd-student-group b) "ESW")
   (print "phd student name")
   (equal (phd-student-name b) "Brian")))
