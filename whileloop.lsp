(defmacro while (test &body body)
    ((do ()
        ((not ,test))
        ,@body)))


(while hungry
  (stare)
  (meow)
  (rub-bgs))



  # vs
(do '()
  ((not hungry))
  (stare)
  (meow)
  (rub-bgs))




macroexpand
macroexpand-1

> (pprint (macrowxpand-1 '(while (able) (high))))


SBCL


(load (compile-file "project.lisp"))


On Lisp by Paul Graham
Practical Common Lisp by Peter Seibel
