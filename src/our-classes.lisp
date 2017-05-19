(load "project.lisp")
(def-class person age name)
(defvar p (make-person :age 10 :name "ola"))
(def-class (ist person) id)
(defvar i (make-ist :id 1 :age 20 :name "matos"))

(def-class animal peso altura)
(def-class (mamifero animal) pelo leite)
(def-class musico genero)
(def-class (croc animal musico) nome)
(defvar c (make-croc :nome "SuperCroc" :peso 10 :altura 20 :genero "Jazz"))
(def-class (kanguru mamifero musico) nome)
(defvar k (make-kanguru :nome "PernaDePau" :pelo "curto" :leite "talvez" :peso 12 :altura 10 :genero "Rock"))
