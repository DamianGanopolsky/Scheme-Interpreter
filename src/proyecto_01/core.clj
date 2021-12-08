;;(ns proyecto-01.core
;;(:gen-class))

;;Cuando tenga el jar -> Se va a llamar al main y este main al repl
;;(defn -main
  ;;  "Funcion ppal del Interprete de Scheme -> Va a"
    ;;[& args]
    ;;(repl)
;;)


;;Aca abajo tiene que estar todo el proyecto


(ns proyecto-01.core
(:gen-class))
(declare ejemplo)
(declare es-el-doble?)
(declare spy)
(defn -main
"Ejemplo de Proyecto en Clojure"
[& args]
(ejemplo))
(defn ejemplo []
(let [mensaje-1 (do (print "Ingrese un numero: ") (flush)),
n1 (read),
mensaje-2 (do (print "Ingrese el doble del numero anterior: ") (flush)),
n2 (read),
salida (println (str "Ud." (if (es-el-doble? (spy "primer numero ingresado" n1) (spy "segundo num ingresado" n2)) " " " no ") "sabe calcular!"))]
(do (print "Presione Enter...") (flush) (read-line) (read-line) 'Chau!)))
(defn es-el-doble? [a b]
(= (* 2 a) b))
(defn spy
([x] (do (prn x) x))
([msg x] (do (print msg) (print ": ") (prn x) x)))

;; en vez de n1, puedo poner (spy "prim num ing" n1)