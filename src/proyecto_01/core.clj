(ns proyecto-01.core
(:gen-class))
(declare repl)
(declare es-el-doble?)
(declare spy)
(defn -main
"Ejemplo de Proyecto en Clojure"
[& args]
(repl))

(require '[clojure.string :as st :refer [blank? starts-with? ends-with? lower-case]]
         '[clojure.java.io :refer [delete-file reader]]
         '[clojure.walk :refer [postwalk postwalk-replace]])



(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-set!)
(declare evaluar-quote)
(declare evaluar-define)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-car)
(declare fnc-cdr)
(declare fnc-env)
(declare fnc-not)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-list?)
(declare fnc-read)
(declare fnc-mayor)
(declare fnc-menor)
(declare fnc-null?)
(declare fnc-sumar)
(declare fnc-append)
(declare fnc-equal?)
(declare fnc-length)
(declare fnc-restar)
(declare fnc-display)
(declare fnc-newline)
(declare fnc-reverse)
(declare fnc-mayor-o-igual)

; Funciones auxiliares

(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare leer-entrada)
(declare actualizar-amb)
(declare restaurar-bool)
(declare generar-nombre-arch)
(declare nombre-arch-valido?)
(declare controlar-aridad-fnc)
(declare proteger-bool-en-str)
(declare verificar-parentesis)
(declare generar-mensaje-error)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-de-cond)
(declare evaluar-secuencia-en-cond)


; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra > y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve 'Goodbye! (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
;;ESTO ES EL ANALISIS LEXICO, ACA por ejemplo agrego el *
(defn repl
  "Inicia el REPL de Scheme."
  ([]
   (println "Interprete de Scheme en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2021") (prn)
   (println "Inspirado en:")
   (println "  SCM version 5f2.")                        ; https://people.csail.mit.edu/jaffer/SCM.html
   (println "  Copyright (C) 1990-2006 Free Software Foundation.") (prn) (flush)
   (repl (list 'append 'append 'car 'car 'cdr 'cdr 'cond 'cond 'cons 'cons 'define 'define
               'display 'display 'env 'env 'equal? 'equal? 'eval 'eval 'exit 'exit
               'if 'if 'lambda 'lambda 'length 'length 'list 'list 'list? 'list? 'load 'load
               'newline 'newline 'nil (symbol "#f") 'not 'not 'null? 'null? 'or 'or 'quote 'quote
               'read 'read 'reverse 'reverse 'set! 'set! (symbol "#f") (symbol "#f")
               (symbol "#t") (symbol "#t") '+ '+ '- '- '< '< '> '> '>= '>=)))
  ([amb]
   (print "> ") (flush)
   (try
     (let [renglon (leer-entrada)]                       ; READ
          (if (= renglon "")
              (repl amb)
              (let [str-corregida (proteger-bool-en-str renglon),
                    cod-en-str (read-string str-corregida),
                    cod-corregido (restaurar-bool cod-en-str),
                    res (evaluar cod-corregido amb)]     ; EVAL
                    (if (nil? (second res))              ;   Si el ambiente del resultado es `nil`, es porque se ha evaluado (exit)
                        'Goodbye!                        ;   En tal caso, sale del REPL devolviendo Goodbye!.
                        (do (imprimir (first res))       ; PRINT
                            (repl (second res)))))))     ; LOOP (Se llama a si misma con el nuevo ambiente)
     (catch Exception e                                  ; PRINT (si se lanza una excepcion)
                   (imprimir (generar-mensaje-error :error (get (Throwable->map e) :cause)))
                   (repl amb)))))                        ; LOOP (Se llama a si misma con el ambiente intacto)


(defn evaluar
  "Evalua una expresion `expre` en un ambiente. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb]
  ;(spy "ENTRO A EVALUAR CON EXPRE" expre)

  ; e.g: (+ 5 3) -> Entra a Cond
  (if (and (seq? expre) (or (empty? expre) (error? expre))) ; si `expre` es () o error, devolverla intacta
      (list expre amb)                                      ; de lo contrario, evaluarla
      ;Si no se cumple lo del if, entra al cond
      (cond
        ; e.g: (+ 5 3) -> No entra a evaluar escalar ni es define
        (not (seq? expre))    (evaluar-escalar expre amb)       ;  (spy "Evaluar escalar devuelve"(evaluar-escalar expre amb))

        (igual? (first expre) 'cond) (evaluar-cond expre amb)

        (igual? (first expre) 'define) (evaluar-define expre amb)

        (igual? (first expre) 'exit) (evaluar-exit expre amb)

        (igual? (first expre) 'if) (evaluar-if expre amb)

        (igual? (first expre) 'lambda) (evaluar-lambda expre amb)

        (igual? (first expre) 'load) (evaluar-load expre amb)

        (igual? (first expre) 'or) (evaluar-or expre amb)

        (igual? (first expre) 'quote) (evaluar-quote expre amb)

        (igual? (first expre) 'set!) (evaluar-set! expre amb)


         ;
         ;
         ;
         ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada
         ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
         ;
         ;
         ;
         ;Clase: Aca van a estar los que vamos a hacer nosotros: evaluar-or, evaluar-define, etc

        ; En el ambiente -> '+ me da +. Pero despues si modifico una variable por ej, 'x me va a dar 9 o
        ; un lambda

         ; e.g: (+ 5 3) -> Se llama a aplicar con las 2 partes evaluadas. La cabeza es solo el +,
         ;la evaluacion del +,vuelve a ser el +. Evaluo el cuerpo -> 5 me da 5 y 3 me da 3.
         ; Llamo a aplicar con + 

         ;res-eval-2 -> Los argumentos evaluados usando reduce (de izq a derecha). Un argumento puede
         ;tener un set y eso afecta a los que quedan de la derecha.

	    	:else (let [res-eval-1 (evaluar (first expre) amb),
             						 res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x))] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
					              	(aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2))))))


(defn aplicar
  "Aplica la funcion `fnc` a la lista de argumentos `lae` evaluados en el ambiente dado."
  ;e.g: (+ 5 3) -> Llamo a aplicar con el + como fnc, y la lista de args es ( 5 3)
  ([fnc lae amb]
  ;(spy "ENTRO A APLICAR CON FUNCION" fnc)
  ;(spy "ENTRO A APLICAR CON argumentos" lae)
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb))
  ([resu1 resu2 fnc lae amb]
   (cond
     (error? resu1) (list resu1 amb)
     (error? resu2) (list resu2 amb)
     ;Si hay un error aca -> Devuelvo el error
     ;Sino -> Me fijo si es una funcion primitiva o un lambda
     
     ;Una funcion no primitiva tiene fnc algo que no es una secuencia
     ;El resultado va a devolver el 15 y el ambiente
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb) amb)
     :else (aplicar-lambda fnc lae amb))))


(defn aplicar-lambda
  "Aplica la funcion lambda `fnc` a `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  (cond
    (not= (count lae) (count (second fnc))) (list (generar-mensaje-error :wrong-number-args fnc) amb)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb)
    :else (aplicar-lambda-multiple fnc lae amb)))


(defn aplicar-lambda-simple
  "Evalua un lambda `fnc` con un cuerpo simple"
  [fnc lae amb]
  (let [lae-con-quotes (map #(if (or (number? %) (string? %) (and (seq? %) (igual? (first %) 'lambda)))
                                 %
                                 (list 'quote %)) lae),
        nuevos-pares (reduce concat (map list (second fnc) lae-con-quotes)),
        mapa (into (hash-map) (vec (map vec (partition 2 nuevos-pares)))),
        cuerpo (first (nnext fnc)),
        expre (if (and (seq? cuerpo) (seq? (first cuerpo)) (igual? (ffirst cuerpo) 'lambda))
                  (cons (first cuerpo) (postwalk-replace mapa (rest cuerpo)))
                  (postwalk-replace mapa cuerpo))]
        (evaluar expre amb)))


(defn aplicar-lambda-multiple
  "Evalua una funcion lambda `fnc` cuyo cuerpo contiene varias partes."
  [fnc lae amb]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb))))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una `lae` (lista de argumentos evaluados)."
  [fnc lae amb]
  ;(spy "ENTRO A APLICAR PRIMITIVA CON FUNCION" fnc)
  
  (cond

    (= fnc '<) (fnc-menor lae)

    (= fnc '>) (fnc-mayor lae)

    (= fnc '>=) (fnc-mayor-o-igual lae)

    (= fnc '+) (fnc-sumar lae)

    (= fnc '-) (fnc-restar lae)

    ;
    ;
    ; Si la funcion primitiva esta identificada por un simbolo, puede determinarse mas rapido que hacer con ella
    ;
    ;
    (igual? fnc 'sumar) (fnc-sumar lae)

    (igual? fnc 'append)  (fnc-append lae)

    (igual? fnc 'car) (fnc-car lae)

    (igual? fnc 'cdr) (fnc-cdr lae)

    (igual? fnc 'cons) (fnc-cons lae)

    (igual? fnc 'display) (fnc-display lae)

    ; ENV
    (igual? fnc 'env) (fnc-env lae amb) 

    (igual? fnc 'equal?) (fnc-equal? lae)

    ; EVAL es un evaluar, no una funcion primitiva

    (igual? fnc 'length) (fnc-length lae)

    (igual? fnc 'list) (fnc-list lae)

    (igual? fnc 'list?) (fnc-list? lae)

    (igual? fnc 'newline) (fnc-newline lae)

    (igual? fnc 'not) (fnc-not lae)

    (igual? fnc 'null?) (fnc-null? lae)

    (igual? fnc 'read) (fnc-read) ;No le paso lae

    (igual? fnc 'reverse) (fnc-reverse lae)

    ;
    ;
    ; Si la funcion primitiva esta identificada mediante una palabra reservada, debe ignorarse la distincion entre mayusculas y minusculas 
    ;
    ;

    :else (generar-mensaje-error :wrong-type-apply fnc)))

(defn fnc-car
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'car), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'car arg1)
         :else (first arg1))))


(defn fnc-cdr
  "Devuelve una lista sin su 1ra. posicion."
  [lae]
  ;(spy "LAE AACA ES" lae)
  (let [ari (controlar-aridad-fnc lae 1 'cdr), arg1 (first lae)]
       (cond
         (error? ari) ari
         (or (not (seq? arg1)) (empty? arg1)) (generar-mensaje-error :wrong-type-arg1 'cdr arg1)
         :else (rest arg1))))


(defn fnc-cons
  "Devuelve el resultado de insertar un elemento en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 2 'cons), arg1 (first lae), arg2 (second lae)]
       (cond
         (error? ari) ari
					   	(not (seq? arg2)) (generar-mensaje-error :only-proper-lists-implemented 'cons)
					   	:else (cons arg1 arg2))))


(defn fnc-display
  "Imprime un elemento por la termina/consola y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae), arg1 (first lae)]
       (case cant-args
         1 (do (print arg1) (flush) (symbol "#<unspecified>"))
         2 (generar-mensaje-error :io-ports-not-implemented 'display)
         (generar-mensaje-error :wrong-number-args-prim-proc 'display))))


(defn fnc-env
  "Devuelve el ambiente."
  [lae amb]
  (let [ari (controlar-aridad-fnc lae 0 'env)]
       (if (error? ari)
           ari
           amb)))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'length), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'length arg1)
         :else (count arg1))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1)
      ()
      lae))


(defn fnc-list?
  "Devuelve #t si un elemento es una lista. Si no, #f."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'list?), arg1 (first lae)]
       (if (error? ari)
           ari
           (if (seq? arg1)
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-newline
  "Imprime un salto de linea y devuelve #<unspecified>."
  [lae]
  (let [cant-args (count lae)]
       (case cant-args
         0 (do (newline) (flush) (symbol "#<unspecified>"))
         1 (generar-mensaje-error :io-ports-not-implemented 'newline)
         (generar-mensaje-error :wrong-number-args-prim-proc 'newline))))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'not)]
       (if (error? ari)
           ari
           (if (igual? (first lae) (symbol "#f"))
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-null?
  "Devuelve #t si un elemento es ()."
  [lae]
  (let [ari (controlar-aridad-fnc lae 1 'null?)]
       (if (error? ari)
           ari
           (if (= (first lae) ())
               (symbol "#t")
               (symbol "#f")))))


(defn fnc-reverse
  "Devuelve una lista con los elementos de `lae` en orden inverso."
  [lae]
    (let [ari (controlar-aridad-fnc lae 1 'reverse), arg1 (first lae)]
       (cond
         (error? ari) ari
         (not (seq? arg1)) (generar-mensaje-error :wrong-type-arg1 'reverse arg1)
         :else (reverse arg1))))


(defn controlar-aridad-fnc
  "Si la `lae` tiene la longitud esperada, se devuelve este valor (que es la aridad de la funcion).
   Si no, devuelve una lista con un mensaje de error."
  [lae val-esperado fnc]
  (if (= val-esperado (count lae))
      val-esperado
      (generar-mensaje-error :wrong-number-args-prim-proc fnc)))


(defn imprimir
  "Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas
  con comillas) y devuelve su valor. Muestra errores sin parentesis."
  ([elem]
   (cond
     (= \space elem) elem    ; Si es \space no lo imprime pero si lo devuelve
     (and (seq? elem) (starts-with? (apply str elem) ";")) (imprimir elem elem)
     :else (do (prn elem) (flush) elem)))
  ([lis orig]
   (cond
     (nil? lis) (do (prn) (flush) orig)
     :else (do (pr (first lis))
               (print " ")
               (imprimir (next lis) orig)))))


(defn revisar-fnc
  "Si la `lis` representa un error lo devuelve; si no, devuelve nil."
  [lis]
  ;(spy "revisar fnc ocn lis" lis)
   (if (error?  lis) lis nil))


(defn revisar-lae
  
  "Si la `lis` contiene alguna sublista que representa un error lo devuelve; si no, devuelve nil."
  [lis]
  ;(spy "Revisar lae con lis" lis)
   (first (remove nil? (map revisar-fnc (filter seq? lis)))))


(defn evaluar-cond
  "Evalua una expresion `cond`."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :bad-or-missing 'cond expre) amb)
      (let [res (drop-while #(and (seq? %) (not (empty? %))) (next expre))]
            (if (empty? res) 
                (evaluar-clausulas-de-cond expre (next expre) amb)
                (list (generar-mensaje-error :bad-or-missing 'cond (first res)) amb)))))


(defn evaluar-clausulas-de-cond
  "Evalua las clausulas de cond."
  [expre lis amb]
  (if (nil? lis)
	     (list (symbol "#<unspecified>") amb) ; cuando ninguna fue distinta de #f
		    (let [res-eval (if (not (igual? (ffirst lis) 'else))
		                       (evaluar (ffirst lis) amb)
		                       (if (nil? (next lis))
		                           (list (symbol "#t") amb)
		                           (list (generar-mensaje-error :bad-else-clause 'cond expre) amb)))]
		         (cond
		           (error? (first res-eval)) res-eval
		           (igual? (first res-eval) (symbol "#f")) (recur expre (next lis) (second res-eval)) 
		           :else (evaluar-secuencia-en-cond (nfirst lis) (second res-eval))))))


(defn evaluar-secuencia-en-cond
  "Evalua secuencialmente las sublistas de `lis`. Devuelve el valor de la ultima evaluacion."
  [lis amb]
	  (if (nil? (next lis))
	      (evaluar (first lis) amb)
	      (let [res-eval (evaluar (first lis) amb)]
	           (if (error? (first res-eval))
   		           res-eval
  	             (recur (next lis) (second res-eval))))))


(defn evaluar-eval
  "Evalua una expresion `eval`."
  [expre amb]
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE <anon> ...")) amb)
      (let [arg (second expre)]
           (if (and (seq? arg) (igual? (first arg) 'quote))
               (evaluar (second arg) amb)
               (evaluar arg amb)))))


(defn evaluar-exit
  "Sale del interprete de Scheme."
  [expre amb]
  (if (> (count expre) 2) ; si son el operador y mas de 1 argumento
      (list (generar-mensaje-error :wrong-number-args-prim-proc 'quit) amb)
      (list nil nil)))


(defn evaluar-lambda
  "Evalua una expresion `lambda`."
  [expre amb]
  (cond
    (< (count expre) 3) ; si son el operador solo o con 1 unico argumento
          (list (generar-mensaje-error :bad-body 'lambda (rest expre)) amb)
    (not (seq? (second expre)))
          (list (generar-mensaje-error :bad-params 'lambda expre) amb)
    :else (list expre amb)))


(defn evaluar-load
  "Evalua una expresion `load`. Carga en el ambiente un archivo `expre` de codigo en Scheme."
  [expre amb]
  (if (= (count expre) 1) ; si es el operador solo
      (list (generar-mensaje-error :wrong-number-args (symbol "#<CLOSURE scm:load ...")) amb)
      (list (symbol "#<unspecified>") (cargar-arch amb (second expre)))))


(defn cargar-arch
  "Carga y devuelve el contenido de un archivo."
  ([amb arch]
   (let [res (evaluar arch amb),
         nom-original (first res),
         nuevo-amb (second res)]
         (if (error? nom-original)
             (do (imprimir nom-original) nuevo-amb)                 ; Mostrar el error
             (let [nom-a-usar (generar-nombre-arch nom-original)]
                   (if (error? nom-a-usar)
                       (do (imprimir nom-a-usar) nuevo-amb)          ; Mostrar el error
                       (let [tmp (try
                                    (slurp nom-a-usar)
                                    (catch java.io.FileNotFoundException _
                                      (generar-mensaje-error :file-not-found)))]
                            (if (error? tmp)
                                (do (imprimir tmp) nuevo-amb)        ; Mostrar el error
                                (do (spit "scm-temp" (proteger-bool-en-str tmp))
                                    (let [ret (with-open [in (java.io.PushbackReader. (reader "scm-temp"))]
                                                (binding [*read-eval* false]
                                                  (try
                                                    (imprimir (list (symbol ";loading") (symbol nom-original)))
                                                    (cargar-arch (second (evaluar (restaurar-bool (read in)) nuevo-amb)) in nom-original nom-a-usar)
                                                    (catch Exception e
                                                       (imprimir (generar-mensaje-error :end-of-file 'list))))))]
                                          (do (delete-file "scm-temp" true) ret))))))))))
  ([amb in nom-orig nom-usado]
   (try
     (cargar-arch (second (evaluar (restaurar-bool (read in)) amb)) in nom-orig nom-usado)
     (catch Exception _
       (imprimir (list (symbol ";done loading") (symbol nom-usado)))
       amb))))


(defn generar-nombre-arch
  "Dada una entrada la convierte en un nombre de archivo .scm valido."
  [nom]
  (if (not (string? nom))
      (generar-mensaje-error :wrong-type-arg1 'string-length nom)
      (let [n (lower-case nom)]
            (if (nombre-arch-valido? n)
                n
                (str n ".scm")))))    ; Agrega '.scm' al final


(defn nombre-arch-valido?
  "Chequea que el string sea un nombre de archivo .scm valido."
  [nombre] (and (> (count nombre) 4) (ends-with? nombre ".scm")))


(defn evaluar-quote
  "Evalua una expresion `quote`."
  [expre amb]
  ;(spy "ENTRO A EVALUAR QUOTE CON EXPRE:" expre)
  (if (not= (count expre) 2) ; si no son el operador y exactamente 1 argumento
      (list (generar-mensaje-error :missing-or-extra 'quote expre) amb)
       (list (second expre) amb)))


(defn generar-mensaje-error
  "Devuelve un mensaje de error expresado como lista."
  ([cod]
 			(case cod 
         :file-not-found (list (symbol ";ERROR:") 'No 'such 'file 'or 'directory)
         :warning-paren (list (symbol ";WARNING:") 'unexpected (symbol "\")\"#<input-port 0>"))
         ()))
  ([cod fnc]
    (cons (symbol ";ERROR:")
    			(case cod
         :end-of-file (list (symbol (str fnc ":")) 'end 'of 'file)
         :error (list (symbol (str fnc)))
         :io-ports-not-implemented (list (symbol (str fnc ":")) 'Use 'of 'I/O 'ports 'not 'implemented)
         :only-proper-lists-implemented (list (symbol (str fnc ":")) 'Only 'proper 'lists 'are 'implemented)
         :unbound-variable (list 'unbound (symbol "variable:") fnc)
         :wrong-number-args (list 'Wrong 'number 'of 'args 'given fnc)
         :wrong-number-args-oper (list (symbol (str fnc ":")) 'Wrong 'number 'of 'args 'given)
         :wrong-number-args-prim-proc (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") (symbol (str fnc '>)))
         :wrong-type-apply (list 'Wrong 'type 'to 'apply fnc)
         ())))
  ([cod fnc nom-arg]
    (cons (symbol ";ERROR:") (cons (symbol (str fnc ":"))
    			(case cod
     			 :bad-body (list 'bad 'body nom-arg)
     			 :bad-else-clause (list 'bad 'ELSE 'clause nom-arg)
      			:bad-or-missing (list 'bad 'or 'missing 'clauses nom-arg)
     			 :bad-params (list 'Parameters 'are 'implemented 'only 'as 'lists nom-arg)
      			:bad-variable (list 'bad 'variable nom-arg)
     			 :missing-or-extra (list 'missing 'or 'extra 'expression nom-arg)
     			 :wrong-type-arg (list 'Wrong 'type 'in 'arg nom-arg)
     			 :wrong-type-arg1 (list 'Wrong 'type 'in 'arg1 nom-arg)
     			 :wrong-type-arg2 (list 'Wrong 'type 'in 'arg2 nom-arg)
         ())))))


; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE SCHEME (ADEMAS DE COMPLETAR `EVALUAR` Y `APLICAR-FUNCION-PRIMITIVA`):






;FUNCION AUXILIAR DE leer-entrada
(defn leer-entrada-recursivo [entradaAnterior iteracion]
(let [newLine (read-line)]
(cond 

(and (= 0 (verificar-parentesis (str entradaAnterior newLine))) (> iteracion 0)) (str entradaAnterior " " newLine)
(and (= 0 (verificar-parentesis (str entradaAnterior newLine))) (= iteracion 0)) (str entradaAnterior newLine)
(and (= 1 (verificar-parentesis (str entradaAnterior newLine))) (> iteracion 0)) (leer-entrada-recursivo (str entradaAnterior " " newLine) (+ 1 iteracion))
(and (= 1 (verificar-parentesis (str entradaAnterior newLine))) (= iteracion 0)) (leer-entrada-recursivo (str entradaAnterior newLine) (+ 1 iteracion))
(> 0 (verificar-parentesis (str entradaAnterior newLine))) (symbol ";ERROR:")
)
)
)


;  "Lee una cadena desde la terminal/consola. Si los parentesis no estan correctamente balanceados 
  ;al presionar Enter/Intro,
  ; se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, 
   ;se la devuelve completa."

  ; "Lee una cadena desde la terminal/consola. Si contiene parentesis de menos al presionar Enter/Intro, 
  ; se considera que la cadena ingresada es una subcadena y el ingreso continua. De lo contrario, 
   ;se la devuelve completa (si corresponde, advirtiendo previamente que hay parentesis de mas)."
(defn leer-entrada []

(leer-entrada-recursivo "" 0)

)



; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0

;                     Funciones auxiliares para verificar-parentesis

;A partir de un texto, me devuelve cada caracter como elemento de una seq, a excepcion de los espacios

; FUNCION AUXILIAR DE verificar-parentesis
(defn obtainRawSeq [text]
(re-seq #"." (clojure.string/replace text #" " ""))
)

; FUNCION AUXILIAR DE verificar-parentesis
(defn balanceado? [lista, contador,i, n]
  (cond
  (= contador -1) (- 0 1)

  (and (= n i) (= contador 0) ) (+ 0 0)

  (and (= n i) (> contador 0) ) (+ 0 1)
  
  (= "(" (nth lista i)) (balanceado? lista (+ contador 1) (+ i 1) n)

  (= ")" (nth lista i)) (balanceado? lista (- contador 1) (+ i 1) n)
  :else (balanceado? lista contador (+ i 1) n))
  )


;; ()()(), devuelve 0, esta bien
;;Devuelve 0 si estan bien balanceados, 1 si faltan parentesis, negativo si estan desbalanceados
;"Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."

(defn verificar-parentesis [texto]
  (balanceado? (obtainRawSeq texto) 0 0 (count (obtainRawSeq texto)))
)

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
;;FAUX
; Clase: "Los errores son listas que tienen en la primera posicion el simbolo de error"

; FUNCION AUXILIAR 
(defn in? 
  "true si la lista contiene el elemento"
  [coll elm]  
  (some #(= elm %) coll))



;Devuelve -1 si no encuentra la clave en el ambiente, sino devuelve la clave

; FUNCION AUXILIAR DE ACTUALIZAR-AMB
(defn buscarUtil [clave, ambiente]
(cond
  (nil? (in? (take-nth 2 ambiente) clave)) -1
  :else   (.indexOf (take-nth 2 ambiente) clave)
)
)



(defn buscarInsensitive [clave, ambiente, i, n]
;(spy "b insensitive con clave" clave)
;(spy "i es " i)
;(spy "n es" n)
(cond
  (= n i) -1

  (igual? (nth ambiente i) clave) i

  :else (buscarInsensitive clave ambiente (+ i 1) n)
)

)




(defn actualizar-amb [ambiente, clave, valor]
(cond
  (and (list? valor) (= (nth valor 0) (symbol ";ERROR:"))) ambiente
  (= -1 (buscarInsensitive clave ambiente 0 (count ambiente))) (concat ambiente (list clave valor))
  (< -1 (buscarInsensitive clave ambiente 0 (count ambiente))) (apply list(assoc (into [] ambiente) (+ 1 (buscarInsensitive clave ambiente 0 (count ambiente))) valor))
  :else ambiente
)
)

; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)


(defn buscar [clave, ambiente]
(cond
  ;(nil? (in? (take-nth 2 ambiente) clave)) (generar-mensaje-error :unbound-variable clave)
  ;:else   (nth (take-nth 2 (rest ambiente))(.indexOf (take-nth 2 ambiente) clave))

  (= -1(buscarInsensitive clave (take-nth 2 ambiente) 0 (count (take-nth 2 ambiente)))) 
  (generar-mensaje-error :unbound-variable clave)

  :else (nth (take-nth 2 (rest ambiente))
  (buscarInsensitive clave (take-nth 2 ambiente) 0 (count (take-nth 2 ambiente)))
)
))



;"Devuelve true o false, segun sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."
(defn error?  [entrada]
  ;(spy "ES UN ERROR ESTA ENTRADA?" entrada)
  (cond
  (= false (list? entrada)) false
  ;(= false (coll? entrada)) false
  (= '() entrada) false
  (empty? entrada) false
  (= (nth entrada 0) (symbol ";WARNING:")) true
  (= (nth entrada 0) (symbol ";ERROR:")) true
  :else false
  ) 
)

; user=> (proteger-bool-en-str "(or #F #f #t #T)")
; "(or %F %f %t %T)"
; user=> (proteger-bool-en-str "(and (or #F #f #t #T) #T)")
; "(and (or %F %f %t %T) %T)"
; user=> (proteger-bool-en-str "")
; ""
;"Cambia, en una cadena, #t por %t y #f por %f (y sus respectivas versiones en mayusculas), para poder aplicarle read-string."
(defn proteger-bool-en-str [entrada]
  (clojure.string/replace entrada #"#F|#T|#f|#t" {"#F" "%F" "#T" "%T" "#f" "%f" "#t" "%t"})  
)

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
;"Cambia, en un codigo leido con read-string, %t por #t y %f por #f (y sus respectivas versiones en mayusculas)."

;Le paso la cadena protegida, leida como read string. Lo primero que hago es convertir esa entrada a string,
;reemplazando todos los %F,%T,... por :%F,:%T, despues hago el read string. Teniendo los :%F,:%T,...
;puedo usar el prewalk-replace para reemplazar con los simbolos correspondientes.

(defn restaurar-bool [entrada]
(clojure.walk/prewalk-replace {:%F (symbol "#F") :%f (symbol "#f") :%T (symbol "#T") :%t (symbol "#t")}
(read-string (clojure.string/replace entrada #"%F|%T|%f|%t" 
{"%F" ":%F" "%T" ":%T" "%f" ":%f" "%t" ":%t"})))  
)

; user=> (igual? 'if 'IF)
; true
; user=> (igual? 'if 'if)
; true
; user=> (igual? 'IF 'IF)
; true
; user=> (igual? 'IF "IF")
; false
; user=> (igual? 6 "6")
; false
; "Verifica la igualdad entre dos elementos al estilo de Scheme (case-insensitive)"


(defn igual? [atomo1, atomo2]
(spy "VERIFICO SI SON IGUALES:" atomo1)
(spy "VERIFICO SI SON IGUALES:" atomo2)
(spy "devuelvo"(cond

  (and (nil? atomo1) (nil? atomo2)) true
  (nil? atomo1) false
  (nil? atomo2) false

  (and (= (symbol "#f") atomo1) (= (symbol "#F") atomo2)) true

  (and (= (symbol "#F") atomo1) (= (symbol "#f") atomo2)) true

  (and (= (symbol "#t") atomo1) (= (symbol "#T") atomo2)) true

  (and (= (symbol "#t") atomo1) (= (symbol "#T") atomo2)) true

  (and (symbol? atomo1) (symbol? atomo2) (= atomo1 atomo2)) true ;(spy "Los simbolos son igaules"true)

  (or (= (symbol "#f") atomo1) (= (symbol "#f") atomo2)) false


  (and (string? atomo1) (symbol? atomo2)) false
  (and (symbol? atomo1) (string? atomo2)) false
  (and (string? atomo1) (string? atomo2)) (= atomo1 atomo2)
  (and (number? atomo1) (string? atomo2)) false
  (and (string? atomo1) (number? atomo2)) false
  
  ;(and (symbol? atomo1) (symbol? atomo2) (= false (= atomo1 atomo2))) false
  (and (number? atomo1) (number? atomo2) (= atomo1 atomo2)) true
  (and (number? atomo1) (number? atomo2) (= false (= atomo1 atomo2))) false
  :else (let [converted  (re-seq #"\w+" (clojure.string/upper-case atomo1) )
  converted2 (re-seq #"\w+" (clojure.string/upper-case atomo2))] 
  (cond
    (and (= converted converted2)
    (= false (nil? converted)) (= false (nil? converted2))) true
    :else false
  
  )
         
  
  )
))

)

;(defn igual-recursivo? [elemento1, elemento2, n, i]
;(cond
 ; (= n i) (true)
  ;(= false (igu? (nth elemento1 i) (nth elemento2 i))) (false) 
  ;(and (string? (nth elemento1 i)) (symbol? (nth elemento2 i))) (false)
  ;(and (symbol? (nth elemento1 i)) (string? (nth elemento2 i))) (false)
  ;(and (string? (nth elemento1 i)) (string? (nth elemento2 i))) (= (nth elemento1 i) (nth elemento2 i))
  
  ;(and (and (list? elemento1) (list? elemento2)) (= (count elemento1) (count elemento2))) 
  ;(igual-recursivo? elemento1 elemento2 (count elemento1) 0)

  ;:else (igual-recursivo? elemento1 elemento2 n (+ i 1))
;)
;)

;(defn igual? [elemento1, elemento2]
;(cond
 ; (and (and (list? elemento1) (list? elemento2)) (= (count elemento1) (count elemento2))) 
 ; (igual-recursivo? elemento1 elemento2 (count elemento1) 0)
  
  ;:else (igu? elemento1 elemento2)
;)
;)

; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
;"Devuelve el resultado de fusionar listas."
(defn fnc-append [entrada]
( cond
   (some false? (map list? entrada)) (generar-mensaje-error :wrong-type-arg 'append (nth entrada (.indexOf (map list? entrada) false)))
  :else (apply concat entrada)
)
  
)


;Convierto la lista a string, la paso a mayusculas y la termino convirtiendo en secuencia para poder
;applicarle apply

(defn compare-2-lists [list1, list2, i, n]
(spy "ENTRO A COMPARE-2-LISTS CON " list1)
(spy "ENTRO A COMPARE-2-LISTS CON " list2)
(spy "i es " i)
(spy "n es " n)
(cond

  (= false(= (spy "cuenta de 1 es"(count list1)) (spy "cuienta de 2 es"(count list2)))) (symbol "#f")
  (= i n) (symbol "#t")

  :else
  (cond
      (igual? (nth list1 i) (nth list2 i)) (compare-2-lists list1 list2 (+ i 1) n)
      :else (symbol "#f")

  )


)
)

(defn eq-recursive [entrada, i, n]
  (spy "ENTRO A EQ RECURSIVE CON" entrada)
  ;(cond
   ; (= (+ i 1) n) (symbol "#t")
    ;:else
    ;(cond
     ; (compare-2-lists (nth entrada i) (nth entrada (+ i 1)) 0 (count (nth entrada i)))
      ;(eq-recursive entrada (+ i 1) n)

      ;:else (symbol "#f")
    
    ;)

  ;)
  (compare-2-lists (nth entrada 0) (nth entrada 1) 0 (count (nth entrada 0)))

)

(defn fnc-equal? [entrada]
(spy "ENTRO A EQUAL CON" entrada)
(cond
  (empty? entrada) (symbol "#t")
  (list? (nth entrada 0)) (eq-recursive entrada 0  (count entrada))
  :else   ( let [converted (re-seq #"\w+" (clojure.string/upper-case entrada) )]
  ;(spy "Converted es" converted)
  (cond
    
    (= 1 (count entrada)) (symbol "#t")
    (apply = converted) (symbol "#t")
    :else (symbol "#f")
  ))
)

)



; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
;"Devuelve la lectura de un elemento de Scheme desde la terminal/consola."
(defn fnc-read []
  (read-string (leer-entrada))
)



(defn fnc-sumar [entrada]
(cond
  (empty? entrada) 0
  (= false(nth (map number? entrada) 0 )) (generar-mensaje-error :wrong-type-arg1 '+ (nth entrada 0))
  (some false? (map number? entrada)) (generar-mensaje-error :wrong-type-arg2 '+ (nth entrada 0))
  :else (reduce + entrada)
)
)



(defn fnc-restar [entrada]
(cond
  (empty? entrada) "(;ERROR: -: Wrong number of args given)"
  (= false(nth (map number? entrada) 0 )) (generar-mensaje-error :wrong-type-arg1 '- (nth entrada 0))
  (some false? (map number? entrada)) (generar-mensaje-error :wrong-type-arg2 '- (nth entrada 0))
  (= 1 (count entrada)) (nth (map - entrada) 0)
  :else (reduce - entrada)
)
)




(defn fnc-menor [entrada]
(cond
  (empty? entrada) (symbol "#t")
  (= 1 (count entrada)) (symbol "#t")
    (= false(nth (map number? entrada) 0 )) (generar-mensaje-error :wrong-type-arg1 '< (nth entrada 0))
  (some false? (map number? entrada)) (generar-mensaje-error :wrong-type-arg2 '< (nth entrada 0))
  (apply < entrada) (symbol "#t")
  :else (symbol "#f")
)
)


(defn fnc-mayor [entrada]
  (cond
  (empty? entrada) (symbol "#t")
  (= 1 (count entrada)) (symbol "#t")
  (= false(nth (map number? entrada) 0 )) (generar-mensaje-error :wrong-type-arg1 '> (nth entrada 0))
  (some false? (map number? entrada)) (generar-mensaje-error :wrong-type-arg2 '> (nth entrada 0))
  (apply > entrada) (symbol "#t")
  :else (symbol "#f")
)
)


(defn fnc-mayor-o-igual [entrada]
 (cond
  (empty? entrada) (symbol "#t")
  (= 1 (count entrada)) (symbol "#t")
    (= false(nth (map number? entrada) 0 )) (generar-mensaje-error :wrong-type-arg1 '>= (nth entrada 0))
  (some false? (map number? entrada)) (generar-mensaje-error :wrong-type-arg2 '>= (nth entrada 0))
  (apply >= entrada) (symbol "#t")
  :else (symbol "#f")
)
  
)


;BUSCAr:
;; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)
  ;"Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   ;y devuelve el valor asociado. Devuelve un error :unbound-variable si no la encuentra."


; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "chau" '(x 6 y 11 z "hola"))
; ("chau" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))


  ;"Evalua una expresion escalar. Devuelve una lista con el resultado y un ambiente."
(defn evaluar-escalar [escalar, ambiente]
;(spy "ENTRO A EVALUAR ESCALAR con el simbolo:" escalar)
;(spy "AMBIENTE E")
(cond
;(= -1 (buscarInsensitive escalar (take-nth 2 ambiente) 0 (count (take-nth 2 ambiente))))
(and (symbol? escalar) (= -1 (buscarInsensitive escalar (take-nth 2 ambiente) 0 (count (take-nth 2 ambiente))))) 
(list (generar-mensaje-error :unbound-variable escalar) ambiente)
(symbol? escalar) (list (buscar escalar ambiente) ambiente)

:else (list escalar ambiente)

)

)

;Ejemplo consultas:
;(define (f x) (+ x 1))

;Se puede ver como:(lambda (x) (+ x 1))

;Evaluandola con 6: (f 6)
;( (lambda (x) (+ x 1)) 6)
;7


; user=> (evaluar-define '(define x 2) '(x 1))
; (#<unspecified> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (+ x 1))))

; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
; Otro test que se comento en las consultas:
; user=>  (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (display x) (newline) (+ x 1))))
;"Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."



; FUNCION AUXILIAR DE EVALUAR-DEFINE
(defn concatenar [ambiente, clave, valor]
  (concat ambiente (list clave valor))
)

; FUNCION AUXILIAR DE EVALUAR-DEFINE
(defn parseo-lambda [expresion, ambiente]

  ;(spy "Expresion es" expresion)

  (concatenar ambiente (nth (nth expresion 1) 0) 
  (concat(list 'lambda  (for [j (range 1 (count (nth expresion 1)))] (nth (nth expresion 1) j))) 
  (for [i (range 2 (count expresion))] (nth expresion i))))
)

; Otro test que se comento en las consultas:
; user=>  (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (display x) (newline) (+ x 1))))

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)

(defn evaluar-define [expresion, ambiente]
;(spy "EXPRESION A EVALUAR-DEFINE ES" expresion)
  (cond

    (< (count expresion) 3)
    (list (generar-mensaje-error :missing-or-extra 'define expresion) ambiente)

    (and (> (count expresion) 3) (= false (list? (nth expresion 1)))) 
    (list (generar-mensaje-error :missing-or-extra 'define expresion) ambiente)

    ;(= (nth expresion 1) (list 'f 'x)) 
    (list? (nth expresion 1))
    (list (symbol "#<unspecified>") (parseo-lambda expresion ambiente))

    (= false (symbol? (nth expresion 1))) 
    (list (generar-mensaje-error :bad-variable 'define (nth expresion 1)) ambiente)

    :else (list (symbol "#<unspecified>") (actualizar-amb ambiente (nth expresion 1) (nth (evaluar (nth expresion 2) ambiente) 0)))
  )
  
)

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
;(Se le pasaron 0 argumentos, error)
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
;(Se le paso solo 1 elemento)

;"Evalua una expresion `if`. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
(defn evaluar-if [expresion, ambiente]

  (cond
  (or (< (count expresion) 3) (> (count expresion) 4))
  (list (generar-mensaje-error :missing-or-extra 'if expresion) ambiente)

  :else (cond
    (= (count expresion) 3) 
      (cond
        (= (symbol "#f") (nth expresion 1)) (list (symbol "#<unspecified>") ambiente) 
        :else  (evaluar (nth expresion 2) ambiente)
      )

    :else 
      (cond
        (= (symbol "#f") (nth expresion 1)) (evaluar (nth expresion 3) ambiente)
        :else (evaluar (nth expresion 2) ambiente)

      )
  )

)
)

; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; "Evalua una expresion `or`.  Devuelve una lista con el resultado y un ambiente."


; FUNCION AUXILIAR DE EVALUAR-OR
(defn or-recursivo [expresionOr, iteracion, n]
(cond
  (= iteracion n) (symbol "#f")
  :else (cond
    (= (symbol "#f") (nth expresionOr iteracion) ) (or-recursivo expresionOr (+ iteracion 1) n)
    :else (nth expresionOr iteracion)
  )
)
)

;Evaluo hasta que aparezca uno distinto de false
(defn evaluar-or [expresionOr, ambiente]

(cond
  (= (count expresionOr) 1) (list (symbol "#f") ambiente) 
  (= (count expresionOr) 2) (list (nth expresionOr 1) ambiente)
  :else (list (or-recursivo expresionOr 1 (count expresionOr)) ambiente) 
)
  
)

; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<unspecified> (x 1))
; user=> (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())
; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))
; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))
; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))
;"Evalua una expresion `set!`. Devuelve una lista con el resultado y un ambiente actualizado con la redefinicion."



; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
;;FAUX
(defn evaluar-set! [expresion, ambiente]
;(spy "Entro a evaluar set con expresion" expresion)
;(spy "Entro a evaluar set con ambiente" ambiente)
(cond

(or (< (count expresion) 3) (> (count expresion) 3))
(list (generar-mensaje-error :missing-or-extra 'set! expresion) ambiente)

(= false (symbol? (nth expresion 1))) 
(list (generar-mensaje-error :bad-variable 'set! (nth expresion 1)) ambiente)

(= -1(buscarInsensitive (nth expresion 1) ambiente 0 (count ambiente))) 
(list (generar-mensaje-error :unbound-variable (nth expresion 1)) ambiente)


:else (list (symbol "#<unspecified>") 
(actualizar-amb ambiente (nth expresion 1) (nth (evaluar (nth expresion 2) ambiente) 0 )  ))

)
)
;(nth (evaluar (nth expresion 2) ambiente) 0)


;;Opcional:
; Al terminar de cargar el archivo en el REPL de Clojure, se debe devolver true.





