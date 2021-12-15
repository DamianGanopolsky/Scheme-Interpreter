(ns proyecto-01.core-test   
(:require [clojure.test :refer :all]             
[proyecto-01.core :refer :all]))  

; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)
(deftest fnc-sumar-test

(testing "Prueba de la funcion: fnc-sumar-test"
(is (= 3 (fnc-sumar '(3))))
(is (= 7 (fnc-sumar '(3 4))))
(is (= 12 (fnc-sumar '(3 4 5))))
(is (= 18 (fnc-sumar '(3 4 5 6))))
)
(testing "Caso lista vacia")
(is (= 0 (fnc-sumar ())))

(testing "Caso tipo incorrecto")
(is (= (list (symbol ";ERROR: +:") 'Wrong 'type 'in 'arg1 'A)) (fnc-sumar '(A 4 5 6)))
(is (= (list (symbol ";ERROR: +:") 'Wrong 'type 'in 'arg2 'A)) (fnc-sumar '(3 A 5 6)))
(is (= (list (symbol ";ERROR: +:") 'Wrong 'type 'in 'arg2 'A)) (fnc-sumar '(3 4 A 6)))
)


; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)


(deftest fnc-restar-test

(testing "Prueba de la funcion: fnc-restar-test"
(is (= -3 (fnc-restar '(3))))
(is (= -1 (fnc-restar '(3 4))))
(is (= -6 (fnc-restar '(3 4 5))))
(is (= -12 (fnc-restar '(3 4 5 6))))
)
(testing "Caso lista vacia")
(is (= "(;ERROR: -: Wrong number of args given)" (fnc-restar ())))

(testing "Caso tipo incorrecto")
(is (= (list (symbol ";ERROR: -:") 'Wrong 'type 'in 'arg1 'A)) (fnc-restar '(A 4 5 6)))
(is (= (list (symbol ";ERROR: -:") 'Wrong 'type 'in 'arg2 'A)) (fnc-restar '(3 A 5 6)))
(is (= (list (symbol ";ERROR: -:") 'Wrong 'type 'in 'arg2 'A)) (fnc-restar '(3 4 A 6)))
)



; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)

(deftest fnc-menor-test

(testing "Prueba de la funcion: fnc-menor-test"
;;((is  (= (list(symbol "#t")) (fnc-menor ())))
(is (= (symbol "#t") (fnc-menor ())))
(is (= (symbol "#t") (fnc-menor '(1))))
(is (= (symbol "#t") (fnc-menor '(1 2))))
(is (= (symbol "#t") (fnc-menor '(1 2 3))))
(is (= (symbol "#t") (fnc-menor '(1 2 3 4))))
(is (= (symbol "#f") (fnc-menor '(1 2 2 4))))
(is (= (symbol "#f") (fnc-menor '(1 2 1 4))))
(is (= (list (symbol ";ERROR: <:") 'Wrong 'type 'in 'arg1 'A)) (fnc-menor '(A 1 2 4)))
(is (= (list (symbol ";ERROR: <:") 'Wrong 'type 'in 'arg2 'A)) (fnc-menor '(1 A 1 4)))
(is (= (list (symbol ";ERROR: <:") 'Wrong 'type 'in 'arg2 'A)) (fnc-menor '(1 2 A 4)))
)
)



; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)
(deftest fnc-mayor-test

(testing "Prueba de la funcion: fnc-mayor-test"
(is (= (symbol "#t") (fnc-mayor ())))
(is (= (symbol "#t") (fnc-mayor '(1))))
(is (= (symbol "#t") (fnc-mayor '(2 1))))
(is (= (symbol "#t") (fnc-mayor '(3 2 1))))
(is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
(is (= (symbol "#f") (fnc-mayor '(4 2 2 1))))
(is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
(is (= (list (symbol ";ERROR: >:") 'Wrong 'type 'in 'arg1 'A)) (fnc-mayor '(A 3 2 1)))
(is (= (list (symbol ";ERROR: >:") 'Wrong 'type 'in 'arg2 'A)) (fnc-mayor '(3 A 2 1)))
(is (= (list (symbol ";ERROR: >:") 'Wrong 'type 'in 'arg2 'A)) (fnc-mayor '(3 2 A 1)))
)
)



;;Hacer deftest nombreDelaFuncion-test
;;Adentro de cada deftest de la func, poner los distintos testings, cada testing es una situacion distinta



; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: >=: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: >=: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: >=: Wrong type in arg2 A)


(deftest fnc-mayor-o-igual-test

(testing "Prueba de la funcion: fnc-mayor-o-igual-test"
(is (= (symbol "#t") (fnc-mayor-o-igual ())))
(is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
(is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))
(is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
(is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
(is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 2 1))))
(is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
(is (= (list (symbol ";ERROR: >=:") 'Wrong 'type 'in 'arg1 'A)) (fnc-mayor-o-igual '(A 3 2 1)))
(is (= (list (symbol ";ERROR: >=:") 'Wrong 'type 'in 'arg2 'A)) (fnc-mayor-o-igual '(3 A 2 1)))
(is (= (list (symbol ";ERROR: >=:") 'Wrong 'type 'in 'arg2 'A)) (fnc-mayor-o-igual '(3 2 A 1)))
)
)


; user=> (fnc-equal? ())
; #t
; user=> (fnc-equal? '(A))
; #t
; user=> (fnc-equal? '(A a))
; #t
; user=> (fnc-equal? '(A a A))
; #t
; user=> (fnc-equal? '(A a A a))
; #t
; user=> (fnc-equal? '(A a A B))
; #f
; user=> (fnc-equal? '(1 1 1 1))
; #t
; user=> (fnc-equal? '(1 1 2 1))
; #f

;(fnc-equal? '((5 0) (5 0)))
;#t

;#t (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13))))
;#t (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13))))

(deftest fnc-equal?-test 
(testing "Prueba de la funcion: fnc-equal"
(is (= (symbol "#t") (fnc-equal? ())))
(is (= (symbol "#t") (fnc-equal? '(A))))
(is (= (symbol "#t") (fnc-equal? '(A a))))
(is (= (symbol "#t") (fnc-equal? '(A a A))))
(is (= (symbol "#t") (fnc-equal? '(A a A a))))
(is (= (symbol "#f") (fnc-equal? '(A a A B))))
(is (= (symbol "#t") (fnc-equal? '(1 1 1 1))))
(is (= (symbol "#f") (fnc-equal? '(1 1 2 1))))
(is (= (symbol "#t") (fnc-equal? '((5 0) (5 0)))))
(is (= (symbol "#f") (fnc-equal? '((5 1) (5 0)))))
(is (= (symbol "#t") (fnc-equal? '(() ()))))
(is (= (symbol "#f") (fnc-equal? '(() (2)))))
(is (= (symbol "#f") (fnc-equal? '((1 3 5) (2)))))
(is (= (symbol "#t") (fnc-equal? '((1 3 5) (1 3 5) (1 3 5)))))
(is (= (symbol "#f") (fnc-equal? '((1 3 5) (1 3 5) (1 3 7)))))
(is (= (symbol "#t") (fnc-equal? '((1 3 5) (1 3 5) (1 3 5) (1 3 5)))))
(is (= (symbol "#t") (fnc-equal? '(((1 3 5) (1 3 5)) ((1 3 5) (1 3 5))))))
(is (= (symbol "#f") (fnc-equal? '(((1 3 5) (1 3 5)) ((1 3 2) (1 3 5))))))
(is (= (symbol "#t")) (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13)))) )
(is (= (symbol "#t")  (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13))))  ))
(is (= (symbol "#f") (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '15 '13) (list '12 '13) (list '12 '13))))))
(is (= (symbol "#f") (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '11 '13) (list '12 '13) (list '12 '13))))))
(is (= (symbol "#f")  (fnc-equal? (list (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '11 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13))))))
(is (= (symbol "#f") (fnc-equal? (list (list (list '11 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13)) (list (list '12 '13) (list '12 '13) (list '12 '13))))))
(is (= (symbol "#f") (fnc-equal? '((0 0) (0 0) (0 0) (0 0) (0 0) (0 1) ))))
)
)



; user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true

;"Devuelve true o false, segun sea o no el arg. una lista con `;ERROR:` o `;WARNING:` como primer elemento."

(deftest error?-test 
  (testing "Prueba de la funcion: fnc-error"

(is (= false (error? '+)))
(is (= false (error? '())))
(is (= true (error? (list (symbol ";ERROR:") 'mal 'hecho))))
(is (= false (error? (list 'mal 'hecho))))
(is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho))))
  )
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

;; ()()(), devuelve 0, esta bien
;;Devuelve 0 si estan bien balanceados, 1 si faltan parentesis, negativo si estan desbalanceados
;"Cuenta los parentesis en una cadena, sumando 1 si `(`, restando 1 si `)`. Si el contador se hace negativo, para y retorna -1."
(deftest verificar-parentesis-test 
 (testing "Prueba de la funcion: verificar-parentesis"
(is (= 1 (verificar-parentesis "(hola 'mundo")))
(is (= -1 (verificar-parentesis "(hola '(mundo)))")))
(is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
(is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
(is (= 0 (verificar-parentesis "(hola '(mundo) )")))
)
)




; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
;"Devuelve el resultado de fusionar listas."
(deftest fnc-append-test 
  (testing "Prueba de la funcion: append")
  (is (= '(1 2 3 4 5 6 7) (fnc-append '( (1 2) (3) (4 5) (6 7)))))

  (is (= (generar-mensaje-error :wrong-type-arg 'append '3) (fnc-append '( (1 2) 3 (4 5) (6 7)))))
  (is (= (generar-mensaje-error :wrong-type-arg 'append 'A) (fnc-append '( (1 2) A (4 5) (6 7)))))
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
;> (equal? '(a (b) c) '(A (B) C))
;#t

;> (equal? "asd" "ASD")
;#f

;> (equal? (list "asd" 5) (list "ASD" 5))
;#f

;> (equal? (list "asd" 5) (list "asd" 5))
;#t


;Si es lista con lista, compara miembro a miebro y hace un and.
;Si son 2 simbolos (e.g: 'if o 'foo), compara de modo insensitiva
;si son 2 strings, compara sensitive
(deftest igual?-test
(testing "Prueba de la funcion: igual")

(is (= true (igual? 'if 'IF)))
(is (= true (igual? 'if 'if)))
(is (= true (igual? 'n 'N)))
(is (= true (igual? 'IF 'IF)))
(is (= false (igual? 'IF "IF")))
(is (= false (igual? 6 "6")))
(is (= true (igual? '(a (b) c) '(A (B) C) )))
(is (= false (igual? "asd" "ASD")))
(is (= false (igual? (list "asd" 5) (list "ASD" 5))))
(is (= true (igual? (list "asd" 5) (list "asd" 5) )))
(is (= true (igual? '(5 0) '(5 0))))
(is (= false (igual? '(5 1) '(5 0))))
(is (= true (igual? '((1 3 5) (1 3 5)) '((1 3 5) (1 3 5)))))
(is (= false (igual? '((1 3 5) (1 3 5)) '((1 3 2) (1 3 5)))))


)


; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)
  ;"Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   ;y devuelve el valor asociado. Devuelve un error :unbound-variable si no la encuentra."

(deftest buscar-test 
(testing "Prueba de la funcion: buscar")
(is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
(is (= (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))

)



; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
;  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
;  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva informacion."

(deftest actualizar-amb-test
(testing "Prueba de la funcion: actualizar-amb")
(is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
(is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
(is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
(is (= '(b 7) (actualizar-amb () 'b 7)))
)



; user=> (proteger-bool-en-str "(or #F #f #t #T)")
; "(or %F %f %t %T)"
; user=> (proteger-bool-en-str "(and (or #F #f #t #T) #T)")
; "(and (or %F %f %t %T) %T)"
; user=> (proteger-bool-en-str "")
; ""
;"Cambia, en una cadena, #t por %t y #f por %f (y sus respectivas versiones en mayusculas), para poder aplicarle read-string."
(deftest proteger-bool-en-str-test
(testing "Prueba de la funcion: proteger-bool-en-str")
(is (= "(or %F %f %t %T)" (proteger-bool-en-str "(or #F #f #t #T)")))
(is (= "(and (or %F %f %t %T) %T)" (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
(is (= "" (proteger-bool-en-str "")))
)



; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
;"Cambia, en un codigo leido con read-string, %t por #t y %f por #f (y sus respectivas versiones en mayusculas)."

(deftest restaurar-bool-test
(testing "Prueba de la funcion: restaurar-bool")
(is (= (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T") ) 
(restaurar-bool (read-string "(or %F %f %t %T)"))))

(is (= (list 'and (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))  
(restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))))

(is (= (list 'and (list 'or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))  
(restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
)




(deftest leer-entrada-test
(testing "Prueba de la funcion: leer-entrada")
;(is (= "Hola" (leer-entrada)))
)


(deftest fnc-read-test
(testing "Prueba de la funcion: fnc-read")
;(is (= (list '0 '0) (fnc-read)))
)


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

(deftest evaluar-escalar-test
(testing "Prueba de la forma especial evaluar escalar")
(is (= (list '32 (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))))
(is (= (list "chau" (list 'x '6 'y '11 'z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
(is (= (list '11 (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 'y '(x 6 y 11 z "hola"))))
(is (= (list "hola" (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 'z '(x 6 y 11 z "hola"))))
(is (= (list (generar-mensaje-error :unbound-variable 'n) '(x 6 y 11 z "hola")) (evaluar-escalar 'n '(x 6 y 11 z "hola"))))


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

;> (or (if #f #t #f) 1)
;1
;> (or (if #f #t #f) #f)
;#f
;> (or (if #t #f) 1)
;1
;> (or #t)
;#t
;> (or #f)
;#f
;> (or #f (if #t 1))
;1


(deftest evaluar-or-test
(testing "Prueba de la forma especial evaluar or")

;> (or (if #f #t #f) 1)
;1
;(is (= 1 (evaluar-or (list 'or))))
;(is (= 1 ) (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))
(is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
(evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))

(is (= (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
(evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))

(is (= (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
(evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))


(is (= (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
(evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))


(is (= (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) 
(evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
)


; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))


; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 7 #f #f))

; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))


; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
;(Se le pasaron 0 argumentos, error)
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
;(Se le paso solo 1 elemento)


(deftest evaluar-if-test
(testing "Prueba de la forma especial evaluar if")

(is (= (list '2 (list 'n '7)) 
(evaluar-if '(if 1 2) '(n 7))))

(is (= (list '7 (list 'n '7)) 
(evaluar-if '(if 1 n) '(n 7))))

(is (= (list '7 (list 'n '7)) 
(evaluar-if '(if 1 n 8) '(n 7))))

(is (= (list (symbol "#<unspecified>") (list 'n '7 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))))

(is (= (list '8 (list 'n '7 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))))


; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<unspecified> (n 9 #f #f))
(is (= (list (symbol "#<unspecified>") (list 'n '9 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))))


(is (= (list (generar-mensaje-error :missing-or-extra 'if (list 'if)) (list 'n '7)) 
(evaluar-if '(if) '(n 7))))

(is (= (list (generar-mensaje-error :missing-or-extra 'if (list 'if '1)) (list 'n '7)) 
(evaluar-if '(if 1) '(n 7))))
)




; user=> (evaluar-define '(define x 2) '(x 1))
; (#<unspecified> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (+ x 1))))


; user=> (evaluar-define '(define) '(x 1))
; ((;ERROR: define: missing or extra expression (define)) (x 1))
; user=> (evaluar-define '(define x) '(x 1))
; ((;ERROR: define: missing or extra expression (define x)) (x 1))
; user=> (evaluar-define '(define x 2 3) '(x 1))
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))
; user=> (evaluar-define '(define ()) '(x 1))
; ((;ERROR: define: missing or extra expression (define ())) (x 1))
; user=> (evaluar-define '(define () 2) '(x 1))
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))
; ((;ERROR: define: bad variable (define 2 x)) (x 1))


; Otro test que se comento en las consultas:
; user=>  (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (display x) (newline) (+ x 1))))
;"Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
(deftest evaluar-define-test
  (testing "Prueba de la forma especial evaluar define")


  (is (= (list (symbol "#<unspecified>") (list 'x '2))
  (evaluar-define '(define x 2) '(x 1))))


; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (+ x 1))))







  (is (= (list (symbol "#<unspecified>") (list 'x '1 'f (list 'lambda '(x) '(+ x 1))))
  (evaluar-define '(define (f x) (+ x 1)) '(x 1))))


; Otro test que se comento en las consultas:
; user=>  (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))
; (#<unspecified> (x 1 f (lambda (x) (display x) (newline) (+ x 1))))
  (is (= (list (symbol "#<unspecified>") 
  (list 'x '1 'f (list 'lambda '(x) '(display x) '(newline) '(+ x 1))))
  (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))))

  (is (= (list (generar-mensaje-error :missing-or-extra 'define (list 'define)) '(x 1))
  (evaluar-define '(define) '(x 1))))

  (is (= (list (generar-mensaje-error :missing-or-extra 'define (list 'define 'x)) '(x 1))
  (evaluar-define '(define x) '(x 1))))

  (is (= (list (generar-mensaje-error :missing-or-extra 'define (list 'define 'x '2 '3)) '(x 1))
  (evaluar-define '(define x 2 3) '(x 1))))

  (is (= (list (generar-mensaje-error :missing-or-extra 'define (list 'define '() )) '(x 1))
  (evaluar-define '(define ()) '(x 1))))

    (is (= (list (generar-mensaje-error :bad-variable 'define 2) '(x 1))
  (evaluar-define '(define 2 x) '(x 1))))

  ;(is (= (list (generar-mensaje-error :bad-variable 'define '()) '(x 1))
  ;(evaluar-define '(define () x) '(x 1))))
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
(deftest evaluar-set!-test
(testing "Prueba de la forma especial evaluar set!")

  (is (= (list (symbol "#<unspecified>") (list 'x '1))
  (evaluar-set! '(set! x 1) '(x 0))))

  (is (= (list (generar-mensaje-error :unbound-variable 'x) '())
  (evaluar-set! '(set! x 1) '())))


  (is (= (list (generar-mensaje-error :missing-or-extra 'set! (list 'set! 'x)) '(x 0))
  (evaluar-set! '(set! x) '(x 0))))

  (is (= (list (generar-mensaje-error :missing-or-extra 'set! (list 'set! 'x '1 '2)) '(x 0))
  (evaluar-set! '(set! x 1 2) '(x 0))))

  (is (= (list (generar-mensaje-error :bad-variable 'set! 1) '(x 0))
  (evaluar-set! '(set! 1 2) '(x 0))))

)



