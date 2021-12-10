(ns proyecto-01.core-test   
(:require [clojure.test :refer :all]             
[proyecto-01.core :refer :all]))  




(deftest es-el-doble?-test   
(testing "Prueba de la funcion: es-el-doble?"     
(is (= true (es-el-doble? 4 8)))     
(is (= false (es-el-doble? 4 7)))   ) ) 



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
(is (= "(;ERROR: +: Wrong type in arg1 A)" (fnc-sumar '(A 4 5 6))))
(is (= "(;ERROR: +: Wrong type in arg2 A)" (fnc-sumar '(3 A 5 6))))
(is (= "(;ERROR: +: Wrong type in arg2 A)" (fnc-sumar '(3 4 A 6))))
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
(is (= "(;ERROR: -: Wrong type in arg1 A)" (fnc-restar '(A 4 5 6))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-restar '(3 A 5 6))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-restar '(3 4 A 6))))
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
(is (= "(;ERROR: -: Wrong type in arg1 A)" (fnc-menor '(A 4 5 6))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-menor '(3 A 5 6))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-menor '(3 4 A 6))))
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
(is (= "(;ERROR: -: Wrong type in arg1 A)" (fnc-mayor '(A 3 2 1))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-mayor '(3 A 2 1))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-mayor '(3 2 A 1))))
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
(is (= "(;ERROR: -: Wrong type in arg1 A)" (fnc-mayor-o-igual '(A 3 2 1))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-mayor-o-igual '(3 A 2 1))))
(is (= "(;ERROR: -: Wrong type in arg2 A)" (fnc-mayor-o-igual '(3 2 A 1))))
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
  (is (= "(;ERROR: -: Wrong type in arg 3)" (fnc-append '( (1 2) 3 (4 5) (6 7)))))
  (is (= "(;ERROR: -: Wrong type in arg A)" (fnc-append '( (1 2) A (4 5) (6 7)))))
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
(is (= true (igual? 'IF 'IF)))
(is (= false (igual? 'IF "IF")))
(is (= true (igual? '(a (b) c) '(A (B) C) )))
(is (= false (igual? "asd" "ASD")))
(is (= false (igual? (list "asd" 5) (list "ASD" 5))))
(is (= true (igual? (list "asd" 5) (list "asd" 5) )))


)

