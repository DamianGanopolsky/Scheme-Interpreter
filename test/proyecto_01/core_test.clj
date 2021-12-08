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

(testing "Prueba de la funcion: fnc-restar-test"
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




;;Hacer deftest nombreDelaFuncion-test
;;Adentro de cada deftest de la func, poner los distintos testings, cada testing es una situacion distinta

