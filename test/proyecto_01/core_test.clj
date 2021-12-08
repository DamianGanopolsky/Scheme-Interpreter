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




;;(deftest saludar-test

    ;;(testing "Test del valor de retorno"
    ;;(is (= nil (saludar "Diego"))
    ;;(is )))


  ;;  (testing "Test del efecto colateral"
    ;;    (let [res (with-out-str (saludar "Diego"))]
      ;;  (is (= res "Hola Diego\r\n"))
       ;; )
    ;;)
;;)
;; \r\n hay que poner en windows en vez de \n

;;Es una funcion pura, no muestr apor la consola nada
;;(deftest calcular-test

  ;;  (testing "Varias aserciones"
    
    ;;    (is (= 4 (calcular 3)))
      ;;  (is (= 5 (calcular 4)))
        ;;(is (= 6 (calcular 5))
    
   ;; )

;;)

;;Hacer deftest nombreDelaFuncion-test
;;Adentro de cada deftest de la func, poner los distintos testings, cada testing es una situacion distinta

