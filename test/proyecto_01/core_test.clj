(ns proyecto-01.core-test   
(:require [clojure.test :refer :all]             
[proyecto-01.core :refer :all]))  






(deftest es-el-doble?-test   
(testing "Prueba de la funcion: es-el-doble?"     
(is (= true (es-el-doble? 4 8)))     
(is (= false (es-el-doble? 4 7)))   ) ) 


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

