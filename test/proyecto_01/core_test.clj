(ns proyecto-01.core-test   
(:require [clojure.test :refer :all]             
[proyecto-01.core :refer :all]))  

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

(deftest fnc-menor-test

(testing "Prueba de la funcion: fnc-menor-test"
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
(is (= (symbol "#f") (fnc-equal? '('list? 'list))))
(is (= (symbol "#t") (fnc-equal? '('list 'LIST))))
(is (= (symbol "#f") (fnc-equal? '('LIST? 'LIST))))
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
(is (= (symbol "#f") (fnc-equal? '((1) 2))))
)
)
(deftest error?-test 
  (testing "Prueba de la funcion: fnc-error"
(is (= false (error? '+)))
(is (= false (error? '())))
(is (= true (error? (list (symbol ";ERROR:") 'mal 'hecho))))
(is (= false (error? (list 'mal 'hecho))))
(is (= true (error? (list (symbol ";WARNING:") 'mal 'hecho))))
  )
)

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

;"Devuelve el resultado de fusionar listas."
(deftest fnc-append-test 
  (testing "Prueba de la funcion: append")
  (is (= '(1 2 3 4 5 6 7) (fnc-append '( (1 2) (3) (4 5) (6 7)))))

  (is (= (generar-mensaje-error :wrong-type-arg 'append '3) (fnc-append '( (1 2) 3 (4 5) (6 7)))))
  (is (= (generar-mensaje-error :wrong-type-arg 'append 'A) (fnc-append '( (1 2) A (4 5) (6 7)))))
)


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
(is (= false (igual? '6 '(6))))
(is (= false (igual? '(6) '6)))
(is (= false (igual? '(symbol "#f") (symbol "#f"))))
(is (= false (igual? '(quote +) 'quote)))
(is (= false (igual?  'quote '(quote +))))
(is (= false (igual?  'igual? 'igual)))
(is (= false (igual?  'igual 'igual?)))
(is (= false (igual?  '?igual 'igual)))
(is (= false (igual?  'igual '?igual)))
(is (= false (igual?  'igual '!!igual)))
(is (= false (igual?  'igual 'igual-)))
(is (= false (igual?  'LIST? 'LIST)))
(is (= false (igual?  (list 'quote) '(quote +))))
(is (= false (igual?  'quote (list 'quote '()))))
(is (= false (igual?  (list 'quote) 'quote)))
(is (= false (igual?  'quote (list 'quote) )))
(is (= true (igual?  (list 'quote) (list 'quote) )))
(is (= true (igual?  (list 'quote '+) (list 'quote '+) )))
(is (= true (igual? '(a (b) c) '(A (B) C) )))
(is (= false (igual? "asd" "ASD")))
(is (= false (igual? (list "asd" 5) (list "ASD" 5))))
(is (= true (igual? (list "asd" 5) (list "asd" 5) )))
(is (= true (igual? '(5 0) '(5 0))))
(is (= false (igual? '(5 1) '(5 0))))
(is (= false (igual? '() '(2))))
(is (= false (igual? '(4) '(2))))
(is (= false (igual? '(4 2) '(2))))
(is (= true (igual? '(() ()) '(() ()))))
(is (= true (igual? '() '())))
(is (= false (igual? '((2) ()) '(() ()))))
(is (= false (igual? '() '4)))
(is (= true (igual? '(5 0 2 -1 4 6 0 8 ()) '(5 0 2 -1 4 6 0 8 ()))))
(is (= false (igual? '(5 0 2 -1 4 6 0 8) '(5 0 2 -1 4 6 0 8 ()))))
(is (= false (igual?  '4 '())))
(is (= true (igual?  '((()) (())) '((()) (())))))
(is (= true (igual? '((1 3 5) (1 3 5)) '((1 3 5) (1 3 5)))))
(is (= false (igual? '((1 3 5) (1 3 5)) '((1 3 2) (1 3 5)))))

)

(deftest buscar-test 
(testing "Prueba de la funcion: buscar")
(is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
(is (= (list (symbol ";ERROR:") 'unbound (symbol "variable:") 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
)



;  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
;  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza la nueva informacion."

(deftest actualizar-amb-test
(testing "Prueba de la funcion: actualizar-amb")
(is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
(is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
(is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
(is (= '(b 7) (actualizar-amb () 'b 7)))
(is (= '(b 7 inicial ()) (actualizar-amb '(b 7) 'inicial '())))
)


;"Cambia, en una cadena, #t por %t y #f por %f (y sus respectivas versiones en mayusculas), para poder aplicarle read-string."
(deftest proteger-bool-en-str-test
(testing "Prueba de la funcion: proteger-bool-en-str")
(is (= "(or %F %f %t %T)" (proteger-bool-en-str "(or #F #f #t #T)")))
(is (= "(and (or %F %f %t %T) %T)" (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
(is (= "" (proteger-bool-en-str "")))
)


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
)


(deftest fnc-read-test
(testing "Prueba de la funcion: fnc-read")
)


(deftest evaluar-escalar-test
(testing "Prueba de la forma especial evaluar escalar")
(is (= (list '32 (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 32 '(x 6 y 11 z "hola"))))
(is (= (list "chau" (list 'x '6 'y '11 'z "hola")) (evaluar-escalar "chau" '(x 6 y 11 z "hola"))))
(is (= (list '11 (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 'y '(x 6 y 11 z "hola"))))
(is (= (list "hola" (list 'x '6 'y '11 'z "hola")) (evaluar-escalar 'z '(x 6 y 11 z "hola"))))
(is (= (list (generar-mensaje-error :unbound-variable 'n) '(x 6 y 11 z "hola")) (evaluar-escalar 'n '(x 6 y 11 z "hola"))))
)


(deftest evaluar-or-test
(testing "Prueba de la forma especial evaluar or")

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

(is (= (list (generar-mensaje-error :missing-or-extra 'if (list 'if)) (list 'n '7)) 
(evaluar-if '(if) '(n 7))))

(is (= (list (generar-mensaje-error :missing-or-extra 'if (list 'if '1)) (list 'n '7)) 
(evaluar-if '(if 1) '(n 7))))


(is (= (list (symbol "#<unspecified>") (list 'n '9 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))))

(is (= (list (symbol "#<unspecified>") (list 'n '9 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#t") '(set! n 9) 'n ) (list 'n 7 (symbol "#f") (symbol "#f")))))

(is (= (list (symbol "#<unspecified>") (list 'n '9 (symbol "#f") (symbol "#f"))) 
(evaluar-if (list 'if (symbol "#t") '(set! n 9) 'n ) (list 'n 7 (symbol "#f") (symbol "#f")))))
)


;"Evalua una expresion `define`. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
(deftest evaluar-define-test
  (testing "Prueba de la forma especial evaluar define")


  (is (= (list (symbol "#<unspecified>") (list 'x '2))
  (evaluar-define '(define x 2) '(x 1))))
  (is (= (list (symbol "#<unspecified>") (list 'x '1 'f (list 'lambda '(x) '(+ x 1))))
  (evaluar-define '(define (f x) (+ x 1)) '(x 1))))

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

)

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



