(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))


(deftest palabra-reservada?-test (testing "Prueba de la funcion: palabra-reservada?"
                                   (is (= true (palabra-reservada? 'AND))) ; una palabra reservada
                                   (is (= false (palabra-reservada? 'PRNT))) ; palabras incolmpletas 
                                   (is (= true (palabra-reservada? 'CHR$))) ; palabra con simbolo $
                                   (is (= false (palabra-reservada? 'NDA))) ; desordenado
                                   (is (= false (palabra-reservada? 'ANDASC)))))
(deftest operador?-test (testing "Prueba de la funcion: operador?"
                          (is (= true (operador? '/))) ; operador
                          (is (= false (operador? '-+))))) ; concatenados

(deftest variable-float?-test (testing "Prueba de la funcion: variable-float?"
                                (is (= true (variable-float? 'X))) ; valido terminado en letra
                                (is (= true (variable-float? 'X3))) ; valido terminado en nro
                                (is (= false (variable-float? 'X!)))
                                (is (= false (variable-float? 5))) ; no valido 
)) ; no valido

(deftest variable-integer?-test (testing "Prueba de la funcion: variable-integer?"
                                  (is (= true (variable-integer? 'X%))) ; valido 
                                  (is (= false (variable-integer? 'X))) ; no valido
                                  (is (= false (variable-integer? 5))) ; no valido 
                                  ))

(deftest variable-string?-test (testing "Prueba de la funcion: variable-string?"
                                 (is (= true (variable-string? 'X$))) ; valido 
                                 (is (= false (variable-string? 'X%))) ; no valido
                                  (is (= false (variable-string? 'X))) ; no valido
                                 (is (= false (variable-string? 5))) ; no valido
                                 ))

(deftest anular-invalidos-test (testing "Prueba de la funcion: anular-invalidos"
                                 (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
                                 (is (= '(IF X nil * Y < 12 THEN LET nil X = 0 nil) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0 nil))))
                                 (is (= (list 'PRINT (symbol "(") 2 '+ 2 (symbol ")")) (anular-invalidos (list 'PRINT (symbol "(") 2 '+ 2 (symbol ")")))))))

(deftest eliminar-cero-decimal-test (testing "Prueba de la funcion: eliminar-cero-decimal"
                                      (is (= 1.5 (eliminar-cero-decimal 1.5)))
                                      (is (= 1.5 (eliminar-cero-decimal 1.50)))
                                      (is (= 1 (eliminar-cero-decimal 1.0)))
                                      (is (= -1 (eliminar-cero-decimal -1.0)))
                                      (is (= -1.5 (eliminar-cero-decimal -1.50)))
                                      (is (= 'A (eliminar-cero-decimal 'A)))))

(deftest eliminar-cero-entero-test (testing "Prueba de la funcion: eliminar-cero-entero"
                                     (is (= nil (eliminar-cero-entero nil)))
                                     (is (= "A" (eliminar-cero-entero 'A)))
                                     (is (= " 0" (eliminar-cero-entero 0)))
                                     (is (= " 1.5" (eliminar-cero-entero 1.5)))
                                     (is (= "-1.5" (eliminar-cero-entero -1.5)))
                                     (is (= " .5" (eliminar-cero-entero 0.5)))
                                     (is (= "-.5" (eliminar-cero-entero -0.5)))))

(deftest desambiguar-test (testing "Prueba de la funcion: desambiguar"
                            (is (= (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))))
                            (is (= (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
                            (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
                            (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))))

(deftest precedencia-test (testing "Prueba de la funcion: precedencia"
                            (is (< (precedencia (symbol ",")) (precedencia '-u))) ; comentar esta linea en caso de ser necesario
                            (is (> (precedencia '-u) (precedencia '-)))
                            (is (> (precedencia '+) (precedencia 'OR)))))

(deftest aridad-test (testing "Prueba de la funcion: aridad"
                       (is (= 0 (aridad (symbol ","))))
                       (is (= 0 (aridad 'SGN)))
                       (is (= 1 (aridad 'ATN)))
                       (is (= 2 (aridad '+)))
                       (is (= 3 (aridad 'MID3$)))))

(deftest cargar-linea-test (testing "Prueba de la funcion: cargar-linea"
                             (is (= '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
                             (is (= '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
                             (is (= '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
                             (is (= '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))))

(deftest contar-sentencias-test (testing "Prueba de la funcion: cargar-linea"
                                  (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
                                  (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
                                  (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))))

(deftest buscar-lineas-restantes-test (testing "Prueba de la funcion: buscar-lineas-restantes"
                                        (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
                                        (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
                                        (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
                                        (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
                                        (is (= (list '(10) '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
                                        (is (= (list '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
                                        (is (= (list '(15) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
                                        (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
                                        (is (= '((20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
                                        (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
                                        (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
                                        (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))))

(deftest continuar-linea-test (testing "Prueba de la funcion: continuar-linea"
                                (is (= (vector ':omitir-restante (vector (list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {})) (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))))

(deftest continuar-linea-test (testing "Prueba de la funcion: continuar-linea"
                                (is (= '() (extraer-data '(()))))
                                (is (= (list "HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))))


(deftest ejecutar-asignacion-test (testing "Prueba de la funcion: ejecutar-asignacion"
                                    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
                                    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5 Y 1}] (ejecutar-asignacion '(Y = 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}])))
                                    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
                                    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
                                    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))))

(deftest preprocesar-expresion-test (testing "Prueba de la funcion: preprocesar-expresion"
                                      (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
                                      (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))))

