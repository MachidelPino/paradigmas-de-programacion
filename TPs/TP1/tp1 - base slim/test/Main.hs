module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 0 "" ~?= "",
      alinearDerecha 4 "hola" ~?= "hola",
      alinearDerecha (-3) "si" ~?= "si"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 2 (+ 10) [1, 2, 3] ~?= [1, 2, 13],
      actualizarElem 0 (+ 10) [] ~?= [],
      actualizarElem (-3) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 3 (+ 10) [1, 2, 3] ~?= [1, 2, 3]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 5 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100, -- El 100% de los valores está acá
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 6 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100 -- El 100% de los valores está acá
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3, 4] ~?= agregar 4 (agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5))))),
      histograma 4 (1, 5) [] ~?= vacio 4 (1, 5),
      histograma 4 (1, 5) [0, 5] ~?= agregar 5 (agregar 0 (vacio 4 (1, 5))),
      histograma 4 (1, 5) [3, 3] ~?= agregar 3 (agregar 3 (vacio 4 (1, 5)))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 0 (agregar 2 (agregar 4 (vacio 3 (0, 6)))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 33.333332,
              Casillero 2.0 4.0 1 33.333332,
              Casillero 4.0 6.0 1 33.333332,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar (-1) (agregar 6 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 1 50.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 50.0
            ],
      casilleros (histograma 3 (0, 6) [0, 2, 3, 4, 5, 5.5])
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 16.666666,
              Casillero 2.0 4.0 2 33.333332,
              Casillero 4.0 6.0 3 50.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
    ]

testsRecr :: Test
testsRecr =
  let literales = recrExpr (\n -> [n]) (\a b -> [a, b]) (\_ i _ d -> i ++ d) (\_ i _ d -> i ++ d) (\_ i _ d -> i ++ d) (\_ i _ d -> i ++ d)     -- Lista de constantes y numeros de rango
    in test
      [ literales (Const 5) ~?= [5.0],
        literales (Rango 1 5) ~?= [1.0, 5.0],
        literales (Suma (Const 2) (Const 3)) ~?= [2.0, 3.0],
        literales (Mult (Suma (Rango 5 9) (Const 2)) (Const 3)) ~?= [5.0, 9.0, 2.0, 3.0],
        literales(Div (Mult (Const 1) (Const 2)) (Resta (Const 3) (Const 4))) ~?= [1.0 , 2.0, 3.0, 4.0]
      ]

testsFold :: Test
testsFold =
  let profundidad = foldExpr (const 1) (\_ _ -> 1) (\i d -> 1 + max i d) (\i d -> 1 + max i d) (\i d -> 1 + max i d) (\i d -> 1 + max i d)
    in test
      [ profundidad (Const 2) ~?= 1,
        profundidad (Rango 1 10) ~?= 1,
        profundidad (Suma (Const 2) (Const 3)) ~?= 2,
        profundidad (Mult (Suma (Rango 5 9) (Const 2)) (Const 3)) ~?= 3,
        profundidad(Div (Mult (Const 1) (Const 2)) (Resta (Const 3) (Const 4))) ~?= 3
      ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Resta (Rango 2 6) (Const 1)) genFijo) ~?= 3.0,
      fst (eval (Resta (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= -0.32698154,
      fst (eval (Mult (Rango 1 3) (Rango 2 4)) genFijo) ~?= 6.0,
      fst (eval (Mult (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 8.74399,
      fst (eval (Div (Rango 4 8) (Const 2)) genFijo) ~?= 3.0,
      fst (eval (Div (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 0.8953669,
      fst (eval (Mult (Suma (Const 1) (Rango 2 4)) (Resta (Const 5) (Rango 1 2))) genFijo) ~?= 14.0
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ 
      casMinimo ((casilleros (fst (armarHistograma 20 5 (dameUno (1,10)) genFijo)))!!0) ~?= infinitoNegativo,
      casMaximo ((casilleros (fst (armarHistograma 6 2 (dameUno (1,30)) genFijo)))!!7) ~?= infinitoPositivo,

      length (casilleros (fst (armarHistograma 12 5 (dameUno (1,100)) genFijo))) ~?= 12+2,

      --en un histograma con genFijo, todos los valores terminan en el casillero del medio
      casPorcentaje ( (casilleros (fst (armarHistograma 12 5 (dameUno (1,100)) genFijo)))!!
           (div (length (casilleros (fst (armarHistograma 12 5 (dameUno (1,100)) genFijo)))) 2)) ~?= 100 
      

    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [ --Los números tanto del eval y del rango no deberian de importar ya que
      -- el rango minimo del primer casillero es siempre -infinito
      -- y el rango maximo del ultimo casillero es siempre infinito
      casMinimo ((casilleros (fst (evalHistograma 4 99 (Rango 0.0 10.0) genFijo))) !! 0)~?= infinitoNegativo,
      casMaximo ((casilleros (fst (evalHistograma 33 87 (Rango 37 70.4) genFijo)))  !! 34) ~?= infinitoPositivo,

      --3 es la cantidad de veces que fue ejecutado y todas caen en el mismo lugar por ser genFijo
      casCantidad ((casilleros (fst (evalHistograma 10 3 (Rango 0.0 10.0) genFijo)))
              !!(div (length (casilleros (fst (evalHistograma 10 3 (Rango 0.0 10.0) genFijo)))) 2)) ~?= 3, 

      --Si no hay rangos en la expresión, no importa que gen se pase, el Histograma es igual
      fst (evalHistograma 2 5 (Suma (Const 3) (Mult (Const 3) (Const 2))) genFijo) ~?= 
        fst (evalHistograma 2 5 (Suma (Const 3) (Mult (Const 3) (Const 2))) (genNormalConSemilla 50000))
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
