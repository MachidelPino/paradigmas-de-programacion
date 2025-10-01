module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> a)                   -- fConst  
         -> (Float -> Float -> a)          -- fRango
         -> (Expr -> a -> Expr -> a -> a)  -- fSuma
         -> (Expr -> a -> Expr -> a -> a)  -- fResta
         -> (Expr -> a -> Expr -> a -> a)  -- fMult
         -> (Expr -> a -> Expr -> a -> a)  -- fDiv
         -> Expr
         -> a
recrExpr fConst fRango fSuma fResta fMult fDiv e = case e of
                                                      Const n -> fConst n
                                                      Rango a b -> fRango a b
                                                      Suma e1 e2 -> fSuma e1 (rec e1) e2 (rec e2)
                                                      Resta e1 e2 -> fResta e1 (rec e1) e2 (rec e2)
                                                      Mult e1 e2 -> fMult e1 (rec e1) e2 (rec e2)
                                                      Div e1 e2 -> fDiv e1 (rec e1) e2 (rec e2)
                                                    where rec = recrExpr fConst fRango fSuma fResta fMult fDiv

foldExpr :: (Float -> a)                   -- fConst  
         -> (Float -> Float -> a)          -- fRango
         -> (a -> a -> a)                  -- fSuma
         -> (a -> a -> a)                  -- fResta
         -> (a -> a -> a)                  -- fMult
         -> (a -> a -> a)                  -- fDiv
         -> Expr
         -> a
foldExpr fConst fRango fSuma fResta fMult fDiv e = case e of
                                                      Const n -> fConst n
                                                      Rango a b -> fRango a b
                                                      Suma e1 e2 -> fSuma (rec e1) (rec e2)
                                                      Resta e1 e2 -> fResta (rec e1) (rec e2)
                                                      Mult e1 e2 -> fMult (rec e1) (rec e2)
                                                      Div e1 e2 -> fDiv (rec e1) (rec e2)
                                                    where rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr (\n g -> (n, g)) (\a b -> dameUno (a, b)) (evalAux (+)) (evalAux (-)) (evalAux (*)) (evalAux (/))

evalAux :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
evalAux op f1 f2 = \g -> case f1 g of
                            (x, g1) -> case f2 g1 of
                                        (y, g2) -> (op x y, g2)

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 xs) xs, g1)
                            where (xs, g1) = muestra f n g

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr (show) (\a b -> show a ++ "~" ++ show b)
          (\e1 re1 e2 re2 -> maybeParen (constructor e1 /= CESuma && noEsConsORango e1) re1 ++ " + " ++ maybeParen (constructor e2 /= CESuma && noEsConsORango e2) re2)
          (\e1 re1 e2 re2 -> maybeParen (noEsConsORango e1) re1 ++ " - " ++ maybeParen (noEsConsORango e2) re2)
          (\e1 re1 e2 re2 -> maybeParen (constructor e1 /= CEMult && noEsConsORango e1) re1 ++ " * " ++ maybeParen (constructor e2 /= CEMult && noEsConsORango e2) re2)
          (\e1 re1 e2 re2 -> maybeParen (noEsConsORango e1) re1 ++ " / " ++ maybeParen (noEsConsORango e2) re2)
            where noEsConsORango exp = constructor exp /= CEConst && constructor exp /= CERango 

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s