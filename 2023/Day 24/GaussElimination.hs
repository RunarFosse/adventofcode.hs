module GaussElimination where

-- Haskell Gaussian Elimination implementation
-- https://github.com/aseem09/Gauss-Elimination-Algorithm

import Data.Ratio

type Vector = [Rational]
type Row = [Rational]
type Matrix = [Row]

-- Gauss Elimination: Solve matrix equation Ax = B
gaussEliminationFromEquation :: Matrix -> Matrix -> Vector
gaussEliminationFromEquation a b = gaussEliminationFromMatrix $ zipMatrix a b

-- Create augmented matrix from A and B
zipMatrix :: Matrix -> Matrix -> Matrix
zipMatrix [] [] = []
zipMatrix (x:xs) (y:ys) = (x ++ y) : (zipMatrix xs ys)

-- Gauss Elimination: Solve a given augmented matrix
gaussEliminationFromMatrix :: Matrix -> Vector
gaussEliminationFromMatrix matrix = traceBack $ gaussReduction matrix

-- Compute the row-reduced-echelon form of the matrix
gaussReduction :: Matrix -> Matrix
gaussReduction [] = []
gaussReduction matrix = r: gaussReduction rs
    where
        (r:rows) = pivotCheck matrix
        rs = map reduceRow rows
        -- Row reduction using row operations
        reduceRow row
            | (head row) == 0 = drop 1 row
            | otherwise = drop 1 $ zipWith (-) (map (*frac) row) r
            where
                frac = (head r)/(head row)

-- Check and swap row if pivot element is zero
pivotCheck :: Matrix -> Matrix
pivotCheck (r:rs)
    | (head r /= 0) = (r:rs)
    | otherwise = pivotCheck (rs ++ [r])

{- Reverse the rows and columns to make the calculation easier and undo
-- the column reversion before returning the solutions
-}
traceBack :: Matrix -> Vector
traceBack = reverse . traceBack' . reverse . map reverse

-- Use back substitution to calculate the solutions
traceBack' :: Matrix -> Vector
traceBack' [] = []
traceBack' (r:rows) = var : traceBack' rs
    where
        var = (head r)/(last r)
        rs = map substituteVariable rows
        substituteVariable (x:(y:ys)) = ((x-var*y):ys)