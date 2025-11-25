{-# LANGUAGE FlexibleInstances #-}

module Solution (Solution (..), wrapSolution) where

data Solution
    = IntegerSolution Integer
    | IntSolution Int
    | StringSolution [Char]

class WrapSolution a where
    wrapSolution :: a -> Solution

instance WrapSolution Integer where
    wrapSolution = IntegerSolution

instance WrapSolution Int where
    wrapSolution = IntSolution

instance WrapSolution [Char] where
    wrapSolution = StringSolution

instance Show Solution where
    show (IntegerSolution n) = show n
    show (IntSolution n) = show n
    show (StringSolution s) = s
