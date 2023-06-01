module Lib
    ( someFunc
    ) where

import Factorial
import TiposDeDatos

someFunc :: IO ()
someFunc = print $ show (factorial 10)
