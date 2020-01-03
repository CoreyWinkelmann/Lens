{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens
import Control.Applicative
import Data.Char
-- import qualified Data.Map as M 
-- import qualified Data.Set as S 
-- import qualified Data.Text as T

data Ship = Ship { _name :: String , _numCrew :: Int } deriving (Show)

makeLenses ''Ship

data Temperature = Temperature { _location :: String , _kelvin :: Float } deriving (Show)

makeLenses ''Temperature

celsius :: Lens' Temperature Float
celsius = lens getter setter
    where
        getter = (subtract 273.15) . view kelvin
        setter temp c = set kelvin (c + 273.15) temp

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = ( f - 32 ) * ( 5 / 9 )

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
    where
        getter = celsiusToFahrenheit . view celsius
        setter temp f = set celsius ( fahrenheitToCelsius f ) temp 

data User =
    User { _firstName :: String
         , _lastName :: String
         , _email :: String
         } deriving (Show)

makeLenses ''User

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter
    where
        getter u = (view firstName u) <> " " <> (view lastName u)
        setter u n = set firstName fn (set lastName ln u)
            where
                w = words n
                fn = head w
                ln = concat $ tail w

data Promotion a = Promotion { _item :: a , _discountPercentage :: Double } deriving (Show)

item :: Lens ( Promotion a ) ( Promotion b ) a b
item = lens getter setter
    where
        getter :: Promotion a -> a
        getter = _item
        setter :: Promotion a -> b -> Promotion b
        setter promo newItem = promo { _item = newItem }

main :: IO ()
main = putStrLn "Hello, Haskell!"
