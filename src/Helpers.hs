module Helpers
    ( isText
    , isNumber
    , isDecimal
    , isBoolean
    , extractText
    , extractNumber
    , extractDecimal
    , extractBoolean
    , extractTexts
    , extractNumbers
    , extractDecimals
    , extractBooleans
    , foldNumbers
    , foldDecimals
    , foldTexts
    , foldBooleans
    , truthy
) where

import Syntax

isText :: Value -> Bool
isText (Text _) = True
isText _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isDecimal :: Value -> Bool
isDecimal (Decimal _) = True
isDecimal _ = False

isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

extractText :: Value -> String
extractText (Text s) = s
extractText _ = error "Called `extractText` on a non-text value"

extractNumber :: Value -> Integer
extractNumber (Number n) = n
extractNumber _ = error "Called `extractNumber` on a non-number value"

extractDecimal :: Value -> Double
extractDecimal (Decimal d) = d
extractDecimal _ = error "Called `extractDecimal` on a non-decimal value"

extractBoolean :: Value -> Bool
extractBoolean (Boolean b) = b
extractBoolean _ = error "Called `extractBoolean` on a non-boolean value"

extractTexts :: [Value] -> Either String [String]
extractTexts values =
    if all isText values
    then Right $ map extractText values
    else Left "Called `extractTexts` on a non-text value"

extractNumbers :: [Value] -> Either String [Integer]
extractNumbers values =
    if all isNumber values
    then Right $ map extractNumber values
    else Left "Called `extractNumbers` on a non-number value"

extractDecimals :: [Value] -> Either String [Double]
extractDecimals values =
    if all isDecimal values
    then Right $ map extractDecimal values
    else Left "Called `extractDecimals` on a non-decimal value"

extractBooleans :: [Value] -> Either String [Bool]
extractBooleans values =
    if all isBoolean values
    then Right $ map extractBoolean values
    else Left "Called `extractBooleans` on a non-boolean value"


foldNumbers :: (Integer -> Integer -> Integer) -> [Value] -> Either String Value
foldNumbers f values = fmap (Number . foldl1 f) (extractNumbers values)

foldDecimals :: (Double -> Double -> Double) -> [Value] -> Either String Value
foldDecimals f values = fmap (Decimal . foldl1 f) (extractDecimals values)

foldTexts :: (String -> String -> String) -> [Value] -> Either String Value
foldTexts f values = fmap (Text . foldl1 f) (extractTexts values)

foldBooleans :: (Bool -> Bool -> Bool) -> [Value] -> Either String Value
foldBooleans f values = fmap (Boolean . foldl1 f) (extractBooleans values)

truthy :: Value -> Bool
truthy (Boolean False) = False
truthy _ = True
