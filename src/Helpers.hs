module Helpers (
    isText,
    isNumber,
    isDecimal,
    isBoolean
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
