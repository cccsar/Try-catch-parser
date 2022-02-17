module Error where

import Data.List(intercalate)

data Error a
    = InvalidToken String
    | EmptyInput
    | EndedEarlier 
    | ExpectedChar String Char
    | ExpectedWord String String
    | TrailingInput a String

instance Show a => Show (Error a) where

    show EmptyInput                     = "Empty Input"
    show EndedEarlier                   = "Input ended before expected"
    show (ExpectedChar options found)   = "Expected one of "++ asList options ++" Found; "
                                          ++ [found] ++ "."
    show (ExpectedWord actual found)    = "Found \""++found++ "\" where \"" 
                                          ++ actual ++ "\" should be."
    show (InvalidToken token)           = "\""++token
                                          ++"\" is not a known word in the language."
    show (TrailingInput exprType input) = "Parsed: "++ show exprType 
                                          ++ " but found trailing input \""++input++"\"."

asList :: Show a => [a] -> String
asList = intercalate ", " . map show