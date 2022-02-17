module PreParser where

import Error ( Error (InvalidToken) )

import Data.Char (isUpper, isSpace) 
import Data.Either (partitionEithers)
import Data.List (stripPrefix)


checkString :: Show a => String -> Either [Error a] ()
checkString ws 
    | null errors = Right ()
    | otherwise   = Left errors
    where
        (errors,_) = partitionEithers . map catchError $ words ws

        catchError word 
            | check word = Right word 
            | otherwise  = Left (InvalidToken word)   
            where
                check a = isInstr a || isToken a 
                isToken = (`elem` tokens)
                tokens = ["try","catch","finally",";"]

isInstr :: String -> Bool
isInstr word = case stripPrefix "instr_" word of
    Just typePart -> let (_,suffix) = span isUpper typePart
                     in null suffix
    Nothing       -> False