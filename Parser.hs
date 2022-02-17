module Parser where

{-
 - This module holds relevant functions for parsing non-terminals of the given grammar.
 -
 - + Each consume"i" function parses some "i" non-terminal.
 -
 - + Lookaheads for each non terminal are deemed as pattern matchs or guards used to
 -   compare the following character to consume against the known lookaheads. Whenever
 -   it doesn't match an appropriate error is produced.
 -
 - + As for lambda productions, they're handled by returning the received inherited 
 -   attributes along with the exact same input. That is, we let the trouble of handling 
 -   lookaheads to callers.
 -
 - + Empty input is deemed as the "$" lookahead and properly considered where valid.
 -
 -}

import Error ( Error (..) )

import Data.Char (isSpace, isUpper)
import Data.List (stripPrefix, intercalate)


data Type 
    = Instr String 
    | EitherInst Type Type 
    deriving Show

parse :: String -> Either (Error Type) Type
parse word = case consumeS . cleanString $ word of 
    Left e      -> Left e 
    Right (remaining,exprType) 
        | null remaining -> Right exprType
        | otherwise      -> Left (TrailingInput exprType remaining)


-- Non terminal functions

-- Functions holding sintectic attributes only
consumeS, consumeB, consumeI :: String  -- ^ Input to consume
                             -> Either (Error a) (String, Type)

-- Functions that get inherited attributes (the ones with a related lambda production)
consumeC, consumeJ :: String  -- ^ Input to consume                     
                   -> Type    -- ^ Inherited attribute
                   -> Either (Error a) (String, Type)

consumeS []         = Left EmptyInput  
consumeS a@(x:xs) 
    | x `elem` "it" = consumeB a
    | otherwise     = Left (ExpectedChar "it" x)
 

consumeB []         = Left EndedEarlier
consumeB a@(x:xs)
    | x `elem` "it" = do
            (remaining,iType) <- consumeI a
            consumeC remaining iType
    | otherwise     = Left (ExpectedChar "it" x)


consumeC []       prevType  = Right ([],prevType) 
consumeC a@(x:xs) prevType 
    | x == ';'      = do
        (remaining,iType) <- consumeI xs
        consumeC remaining iType
    | x `elem` "cf" = Right (a,prevType)
    | otherwise     = Left (ExpectedChar "cf;" x)
                              

consumeI []       = Left EndedEarlier 
consumeI a@(x:xs) = case x of
    't' -> do 
        rem        <- toEither (stripPrefix "try" a) (ExpectedWord "try" (take 3 a))
        (b,bType)  <- consumeB rem            
        rem'       <- toEither (stripPrefix "catch" b) (ExpectedWord "catch" (take 4 b))
        (c,b2Type) <- consumeB rem'

        consumeJ c (EitherInst bType b2Type)
    'i' -> do
        typePart <- toEither (stripPrefix "instr_" a) (ExpectedWord "instr_" (take 6 a))

        let (typeString,rem)  = span isUpper typePart

        return (rem, Instr typeString)
           
    _   -> Left (ExpectedChar "it" x)


consumeJ []     prevType = Right ([],prevType)
consumeJ a@(x:xs) prevType
    | x == 'f'      = do
        rem <- toEither (stripPrefix "finally" a) (ExpectedWord "finally" (take 7 a))
        consumeB rem
    | x `elem` "c;" = Right (a,prevType)
    | otherwise     = Left (ExpectedChar "cf;" x) 


 -- Helper functions

toEither :: Maybe a -> b -> Either b a
toEither maybe left = case maybe of 
    Just a  -> Right a 
    Nothing -> Left left


cleanString :: String -> String
cleanString = filter (not . isSpace)