module TryCatch where

import Parser ( parse , Type (..))
import PreParser ( checkString )
import Error (Error (..))


main :: IO ()
main = do
    putStr "Provide a valid try-catch expression: "
    input <- getLine 

    case (checkString input :: Either [Error Type] ()) of 
        Right _     -> do

                case parse input of
                    Right exprType -> putStrLn ("The type of the given expression is: " ++ show exprType)
                    Left err       -> print err

        Left errors -> mapM_ print errors