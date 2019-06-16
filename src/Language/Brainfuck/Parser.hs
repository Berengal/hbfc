module Language.Brainfuck.Parser where

isBFInstr :: Char -> Bool
isBFInstr c = elem c "+-<>.,[]"

newtype BFProgram = BFProgram {getBFProgram :: String}
  deriving (Show)

-- All characters are valid, non-instructions are comments
parse :: String -> Maybe BFProgram
parse prog =
  let p = filter isBFInstr prog
  in if not (checkValid 0 p) then
       Nothing
  else Just (BFProgram p)
  where
    -- Make sure the brackets match up
    checkValid 0 [] = True
    checkValid _ [] = False
    checkValid n (i:is) | n < 0 = False
                        | otherwise = case i of
                            '[' -> checkValid (n+1) is
                            ']' -> checkValid (n-1) is
                            _   -> checkValid n is

