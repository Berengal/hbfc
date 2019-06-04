module Language.Brainfuck.Parser where

import Data.Maybe
import Data.Tuple

data BFInst= IncD | DecD | DRig | DLef | JmpF | JmpB | Inp | Out
  deriving (Eq)

insCharTable = [(IncD, '+'), (DecD, '-'), (DRig, '>'), (DLef, '<')
               , (JmpF, '['), (JmpB, ']'), (Inp, ','), (Out, '.')]

instance Show BFInst where
  show = pure . fromJust . flip lookup insCharTable

toInst :: Char -> Maybe BFInst
toInst = flip lookup (map swap insCharTable)

-- All characters are valid, non-instructions are comments
parse :: String -> Maybe [BFInst]
parse prog =
  let p = catMaybes (map toInst prog)
  in if not (checkValid 0 p) then
       Nothing
  else Just p
  where
    -- Make sure the brackets match up
    checkValid 0 [] = True
    checkValid _ [] = False
    checkValid n (i:is) | n < 0 = False
                        | otherwise = case i of
                            JmpF -> checkValid (n+1) is
                            JmpB -> checkValid (n-1) is
                            _    -> checkValid n is
