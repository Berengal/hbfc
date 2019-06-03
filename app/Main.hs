{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Word
import Data.Maybe
import Data.Tuple
import Data.Char
import System.IO
import System.Environment

{- [Instructions and parsing]

-}

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

{- [Machine state]

-}

data BFState = BF { dp :: !(Zipper Word8)  -- Data pointer
                  , ip :: !(Zipper BFInst) -- Instruction pointer
                  }
  deriving (Show, Eq)

setIp ip state = state{ip=ip}
setDp dp state = state{dp=dp}

data Zipper a = Z [a] !a [a]
  deriving (Show, Eq)

val (Z _ v _) = v
setVal (Z l _ r) v = Z l v r
iRight (Z l v (r:rs)) = Just $ Z (v:l) r rs
iRight _ = Nothing
iLeft  (Z (l:ls) v r) = Just $ Z ls l (v:r)
iLeft _ = Nothing
dRight (Z l v r) | null r    = Z (v:l) 0 []
                 | otherwise = Z (v:l) (head r) (tail r)
dLeft (Z l v r) | null l    = Z [] 0 (v:r)
                | otherwise = Z (tail l) (head l) (v:r)
dInc (Z l v r) = Z l (v+1) r
dDec (Z l v r) = Z l (v-1) r

-- Word8 <-> Char
w2c = chr . fromIntegral
c2w = fromIntegral . ord

freshState :: [BFInst] -> BFState
freshState (i:is) = BF (Z [] 0 []) (Z [] i is)

{- [Interpreter]
-}

step :: BFState -> IO (Maybe BFState)
step state@BF{..} = case val ip of
  IncD -> uptDp (dInc dp)
  DecD -> uptDp (dDec dp)
  DRig -> uptDp (dRight dp)
  DLef -> uptDp (dLeft dp)
  JmpF -> if val dp == 0 then
            return (flip setIp state <$> zipBracketF 0 ip)
          else
            return nstate
  JmpB -> if val dp /= 0 then
            return (flip setIp state <$> zipBracketB 0 ip)
          else
            return nstate
  Inp  -> do
    eof <- isEOF
    if eof then return Nothing else do
    c <- getChar
    uptDp (setVal dp (c2w c))
  Out  -> do putChar (w2c (val dp))
             return nstate
  where nstate = flip setIp state <$> iRight ip
        uptDp dp = return (setDp dp <$> nstate)

-- Zip to matching bracket helpers
zipBracketF :: Int -> Zipper BFInst -> Maybe (Zipper BFInst)
zipBracketF n z = zipBracket JmpF JmpB iRight n (Just z)

zipBracketB :: Int -> Zipper BFInst -> Maybe (Zipper BFInst)
zipBracketB n z = zipBracket JmpB JmpF iLeft n (Just z)

zipBracket up down dir = go
  where go _ Nothing = Nothing
        go n (Just z) | n == 1 && v == down = iRight z
                      | v == up   = go (n+1) z'
                      | v == down = go (n-1) z'
                      | otherwise = go n z'
          where v = val z
                z' = dir z

run :: [BFInst] -> IO ()
run program = go (freshState program)
  where
    go s = do
      s <- step s
      case s of
        Nothing -> return ()
        (Just s) -> go s

-- Read and execute first arg, if no args then execute first line using rest of
-- stdin as input
main :: IO ()
main = do
  args <- getArgs
  prog <- if null args then
            getLine
          else
            readFile (head args)
  case parse prog of
    Nothing -> putStrLn "Invalid Program"
    Just p  -> do
      run p
