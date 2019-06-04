{-# LANGUAGE RecordWildCards #-}
module Language.Brainfuck.Interpreter where

import Language.Brainfuck.Parser

import Control.Monad
import Data.Word
import Data.Char
import System.IO

{- [Machine state]

-}

data BFState = BF { dp :: !(Zipper Word8)  -- Data pointer
                  , ip :: !(Zipper BFInst) -- Instruction pointer
                  }
  deriving (Show, Eq)

setIp ip state = state{ip=ip}
setDp dp state = state{dp=dp}

freshState :: [BFInst] -> Maybe BFState
freshState (i:is) = Just $ BF (Z [] 0 []) (Z [] i is)
freshState _ = Nothing

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

{- [Interpreter]
-}

step :: BFState -> IO (Maybe BFState)
step state@BF{..} = case val ip of
  IncD -> uptDp (dInc dp)
  DecD -> uptDp (dDec dp)
  DRig -> uptDp (dRight dp)
  DLef -> uptDp (dLeft dp)
  
  JmpF -> if val dp == 0 then
            uptIp (zipBracketF ip)
          else
            return nstate
  JmpB -> if val dp /= 0 then
            uptIp (zipBracketB ip)
          else
            return nstate
            
  Inp  -> do
    eof <- isEOF
    if eof then return nstate else do
    c <- getChar
    uptDp (setVal dp (c2w c))
    
  Out  -> do putChar (w2c (val dp))
             return nstate
             
  where nstate = flip setIp state <$> iRight ip
        uptDp dp = return (setDp dp <$> nstate)
        uptIp ip = return (flip setIp state <$> ip)

-- Zip to matching bracket helpers
zipBracketF :: Zipper BFInst -> Maybe (Zipper BFInst)
zipBracketF z = zipBracket JmpF JmpB iRight 0 z

zipBracketB :: Zipper BFInst -> Maybe (Zipper BFInst)
zipBracketB z = zipBracket JmpB JmpF iLeft 0 z

zipBracket up down dir = go
  where go n z | n == 1 && v == down = iRight z
               | v == up   = dir z >>= go (n+1)
               | v == down = dir z >>= go (n-1)
               | otherwise = dir z >>= go n
          where v = val z

run :: [BFInst] -> IO ()
run program = go (freshState program)
  where
    go Nothing = return ()
    go (Just s) = step s >>= go
