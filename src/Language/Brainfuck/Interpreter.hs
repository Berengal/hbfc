{-# LANGUAGE RecordWildCards #-}
module Language.Brainfuck.Interpreter where

import Language.Brainfuck.Parser

import Control.Monad
import Data.Word
import Data.Char
import System.IO

{- [Machine state]

-}

data Zipper a = Z [a] a [a]
  deriving (Show, Eq)

data BFICode
  = BFIAdd
  | BFISub
  | BFIRig
  | BFILef
  | BFIJmF (Zipper BFICode)
  | BFIJmB (Zipper BFICode)
  | BFIInp
  | BFIOut

instance Show BFICode where
  show BFIAdd = "+"
  show BFISub = "-"
  show BFIRig = ">"
  show BFILef = "<"
  show (BFIJmF _) = "["
  show (BFIJmB _) = "]"
  show BFIInp = ","
  show BFIOut = "."

toBFICode :: [BFInst] -> [BFICode]
toBFICode = fst . go [] []
  where
    go :: [BFICode] -> [Zipper BFICode] -> [BFInst] -> ([BFICode], [Zipper BFICode])
    go _ _ [] = ([], [])
    go prev ~fjmps@(fjmp:fjmpTail) (i:is) = result
      where
        (next, bjmps) = go (head (fst result):prev) fjmps is
        result = case i of
          IncD -> (BFIAdd : next, bjmps)
          DecD -> (BFISub : next, bjmps)
          DRig -> (BFIRig : next, bjmps)
          DLef -> (BFILef : next, bjmps)
          JmpF -> let (next', (bjmp':bjmps')) = go (this:prev) (thisZ:fjmps) is
                      thisZ = Z prev this next'
                      this = BFIJmF bjmp'
                  in (this:next', bjmps')
          JmpB -> let this = BFIJmB fjmp
                      thisZ = Z prev this next'
                      (next', bjmps) = go (this:prev) fjmpTail is
                  in (this:next', thisZ:bjmps)
          Inp  -> (BFIInp : next, bjmps)
          Out  -> (BFIOut : next, bjmps)


data BFState = BF { dp :: (Zipper Word8)  -- Data pointer
                  , ip :: (Zipper BFICode) -- Instruction pointer
                  }
  deriving Show

setIp ip state = state{ip=ip}
setDp dp state = state{dp=dp}

freshState :: [BFICode] -> Maybe BFState
freshState (i:is) = Just $ BF (Z [] 0 []) (Z [] i is)
freshState _ = Nothing

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
  
  BFIAdd -> uptDp (dInc dp)
  BFISub -> uptDp (dDec dp)
  BFIRig -> uptDp (dRight dp)
  BFILef -> uptDp (dLeft dp)
  
  BFIJmF ip' -> if val dp == 0 then
                  uptIp ip'
                else
                  return nstate
  BFIJmB ip' -> if val dp /= 0 then
                  uptIp ip'
                else
                  return nstate
            
  BFIInp -> do
    eof <- isEOF
    if eof then return nstate else do
    c <- getChar
    uptDp (setVal dp (c2w c))
    
  BFIOut -> do putChar (w2c (val dp))
               return nstate
             
  where nstate = flip setIp state <$> iRight ip
        uptDp dp = return (setDp dp <$> nstate)
        uptIp ip = return (Just (setIp ip state))

run :: [BFICode] -> IO ()
run program = go (freshState program)
  where
    go Nothing = return ()
    go (Just s) = step s >>= go
