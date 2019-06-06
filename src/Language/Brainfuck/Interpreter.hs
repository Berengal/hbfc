{-# LANGUAGE RecordWildCards #-}
module Language.Brainfuck.Interpreter where

import Language.Brainfuck.Parser

import Control.Monad
import Data.Word
import Data.Char
import System.IO

{- [Machine state]

-}

data BFICode
  = BFIAdd BFICode
  | BFISub BFICode
  | BFIRig BFICode
  | BFILef BFICode
  | BFIJmF BFICode BFICode
  | BFIJmB BFICode BFICode
  | BFIInp BFICode
  | BFIOut BFICode
  | BFIEnd

instance Show BFICode where
  show (BFIAdd n) = '+':show n
  show (BFISub n) = '-':show n
  show (BFIRig n) = '>':show n
  show (BFILef n) = '<':show n
  show (BFIJmF n _) = '[':show n
  show (BFIJmB n _) = ']':show n
  show (BFIInp n) = ',':show n
  show (BFIOut n) = '.':show n
  show BFIEnd = ""

toBFICode :: [BFInst] -> BFICode
toBFICode = fst . go (error "invalid code (unmatched backjmp)")
  where
    go :: [BFICode] -> [BFInst] -> (BFICode, [BFICode])
    go _ [] = (BFIEnd, error "invalid code (unmatched fwdjmp)")
    go ~fjmps@(fjmp:fjmpTail) (i:is) = result
      where
        (next, bjmps) = go fjmps is
        result = case i of
          IncD -> (BFIAdd next, bjmps)
          DecD -> (BFISub next, bjmps)
          DRig -> (BFIRig next, bjmps)
          DLef -> (BFILef next, bjmps)
          JmpF -> let (next', (bjmp':bjmps')) = go (next':fjmps) is
                      this = BFIJmF next' bjmp'
                  in (this, bjmps')
          JmpB -> let this = BFIJmB next' fjmp
                      (next', bjmps) = go fjmpTail is
                  in (this, next':bjmps)
          Inp  -> (BFIInp next, bjmps)
          Out  -> (BFIOut next, bjmps)

data Zipper a = Z [a] a [a]
  deriving (Show, Eq)

data BFState = BF { dp :: (Zipper Word8)  -- Data pointer
                  , ip :: BFICode -- Instruction pointer
                  }
  deriving Show

setIp ip state = state{ip=ip}
setDp dp state = state{dp=dp}

freshState :: BFICode -> BFState
freshState i = BF (Z [] 0 []) i

val (Z _ v _) = v
setVal (Z l _ r) v = Z l v r
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
step state@BF{..} = case ip of
  
  BFIAdd n -> goNext n . setDp (dInc dp) $ state
  BFISub n -> goNext n . setDp (dDec dp) $ state
  BFIRig n -> goNext n . setDp (dRight dp) $ state
  BFILef n -> goNext n . setDp (dLeft dp) $ state
  
  BFIJmF n jmp -> if val dp == 0 then
                    goNext jmp state
                  else
                    goNext n state
  BFIJmB n jmp -> if val dp /= 0 then
                    goNext jmp state
                  else
                    goNext n state
            
  BFIInp n -> do eof <- isEOF
                 if eof then goNext n state else do
                 c <- getChar
                 goNext n . setDp (setVal dp (c2w c)) $ state
  BFIOut n -> do putChar (w2c (val dp))
                 goNext n state
  BFIEnd -> return Nothing
             
  where goNext n state = return (Just (setIp n state))

run :: BFICode -> IO ()
run program = go (Just $ freshState program)
  where
    go Nothing = return ()
    go (Just s) = step s >>= go
