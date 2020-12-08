#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)

main :: IO ()
main =
  let
    buildProgram = foldl addInstruction Map.empty . zip [0..] . map words . lines
    addInstruction program (pc, ins) = Map.insert pc ins program
    runProgram program = runInstruction program 0 0 Set.empty
    readInt ('+':value) = read value
    readInt ('-':value) = negate $ read value
    readInt value = read value
    options program = program : options' program 0
    options' program i =
      case Map.lookup i program of
        Nothing -> []
        Just ("nop":rest) -> Map.insert i ("jmp":rest) program : options' program (i + 1)
        Just ("jmp":rest) -> Map.insert i ("nop":rest) program : options' program (i + 1)
        Just ("acc":_) -> options' program (i + 1)
    runInstruction program pc acc set =
      if Set.member pc set then
        (pc, acc)
      else
        case Map.lookup pc program of
          Nothing -> (pc, acc)
          Just ("nop":_) -> runInstruction program (pc + 1) acc (Set.insert pc set)
          Just ("acc":value:_) -> runInstruction program (pc + 1) (acc + readInt value) (Set.insert pc set)
          Just ("jmp":value:_) -> runInstruction program (pc + readInt value) acc (Set.insert pc set)
  in do
    contents <- getContents
    let program = buildProgram contents
    let programSize = Map.size program
    putStrLn $ show $ find (\(pc, _) -> pc >= programSize) $ map runProgram $ options program
