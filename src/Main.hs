{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Data.List (isPrefixOf)
import Text.Hspi
import Text.Hspi.Command
import System.Environment

main = do
  args <- getArgs
  let options = parseOptions $ preprocOpt args
  case options of
    Right opt -> mergeFiles opt
    Left msg -> do
      putStrLn msg
      help

