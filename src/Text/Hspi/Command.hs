{-# LANGUAGE OverloadedStrings #-}
module Text.Hspi.Command (
  help,
  preprocOpt,
  parseOptions
) where
import Data.List.Split
import Data.List (isPrefixOf)
import Text.Hspi
import System.Environment

mkPair :: String -> Either String (String, String)
mkPair str = if length ps == 2
               then Right (ps !! 0, ps !! 1)
               else Left $ "Parse Error. \"" ++ str ++ "\" is not pair."
  where ps = splitOn "," str

preprocOpt :: [String] -> [String]
preprocOpt xs = foldr accHelper [] xs
  where 
    accHelper x acc
      | "--input=" `isPrefixOf` x = ("-i":(drop 8 x):acc)
      | "--output=" `isPrefixOf` x = ("-o":(drop 9 x):acc)
      | "--add=" `isPrefixOf` x = ("-a":(drop 6 x):acc)
      | otherwise = (x:acc)

parseOptions :: [String] -> Either String Options
parseOptions [] = Right $ defaultOption "."
parseOptions (x:[]) = Right $ defaultOption x
parseOptions (x:xs) = do
  (pFlag, sFlag, cFlag, opts) <- foldl accHelper (Right (0, True, True, customOption x)) xs
  let imps = importers opts 
  case (pFlag, sFlag, cFlag) of
    (0, True, True) -> return $ opts { importers = (scriptImporter:cssImporter:imps) }
    (0, True, False) -> return $ opts { importers = (scriptImporter:imps) }
    (0, False, True) -> return $ opts { importers = (cssImporter:imps) }
    (0, False, False) -> return opts
    _ -> Left "Parse Error. The value according to the last option is missing." 
  where
    accHelper eOpts y = do
      acc@(pFlag, sFlag, cFlag, opts) <- eOpts 
      case y of
        "--no-script" -> return (pFlag, False, cFlag, opts)
        "--no-css" -> return (pFlag, sFlag, False, opts)
        "-i" -> setFlag acc 1 y
        "-o" -> setFlag acc 2 y
        "-a" -> setFlag acc 3 y
	opt@('-':_) -> unknownOption opt
	val -> case pFlag of
                 1 -> do
                        newOpts <- setInput val opts
                        return (0, sFlag, cFlag, newOpts)
		 2 -> do
                        newOpts <- setOutput val opts
                        return (0, sFlag, cFlag, newOpts)
                 3 -> do
                        newOpts <- setImporter val opts
                        return (0, sFlag, cFlag, newOpts)
                 _ -> Left $ "Sorry, You might find a Bug..."
    setFlag (pFlag, sFlag, cFlag, opts) v str = 
      if pFlag == 0
        then Right (v, sFlag, cFlag, opts)
        else parseError str
    setInput path opts
      | null path = Left $ "Failed to set input file path: \"" ++ path ++ "\""
      | otherwise = return $ opts { inputFile = path }
    setOutput path opts
      | null path = Left $ "Failed to set output file path: \"" ++ path ++ "\""
      | otherwise = return $ opts { outputFile = path }
    setImporter val opts = do
      ps <- foldr (\x acc -> do
                     acc' <- acc
                     p <- mkPair x
                     return (p:acc')) (Right []) $ splitOn "/" val
      let aImps = map (\(tag, attr) -> 
	                generalImporter tag attr) ps
          pImps = importers opts
      return $ opts { importers = (aImps ++ pImps) }
      
    parseError str = Left $ "Parse Error. Option \"" ++ str ++ "\" is at invalid position."
    unknownOption str = Left $ "Parse Error. Unknown Option \"" ++ str ++ "\""

help :: IO ()
help = do
  putStrLn expr
  putStrLn "Options:"
  mapM_ (\(opt, exp) -> putStrLn $ "  " ++ opt ++ ":\n      " ++ exp) options
  where expr = "hspi [project path] [-options]"
	options = [("--no-script", "Do not merge JavaScript file \n      (do not insert contents of .js files into a script tag)"),
                   ("--no-css", "Do not merge CSS file \n      (do not insert contents of .css files into a style tag)"),
                   ("--add=[tag1,attr1/tag2,attr2/...]", "Add optional pair of tag and attribute, which will be included into a file."),
                   ("-a [tag1,attr/tag2,attr2/...]", "The same as \"--add\""),
                   ("--input=[input file]", "Designate input html file name"),
 		   ("-i [input file]", "The same as \"--input\""),
                   ("--output=[output file]", "Designate output html file path"),
                   ("-o [output file]", "The same as \"--output\"")]
