{-# LANGUAGE OverloadedStrings #-}
module Text.Hspi (
  Options,
  inputFile,
  outputFile,
  rootPath,
  importers,
  defaultOption,
  customOption,
  mergeFiles,
  getRelativePath,
  generalImporter,
  scriptImporter,
  cssImporter
) where

import Control.Applicative
import Data.List (partition, isPrefixOf)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP
import Network.Browser
import Network.HTTP.Proxy
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.Directory
import System.Environment
import System.FilePath.Posix

data Options = Options {
	         inputFile :: FilePath,
                 outputFile :: FilePath,
                 rootPath :: FilePath, 
                 importers :: [Importer]
               }

defaultOption :: FilePath -> Options
defaultOption path = Options {
                       inputFile = "index.html",
                       outputFile = "index.html",
                       rootPath = path,
                       importers = [scriptImporter, cssImporter]
                     }

customOption :: FilePath -> Options
customOption path = Options {
                       inputFile = "index.html",
                       outputFile = "index.html",
                       rootPath = path,
                       importers = []
                     }

type Converter = TagTree BS.ByteString -> IO (TagTree BS.ByteString)
type Importer = FilePath -> TagTree BS.ByteString -> IO (TagTree BS.ByteString)

-- read the index.html under the directory, and 
-- insert all the source files' contents into it.
mergeFiles :: Options -> IO ()
mergeFiles opts = do
  let root = rootPath opts
      inFile = inputFile opts
      outFile = outputFile opts
      imps = importers opts
  src <- BS.readFile $ getRelativePath root inFile
  let html = tagTree $ parseTags src
  merged <- mapM (tagImporter root imps) html
  outFile' <- checkFilePath outFile
  BS.writeFile outFile' . renderTags $ flattenTree merged

checkFilePath :: FilePath -> IO (FilePath)
checkFilePath path = do
  flg <- doesFileExist path
  if flg
    then do
      putStrLn $ path ++ " has already exists. Do you overwrite it?(Y/N)"
      ch <- getChar
      if ch == 'Y'
        then return path
        else avoidDupPath path 1
     else return path
  where 
    avoidDupPath path cnt = do
      let (p, ext) = splitExtension path
          nextPath = p ++ "-" ++ (show cnt) ++ ext
      flg <- doesFileExist nextPath
      if flg 
        then avoidDupPath path (cnt+1)
	else return nextPath

-- join root path and relative path
getRelativePath :: FilePath -> FilePath -> FilePath
getRelativePath root path
  | null path = rmSepsTail root
  | null root = rmSepsInit path
  | otherwise = rmSepsTail root </> rmSepsInit path
  where rmSepsInit [] = []
        rmSepsInit (x:xs) = if x == pathSeparator
                              then rmSepsInit xs
                              else (x:xs)
        rmSepsTail = reverse . rmSepsInit . reverse 

tagImporter :: FilePath -> [Importer] -> Converter
tagImporter path imps = unionImporters path imps

unionConverters :: [Converter] -> Converter
unionConverters xs = foldr (\x acc -> \y -> do
                                        z <- x y
                                        acc z) return xs

unionImporters :: FilePath -> [Importer] -> Converter
unionImporters path xs = unionConverters $ map ($ path) xs

-- Function to include the source contents.
-- It reads the source file and insert the contents,
-- when it finds tags that has an attribute, which is
-- the source file path.
generalImporter :: String -> String -> Importer
generalImporter tagName attrName = converter
  where 
    attrF = partition ((== (BS.pack attrName)) . fst) 
    converter root (TagBranch tagName' attrs ts)
      | (BS.pack tagName) == tagName' = 
          case attrF attrs of
            ([], xs) -> (TagBranch tagName' xs) <$> mapM (converter root) ts
	    ((y:_), xs) -> do
               src <- getSrcFile root $ BS.unpack $ snd y
               let innerTextTag = TagLeaf $ TagText src
               (TagBranch tagName' xs) <$> (innerTextTag:) <$> mapM (converter root) ts
      | otherwise = (TagBranch tagName' attrs) <$> mapM (converter root) ts 
    converter _ leaf@(TagLeaf _) = return leaf
                       
scriptImporter :: Importer
scriptImporter = generalImporter "script" "src"

cssImporter :: Importer
cssImporter = converter
  where 
    attrF = partition ((== (BS.pack "href")) . fst) 
    converter root (TagBranch tagName' attrs ts) 
      | (BS.pack "link") == tagName' = 
          case attrF attrs of
            ([], xs) -> (TagBranch tagName' xs) <$> mapM (converter root) ts
	    ((y:_), _) -> do
               src <- getSrcFile root $ BS.unpack $ snd y
               let innerTextTag = TagLeaf $ TagText src
               (TagBranch (BS.pack "style") [(BS.pack "type", BS.pack "text/css")]) <$> (innerTextTag:) <$> mapM (converter root) ts
      | otherwise = (TagBranch tagName' attrs) <$> mapM (converter root) ts
    converter _ leaf@(TagLeaf _) = return leaf

-- It reads an source file.
-- If the source path is HTTP/HTTPS Protocol, 
-- it'll get the file over network.
-- Otherwise, it reads the local file path.
getSrcFile :: String -> String -> IO BS.ByteString
getSrcFile root path 
  | isUrl path = do
      env <- lookupEnv (if isHTTP path then "HTTP_PROXY" else "HTTPS_PROXY")   
      resp <- browse $ do
	case env >>= parseProxy of
          Just proxy -> setProxy proxy
	  Nothing -> return ()
        request $ getRequest path 
      return . BS.pack . rspBody $ snd resp
   | otherwise = BS.readFile $ getRelativePath root path
  where
    isUrl str = "http:" `isPrefixOf` str || "https:" `isPrefixOf` str
    isHTTP str = "http:" `isPrefixOf` str

