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
import Control.Lens
import Data.Maybe (fromJust, isJust)
import Data.Aeson.Lens (_String, key)
import Data.List (partition, isPrefixOf)
import qualified Data.ByteString.Lazy.Char8 as BLS
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Base64 as B64
import qualified Network.Wreq as W
import OpenSSL.Session (context)
import Network.HTTP.Client.OpenSSL
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Regex.Posix
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

type Converter = TagTree BLS.ByteString -> IO (TagTree BLS.ByteString)
type Importer = FilePath -> TagTree BLS.ByteString -> IO (TagTree BLS.ByteString)

-- read the index.html under the directory, and 
-- insert all the source files' contents into it.
mergeFiles :: Options -> IO ()
mergeFiles opts = do
  let root = rootPath opts
      inFile = inputFile opts
      outFile = outputFile opts
      imps = importers opts
  src <- BLS.readFile $ getRelativePath root inFile
  let html = tagTree $ parseTags src
  merged <- mapM (tagImporter root imps) html
  outFile' <- checkFilePath outFile
  BLS.writeFile outFile' . renderTags $ flattenTree merged

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
    attrF = partition ((== (BLS.pack attrName)) . fst) 
    converter root (TagBranch tagName' attrs ts)
      | (BLS.pack tagName) == tagName' = 
          case attrF attrs of
            ([], xs) -> (TagBranch tagName' xs) <$> mapM (converter root) ts
            ((y:_), xs) -> do
               src <- getSrcFile root $ BLS.unpack $ snd y
               let innerTextTag = TagLeaf $ TagText src
               (TagBranch tagName' xs) <$> (innerTextTag:) <$> mapM (converter root) ts
      | otherwise = (TagBranch tagName' attrs) <$> mapM (converter root) ts 
    converter _ leaf@(TagLeaf _) = return leaf
                       
scriptImporter :: Importer
scriptImporter = generalImporter "script" "src"

cssImporter :: Importer
cssImporter = converter
  where 
    attrF = partition ((== (BLS.pack "href")) . fst) 
    converter root (TagBranch tagName' attrs ts) 
      | (BLS.pack "link") == tagName' = 
          case attrF attrs of
            ([], xs) -> (TagBranch tagName' xs) <$> mapM (converter root) ts
            ((y:_), _) -> do
               src <- getSrcFile root $ BLS.unpack $ snd y
               let innerTextTag = TagLeaf $ TagText src
               (TagBranch (BLS.pack "style") [(BLS.pack "type", BLS.pack "text/css")]) <$> (innerTextTag:) <$> mapM (converter root) ts
      | otherwise = (TagBranch tagName' attrs) <$> mapM (converter root) ts
    converter _ leaf@(TagLeaf _) = return leaf

-- It reads an source file.
-- If the source path is HTTP/HTTPS Protocol, 
-- it'll get the file over network.
-- Otherwise, it reads the local file path.
getSrcFile :: String -> String -> IO BLS.ByteString
getSrcFile root path 
  | isUrl path = do
      env <- lookupEnv (if isHTTP path then "HTTP_PROXY" else "HTTPS_PROXY")   
      let mProxyInfo = do
            env' <- env
            decompUrl env'
          mProxy = do
            (_, _, _, proxyUrl, port) <- mProxyInfo
            return $ W.Proxy (BS.pack proxyUrl) port
          mAuth = do
            (_, user, pass, _, _) <- mProxyInfo
            if length user > 0
              then return . B64.encode $ BS.pack (user ++ ":" ++ pass)
              else Nothing
          opts' = if isJust mProxy
                    then W.defaults & W.proxy ?~ (fromJust mProxy)
                    else W.defaults
          opts = if isJust mAuth
                   then opts' & W.header "Proxy-Authorization" .~ [(fromJust mAuth)]
                   else opts'
      putStrLn $ show opts
      putStrLn $ "Downloading a file from " ++ path
      case getProtocol path of
        Just "http" -> do
          res <- W.getWith opts path
          return $ res ^. W.responseBody
        Just "https" -> do
          res <- withOpenSSL $ W.getWith opts path
          return $ res ^. W.responseBody
        _ -> error $ "Can't get remote file. url: " ++ path
   | otherwise = BLS.readFile $ getRelativePath root path
  where
    isUrl str = "http:" `isPrefixOf` str || "https:" `isPrefixOf` str
    isHTTP str = "http:" `isPrefixOf` str
    getProtocol str = if isUrl str
                        then if "http:" `isPrefixOf` str
                               then Just "http"
                               else if "https:" `isPrefixOf` str
                                      then Just "https"
                                      else Nothing
                        else Nothing

decompUrl :: String -> Maybe (String, String, String, String, Int)
decompUrl path = do
      (protocol, user, pass, url, port) <- urlInfo
      if length port > 0
        then return (protocol, user, pass, url, read port)
        else case protocol of 
               "https" -> return (protocol, user, pass, url, 443)
               "http" -> return (protocol, user, pass, url, 80)
               _ -> Nothing
  where 
    urlInfo = let (_, _, _, xs) = path =~ ("([a-z]+)://(([a-zA-Z0-9_\\-]+):([a-zA-Z0-9_\\-]+)@)?([a-zA-Z0-9\\.]*)(:([0-9]+))?/?" :: String) 
                                  :: (String, String, String, [String])
              in if length xs > 0
                   then Just (xs !! 0, xs !! 2, xs !! 3, xs !! 4, xs !! 6)
                   else Nothing
