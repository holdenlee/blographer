module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Network.HTTP
import Network.URI
import System.Environment
import Control.Concurrent.ParallelIO
import Text.Parsec
import Text.Parsec.String
import Data.Aeson

-- helper function for getting page content
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

getLocal :: String -> IO (IOSArrow XmlTree (NTree XNode))
getLocal url = do
  --contents <- runMaybeT $ openUrl url
  contents <- readFile url -- "test.html"
  return $ readString [withParseHTML yes, withWarnings no] contents

printList :: (Show a) => [a] -> IO ()
printList li = mapM_  (putStrLn . show) li

tiddly = do
  pg <- getLocal "gowers.html"
  t <- runX (pg >>> multi (hasName "div" >>> hasAttr "title"
                                    >>> hasAttrValue "modifier" (=="gowers"))
                >>> getAttrValue "title" &&& ( --arr id))
                    getChildren >>> hasName "pre" >>> getChildren >>> getText))
  let t' = map (second parseTiddly) t
  B.writeFile "polymath.txt" $ encode t'
  --forM t' (putStrLn . show) --(\(x,y) -> putStrLn x)

main = tiddly

fromEither :: b -> Either a b -> b
fromEither def x = case x of
                     Right y -> y
                     Left _ -> def

parseTiddly :: String -> [(String, Maybe String)]
parseTiddly = fromEither [] . parse (parseTiddly' []) ""

parseTiddlyLink :: Parser (String, Maybe String)
parseTiddlyLink = 
  do
    string "[["
    ans <- (do
             text <- many1 (noneOf "|]")
             ((do
                char '|'
                link <- many1 (noneOf "]")
                return (link, Just text)) <|>
              return (text, Nothing)))
    string "]]"
    return ans

parseTiddly' :: [(String, Maybe String)] -> Parser [(String, Maybe String)]
parseTiddly' li = 
    (eof >> return li) <|>
    try (do 
          p <- parseTiddlyLink
          parseTiddly' (p:li)) <|>
    (anyChar >> parseTiddly' li)
