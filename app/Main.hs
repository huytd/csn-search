{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Text.HTML.DOM
import Text.XML (Document)
import Text.XML.Cursor ((&|), hasAttribute, attributeIs, attribute, element, fromDocument, ($//), (>=>), (&//))
import Data.Text (unpack)
import Data.List
import System.Environment

data AlbumItem = AlbumItem
  { albumTitle :: String
  , albumUrl :: String
  } deriving ( Show )

parseUrl :: String -> IO Document
parseUrl url = do
  req <- parseRequest url
  doc <- httpSink req $ const sinkDoc
  return doc

parseAlbumData :: Document -> [AlbumItem]
parseAlbumData document = result where
  cursor = fromDocument document
  _links = cursor
    $// element "table" >=> attributeIs "class" "tbtable"
    &// element "tr"
    &// element "td" >=> attributeIs "style" "font-size: 10px;"
    &// element "a" >=> hasAttribute "title"
    &| attribute "href"
  _album_links = cursor
    $// element "table" >=> attributeIs "class" "tbtable"
    &// element "tr"
    &// element "a" >=> attributeIs "class" "musictitle"
    &| attribute "href"
  _titles = cursor
    $// element "table" >=> attributeIs "class" "tbtable"
    &// element "td" >=> attributeIs "style" "font-size: 10px;"
    &// element "a" >=> hasAttribute "title"
    &| attribute "title"
  _album_titles = cursor
    $// element "table" >=> attributeIs "class" "tbtable"
    &// element "a" >=> attributeIs "class" "musictitle"
    &| attribute "title"
  links = foldl (\a l -> if length l > 0 then a ++ [head l] else a) [] (_links ++ _album_links)
  titles = foldl (\a t -> if length t > 0 then a ++ [head t] else a) [] (_titles ++ _album_titles)
  zipped = zipWith (\a b -> (a, b)) links titles
  result = map (\t -> AlbumItem
                 { albumTitle = unpack $ snd t
                 , albumUrl = unpack $ fst t
                 }) zipped

formatOutput :: [AlbumItem] -> String
formatOutput albums = intercalate "\n\n" list where
  list = map (\a -> "- " ++ albumTitle a ++ "\n  " ++ albumUrl a) albums

addArgumentToParam :: String -> String -> String
addArgumentToParam list word =
  if isInfixOf "mode=" word then
    list ++ "&" ++ word
  else
    if length list > 0 then
      list ++ "+" ++ word
    else
      list ++ word

searchUrlFromParam :: [String] -> String
searchUrlFromParam args = url where
  params = foldl addArgumentToParam "" args
  url = "http://search.chiasenhac.vn/search.php?s=" ++ params

main :: IO ()
main = do
  args <- getArgs
  doc <- parseUrl $ searchUrlFromParam args
  putStrLn $ formatOutput ( parseAlbumData doc )
