{-# LANGUAGE OverloadedStrings #-}
module Day12 where
import Data.Char(isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.List.NonEmpty(NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NonEmpty

-- Our graph type, as an adjacency list
type Graph = HashMap Text [Text]

insertEdge :: (Text, Text) -> Graph -> Graph
insertEdge (v1, v2) = HashMap.insertWith (++) v1 [v2]
    . HashMap.insertWith (++) v2 [v1]

graphFromEdges :: [(Text, Text)] -> Graph
graphFromEdges = foldr insertEdge HashMap.empty

splitEdge :: Text -> (Text, Text)
splitEdge edge = let (v1, v2) = Text.break (=='-') edge in
    (v1, Text.drop 1 v2)

readInput :: IO Graph
readInput = graphFromEdges . fmap splitEdge . Text.lines <$> Text.IO.readFile "input/day12.txt"

isBig :: Text -> Bool
isBig = Text.all isUpper

-- Given a graph, a start vertex, and an end vertex, find the list of all
-- paths (as lists of vertexes) between them
listPaths :: Graph -> Text -> Text -> [NonEmpty Text]
listPaths graph start end = go graph (pure start) end
  where 
    go :: Graph -> NonEmpty Text -> Text -> [NonEmpty Text]
    go graph path@(currVtx :| _) end 
        | currVtx == end = return path
        | otherwise = do
            nextVtx <- filter (\t -> isBig t || (t `notElem` path)) $ HashMap.findWithDefault [] currVtx graph
            go graph (nextVtx <| path) end

part1 :: Graph -> Int
part1 g = length $ listPaths g "start" "end"

-- keep track of not only the path but also whether or not
-- we can still visit a small room twice
type Path = (NonEmpty Text, Bool)

canVisit :: Path -> Text -> Bool
canVisit (path, canRevisit) vtx
    | vtx == "start" || vtx == "end" = vtx `notElem` path 
    | isBig vtx = True
    | otherwise = vtx `notElem` path || canRevisit

appendToPath :: Path -> Text -> Path
appendToPath (path, canRevisit) vtx
    | not canRevisit = (vtx <| path, False)
    | vtx `elem` NonEmpty.filter (not . isBig) path = (vtx <| path, False)
    | otherwise = (vtx <| path, True)

-- largely copypasted from above, but with the new filter function and modified
-- to work with our extended path type
listPaths2 :: Graph -> Text -> Text -> [Path]
listPaths2 graph start end = go graph (pure start, True) end
  where 
    go :: Graph -> Path -> Text -> [Path]
    go graph path@(currVtx :| _, _) end 
        | currVtx == end = return path
        | otherwise = do
            nextVtx <- filter (canVisit path) $ HashMap.findWithDefault [] currVtx graph
            go graph (appendToPath path nextVtx) end

part2 :: Graph -> Int
part2 g = length $ listPaths2 g "start" "end"
