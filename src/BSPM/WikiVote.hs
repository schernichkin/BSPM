module BSPM.WikiVote where

import BSPM.SSSP

readWikiWote :: String -> IO Graph
readWikiWote path = do
  content <- readFile path
  let l = map (\l -> words l) $ filter (\l -> head l /= '#') $ lines content
  undefined
