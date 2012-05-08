module Haguruma (
    Step(..)
  , DepList(..)
  , schedule
  , dependencyList
  , dependencyList'
  , perform
  , defaultMain
  ) where

import Control.Monad (filterM, liftM, ap)
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as Map
import System.Directory (doesFileExist, doesDirectoryExist)


data Step = Step { name    :: String
                 , inputs  :: [FilePath]
                 , outputs :: [FilePath]
                 , action  :: IO ()
                 }

instance Eq Step where
  (Step n i o _) == (Step n' i' o' _) =  n == n'
                                      && i == i'
                                      && o == o'

instance Show Step where
  show s = "Step " ++ show (name s)

type DepList a = [(a, [a])]

-- | 'schedule dl ss' finds deep dependencies of given steps 'ss' by using a
-- dependency list 'dl', and returns the all steps in topological order.
-- This is based on a depth-first-search-based topological sort algorithm,
-- described in http://en.wikipedia.org/wiki/Topological_sorting, and also
-- supports cycle detection.
schedule :: DepList Step -> [Step] -> [Step]
schedule dl ss = let (sorted, _) = foldl (visit []) ([], []) ss
                 in  reverse sorted
  where
    visit :: [Step] -> ([Step], [Step]) -> Step -> ([Step], [Step])
    visit stk (sorted, visited) s
      | s `elem` stk     = let cyc = dropWhile (/= s) $ reverse $ s:stk
                           in  error $  "Found a cycle: "
                                     ++ intercalate " -> " (map show cyc)
      | s `elem` visited = (sorted, visited)
      | otherwise        = let deps = fromJust $ lookup s dl
                               (sorted', visited') = foldl (visit $ s:stk) (sorted, s:visited) deps
                           in  (s:sorted', visited')

dependencyList :: [Step] -> DepList Step
dependencyList steps = map (\s -> (s, directDeps s)) steps
  where
    directDeps s = map producer $ inputs s
    producer i = case Map.lookup i productProducerMap of
                   Just p  -> p
                   Nothing -> error $ "No producer of \"" ++ i ++ "\" found."
    productProducerMap = foldl (\m s -> foldl (\m' p -> Map.insert p s m') m $ outputs s) Map.empty steps

dependencyList' :: [Step] -> IO (DepList Step)
dependencyList' steps = return zip `ap` return steps `ap` mapM directDeps steps
  where
    directDeps s = liftM (map producer) $ filterM doesNotExist $ inputs s
    doesNotExist fp | last fp == '/' = doesDirectoryExist fp
                    | otherwise      = doesFileExist fp
    producer i = case Map.lookup i productProducerMap of
                   Just p  -> p
                   Nothing -> error $ "No producer of \"" ++ i ++ "\" found."
    productProducerMap = foldl (\m s -> foldl (\m' p -> Map.insert p s m') m $ outputs s) Map.empty steps

perform :: Step -> IO ()
perform s = do
  ok <- liftM and $ mapM doesFileExist $ outputs s
  if ok
    then do putStrLn $ "Skipping a step \"" ++ name s ++ "\"..."
    else do putStrLn $ "Executing a step \"" ++ name s ++ "\"..."
            action s

defaultMain :: [Step] -> [Step] -> IO ()
defaultMain steps targets = mapM_ perform $ schedule (dependencyList steps) targets
