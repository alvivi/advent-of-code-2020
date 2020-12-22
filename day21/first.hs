#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

parseLine :: String -> (Set String, [String])
parseLine line = (Set.fromList $ words ingredients, allergens)
  where
    (ingredients, rhs) = break (== '(') line
    allergens = map (filter (/= ',')) $ tail $ words $ tail $ init rhs

parse :: String -> Map String (Set String)
parse = foldl step Map.empty . lines
  where
    doInsert recipe Nothing = Just recipe
    doInsert recipe (Just recipes) = Just $ Set.intersection recipe recipes
    insert recipe acc allergen = Map.alter (doInsert recipe) allergen acc
    step acc line = let (recipe, allergens) = parseLine line in foldl (insert recipe) acc allergens

parseRecipes :: String -> [[String]]
parseRecipes = map (words . fst . break (== '(')) . lines

ingredientsWithAllergens :: Map String (Set String) -> [[String]]
ingredientsWithAllergens allergenSets | Map.size allergenSets <= 0 = []
ingredientsWithAllergens allergenSets = map snd ingredients : ingredientsWithAllergens allergenSets'
  where
    allergenSets' = Map.filter ((> 0) . Set.size) setWithoutIngredients
    setWithoutIngredients = Map.map (removeIngredients ingredients) allergenSets
    removeIngredients ingredients set = foldl (flip Set.delete) set $ map snd ingredients
    step acc allergen recipe = if Set.size recipe == 1 then (allergen, head $ Set.elems recipe) : acc else acc
    ingredients = Map.foldlWithKey step [] allergenSets

main :: IO ()
main = do
  contents <- getContents
  let allergens = Set.fromList $ concat $ ingredientsWithAllergens $ parse contents
  print $ length $ filter (not . (`Set.member` allergens)) $ concat $ parseRecipes contents
