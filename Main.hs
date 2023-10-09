{-# LANGUAGE OverloadedStrings #-}

-- Import necessary libraries
import Data.Aeson -- Import Aeson for JSON parsing
import Control.Applicative ((<$>), (<*>)) -- Import applicative functions for dealing with Functors
import Control.Monad (mzero) -- Import mzero for dealing with failure in Monads
import qualified Data.ByteString.Lazy as B -- Import ByteString for handling byte string data
import Data.List
import Data.Char
import qualified Data.Map as M

data Pokemon = Pokemon { pokemonId :: Int
                       , name :: String
                       , types :: [String]
                       , weaknesses :: [String]
                       , description :: String
                       , evolutions :: [String]
                       , stats :: M.Map String Int
                       } deriving (Show) 

-- Define how to parse Pokemon data from JSON
instance FromJSON Pokemon where
  parseJSON (Object v) = do -- Define how to parse a JSON object into a Pokemon
    pid <- v .: "id" -- Extract the id field
    pname <- v .: "name" -- Extract the name field
    ptypes <- v .: "types" -- Extract the types field
    pweaknesses <- v .: "weaknesses" -- Extract the weaknesses field
    pevolutions <- map capitalize <$> v .: "evolutions" -- Extract and capitalize the evolutions field
    pstats <- v .: "stats" -- Extract the stats field
    pdescription <- map (\c -> if c == '\f' then ' ' else c) . map (\c -> if c == '\n' then ' ' else c) <$> v .: "description" 
    -- Extract the description field and replace newline with spaces
    return Pokemon {pokemonId = pid, name = pname, types = ptypes, weaknesses = pweaknesses, description = pdescription, evolutions = pevolutions, stats = pstats} 
    -- Construct a Pokemon object from the extracted fields
  parseJSON _ = mzero -- If the input is not a JSON object, fail

-- Function to keep running an action until it returns False
whileTrue :: IO Bool -> IO () -- Define a function that takes an IO action returning a Bool and keeps running it until it returns False
whileTrue action = do
  shouldContinue <- action -- Run the action and get its result
  if shouldContinue then whileTrue action else return () -- If the action returned True, run it again; otherwise, stop

-- Function to capitalize the first letter of a string
capitalize :: String -> String
capitalize [] = []
capitalize (head:tail) = toUpper head : map toLower tail -- If the input is not an empty string, capitalize the first character and make the rest lowercase

-- Function to format a Pokemon object into a string
formatPokemon :: Pokemon -> String -- Define a function that takes a Pokemon and returns a string representation of it
formatPokemon pokemon =
  "Id number: " ++ show (pokemonId pokemon) ++
  "\nName: " ++ capitalize (name pokemon) ++ -- Capitalize the name field
  "\nType(s): " ++ formatList "and" (types pokemon) ++ -- Format the types field
  "\nWeak to: " ++ formatList "," (weaknesses pokemon) ++ -- Format the weaknesses field
  "\nDescription: " ++ description pokemon ++ -- Format the description field
  "\nEvolutions: " ++ formatList "," (evolutions pokemon) ++ -- Format the evolutions field
  "\nStats: " ++ formatStats (stats pokemon) -- Format the stats field

-- Function to format a list into a string
formatList :: String -> [String] -> String -- Define a function that takes a separator and a list of strings and concatenates them with the separator in between
formatList _ [] = "" 
formatList _ [x] = x 
formatList conjunction [x,y] = x ++ " " ++ conjunction ++ " " ++ y -- If the list has two elements, join them with the conjunction
formatList separator (x:xs) = x ++ separator ++ " " ++ formatList separator xs 
-- If the list has more than two elements, join the first element with the rest of the list using the separator

formatStats :: M.Map String Int -> String 
formatStats stats = intercalate ", " $ map formatStat $ M.toList stats 
  where formatStat (name, value) = name ++ ": " ++ show value

-- Function for the main loop of the program
loop :: [Pokemon] -> IO Bool -- Define a function that takes a list of Pokemon and returns an IO action that interacts with the user and returns whether to continue or not
loop pokemons = do
  putStrLn "Enter 1 to search by name, 2 to search by ID, 3 to filter by type or 4 to quit:" 
  option <- getLine 
  case option of
    "1" -> do 
      putStrLn "Enter the name of a Pokémon:"
      query <- getLine 
      let lowerQuery = map toLower query -- Convert the response to lowercase
      let result = find (\p -> map toLower (name p) == lowerQuery) pokemons -- Find a Pokemon with a matching name
      case result of
        Just pokemon -> putStrLn (formatPokemon pokemon) >> return True -- If a Pokemon was found, print it and continue
        Nothing -> putStrLn "No Pokémon found with that name." >> return True -- If no Pokemon was found, print an error message and continue
    "2" -> do 
      putStrLn "Enter the ID of a Pokémon:" 
      query <- getLine 
      let result = find (\p -> pokemonId p == read query) pokemons -- Find a Pokemon with a matching ID
      case result of
        Just pokemon -> putStrLn (formatPokemon pokemon) >> return True 
        Nothing -> putStrLn "No Pokémon found with that ID." >> return True 
    "3" -> do 
      putStrLn "Enter a Pokémon type:" 
      query <- getLine 
      let lowerQuery = map toLower query
      let results = filter (\p -> lowerQuery `elem` (map (map toLower) (types p))) pokemons -- Find all Pokemon with a matching type
      case results of -- Depending on the results...
        [] -> putStrLn "No Pokémon found with that type." >> return True
        _ -> do
          putStrLn $ intercalate "\n\n" $ map formatPokemon results -- If any Pokemon were found, print them and continue
          return True
    "4" -> do -- If the user's response was "4"...
      putStrLn "Goodbye!" -- Print a farewell message
      return False -- Stop the loop
    _ -> do -- If the user's response was anything else...
      putStrLn "Invalid option. Please enter a number between 1 and 4." 
      return True -- Continue the loop

-- Main function
main :: IO () -- Define the main function that performs IO and doesn't return anything
main = do
  jsonData <- B.readFile "pokemon_data.json" -- Read the Pokemon data from a JSON file
  let maybePokemons = decode jsonData :: Maybe [Pokemon] -- Try to parse the JSON data into a list of Pokemon

  putStrLn "Welcome to MyPokedex! Here you can search pokemons by name, id, and filter them by type." -- Print a welcome message

  case maybePokemons of -- Depending on whether the parsing succeeded...
    Just pokemons -> whileTrue (loop pokemons) -- If it did, start the main loop with the parsed Pokemon
    Nothing -> putStrLn "Error loading Pokémon data." -- If it didn't, print an error message

