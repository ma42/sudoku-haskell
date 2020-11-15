module Main where

    {-|
        ********************************************    HOW TO USE SUDOKU SOLVER    ********************************************

        1) Set the name (and path if applicable) of the .txt file you want to use for the program.
            This is done by writing the name/path to the "yourTextFileName.txt" string in main function below.
            File must contain sudoku boards given in the same format as in easy50.txt and inconsistent.txt files, 
            with 9 digits per line, 0 for empty square and ====== separating boards. There shoud not be a ===== after the last board. 

        2) Choose if you want to use the sudoku solver with a 4x4 or 9x9 grid. 
            Default is 9x9 grid, but flipping the variable line commenting under the main function will switch to 4x4 grid.  

        3a) Save file and load to GHCI. 
            Run 'main' and see a list of 'True' or 'False' stating if the sudukos in the input file can be solved or not. 

        3b) Alternativly, save file, compile with 'ghc -o nameOfOutputFile sudoku_verifier.hs' and run output file. 
            A list of 'True' or 'False' stating if the sudukos in the input file can be solved or not will be printed.  

        ************************************************************************************************************************
    -}


main = do  
        file <- readFile "easy50.txt"                   -- <- INSERT NAME OF TEXT FILE TO USE
        print $ map verifySudoku (splitString (== '=') file)


-- ******************************    CHANGE SUDOKU GRID SIZE    ******************************

rows = "ABCDEFGHI"          -- Comment out for 4x4 grid.
cols = "123456789"          -- Comment out for 4x4 grid.
nineGrid = True             -- Comment out for 4x4 grid.

-- rows = "ABCD"            -- Comment in for 4x4 grid.
-- cols = "1234"            -- Comment in for 4x4 grid.
-- nineGrid = False         -- Comment in for 4x4 grid.

-- ******************************************************************************************

-- Takes two lists as inputs and returns a nested list containing all the combinations of the elements in the input lists.
cross :: [a] -> [a] -> [[a]]
cross list1 list2 = [[x,y] | x <- list1, y <- list2]

-- Takes a String as input returns input string without new line character.
removeNewLine :: String -> String
removeNewLine xs = [ x | x <- xs, x `notElem` "\n" ]

-- Splits a string at given delimiter. Returns list with substrings. 
splitString :: (Char -> Bool) -> String -> [String]
splitString p s = case dropWhile p s of
                    "" -> []
                    s' -> w : splitString p s''
                            where (w, s'') = break p s'

-- Type casts digit to int. 
digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'

-- Takes a string as input and returns a tuple with a String, the square string, and an Int, the value currently places at that square. 
parseBoard :: String -> [(String, Int)]
parseBoard boardString = zip (cross rows cols) (map digitToInt $ removeNewLine boardString)

-- Produces a nested list containing the square strings of all rows, all cols and all boxes.
unitList :: [[String]]
unitList = 
    [ cross [r] cols | r <- rows ] ++ 
    [ cross rows [c] | c <- cols ] ++ 
    if nineGrid
        then [ cross rs cs | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789"]] 
    else [ cross rs cs | rs <- ["AB", "CD"], cs <- ["12", "34"]] 
    
-- Takes a square string as input and returns a nested list containing all units
-- from the unitList that contains the square string.
filterUnitList :: String -> [[String]]
filterUnitList square = [ u | u <- unitList, square `elem` u ]

-- Creates a list of tuples where the first value is a String, the square string, and second being a nested list of the 
-- units which the square strings belong to.
units :: [(String, [[String]])]
units = [ (s, filterUnitList s) | s <- cross rows cols ]

-- Takes a nested list and returns a flattened list containing all the elements of the sublists of the input.
foldList :: [[a]] -> [a]
foldList = concat
 
-- Takes a list as input and returns a list containing the same elements, but with duplicates removed. 
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

-- Creates the a nested list containing tuples of every square string as first value and as a second value, the flattened list 
-- of the units it belongs to, without duplicates and itself.
peers :: [(String, [String])]
peers = [ (fst t, filter (\x -> x/= fst t) (removeDuplicates . foldList $ snd t) )  | t <- units ]

-- Using the Maybe type; the first parameter is returned if no value is encapsulated, otherwise the Just value
-- will be returned.  
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

-- Returns the peers of the first parameter (the square string) using the peers value.
getPeers :: String -> [String]
getPeers square =  fromMaybe [square] (lookup square peers) 

-- Takes a list of Maybe values and returns a list of the values that were encapsulated.
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList (Just x:xs)  = x : justifyList xs

-- Takes a list of input values and list of tuples, returns list of the tuples values corresponding with the input list. 
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs table = justifyList [ lookup x table | x <- xs]

-- Takes square tuple and board list as inputs. If square has no value it returns a tuple with
-- a list of values that are ok to place, if no value can be found it returns a tuple with an empty list.  
-- If square already contains a value that is valid, it returns a tuple with the square value and its current value in a list. 
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, 0) board = 
    if nineGrid
        then (sq, filter (\nbr -> nbr `notElem` lookups (getPeers sq) board) [1..9]) 
    else (sq, filter (\nbr -> nbr `notElem` lookups (getPeers sq) board) [1..4])
validSquareNumbers (sq, v) board = (sq, [v])

-- Takes a list of square tuples and returns a list containing the tuples of 
-- all squares and its possible values for that suduko board.
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board = [ validSquareNumbers t board | t <- board ]

-- Takes a list and if all values in the list are unique it returns True, otherwise returns False. 
uniqeNbrs :: Eq a => [a] -> Bool
uniqeNbrs [] = True
uniqeNbrs (x:xs) = notElem x xs && uniqeNbrs xs

-- Takes a list of square units and a list of tuples with square strings and a list of all
-- values that could be placed on that square. Resturns boolean if a unit is valid in a board or not. 
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit board =
    if nineGrid
        then all (`elem` concat (lookups unit board)) [1 .. 9]
               &&
                uniqeNbrs (concat (filter ((== 1) . length) (lookups unit board)))
    else all (`elem` concat (lookups unit board)) [1..4] 
          && 
            uniqeNbrs (concat (filter ((== 1) . length) (lookups unit board)))

-- Takes a list of tuples with square strings and the value on that squares. 
-- Checks if all units in the variable unitList are valid for a Sudoku board and returns boolean. 
validUnits :: [(String, Int)] -> Bool
validUnits board = all (\unit -> validUnit unit (validBoardNumbers board)) unitList

-- Checks weather a sudoku board is valid or not, returns boolean. 
verifySudoku :: String -> Bool
verifySudoku xs = validUnits (parseBoard xs)

