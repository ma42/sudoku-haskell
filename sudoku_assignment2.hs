module Main where

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import System.IO
import Control.Monad
import System.Posix

main = do        
        --putStrLn "\n\n***** WELCOME THE BEST SUDOKU PROGRAM YOU HAVE EVER SEEN *****\nWrite path and name of the file you want to open"
        --filename <- putStr "Insert file path> " >> hFlush stdout >> getLine
        file <- readFile "easy5.txt"
        let fileBoards = map removeNewLine (splitString (== '=') file)
        print $ fileBoards!!0
        -- replLoop fileBoards
        -- return()

type Board = [(String, [Int])]

replLoop :: [String] -> IO ()
replLoop boards = do 
        putStr $ "\nCURRENT SUDOKU: \n\n" ++ prettyPrint (head boards)
        putStrLn "\nOPTIONS: \n\n:1 for solving sudoku and get a new\n:2 for solving all sudokus in file \n:3 for assigning value to a square and get help solving sudoku\n:quit to exit\n"
        input <- read'
        if input == ":quit"
            then do 
                putStrLn "\n********** EXITING PROGRAM **********"
                sleep 1
                return()
        else evalAndPrint' input boards >> sleep 2 >> replLoop (tail boards)

read' :: IO String
read' = putStr "Sudoku> " 
        >> hFlush stdout
        >> getLine

evalAndPrint' :: String -> [String] -> IO ()
evalAndPrint' input boards
    | input == ":1" = putStrLn $ "\nSOLVED SUDOKU: \n\n" ++ solveAndPrint (head boards) ++ "\n***************"
    | input == ":2" = printAll boards
    | input == ":3" = partlySolve (head boards)
    | otherwise = do 
                    putStrLn ("\n***** " ++ input ++ " IS NOT A VALID OPTION")
                    sleep 2
                    replLoop boards

partlySolve :: String -> IO ()
partlySolve rawBoard = do 
                        let board = fromJust (parseBoard rawBoard)
                        putStrLn ("\n" ++ prettyPrint (makeStringFromBoard board))
                        putStr "POSSIBLE MOVES:\n\n"
                        let possibleMovesList = possibleMoves board
                        
                        if null possibleMovesList
                            then do 
                                putStrLn "\n\nHURRAY YOU SOLVED THE SUDOKU!"
                                sleep 2
                                return()
                        else do 
                            mapM_ print possibleMovesList
                            putStrLn "\nTO ASSIGN A VALUE INSERT SQUARE NAME:"
                            square <- putStr "Insert square name> " >> hFlush stdout >> getLine
                            value <- putStr "Insert value> " >> hFlush stdout >> getLine
                            if not (validInput square value)
                                then do 
                                    putStrLn "\nINVALID INPUT - SQUARE MUST BE BETWEEN A1 AND I9, AND VALUE BETWEEN 1 AND 9"
                                    sleep 3
                                    partlySolve rawBoard
                            else do
                                let returnBoard = assign (digitToInt (head value)) square board
                                let returnString = makeStringFromBoard (fromJust returnBoard) 
                                if isNothing (parseBoard returnString)
                                    then do 
                                        putStrLn ("\nOH NO, THE SUDOKU CAN'T BE SOLVED WITH " ++ value ++ " ON " ++ square)
                                        sleep 1
                                        partlySolve rawBoard 
                                else partlySolve returnString 

validInput :: String -> String -> Bool
validInput square value = (length square == 2) 
                            && drop 1 square `elem` ["1","2","3","4","5","6","7","8","9"]
                            && take 1 square `elem` ["A","B","C","D","E","F","G","H","I"] 
                            && value `elem` ["1","2","3","4","5","6","7","8","9"]
                               
possibleMoves :: Board -> [(String, String)]
possibleMoves board = [ ( fst value, intersperse ',' (map intToDigit (snd value)) ) | value <- board , length (snd value) > 1 ]

printAll :: [String] -> IO ()
printAll boards = do 
                putStrLn "\n\n****** SOLVING ALL SUDOKUS FOUND IN FILE ******\n"
                mapM_ (putStrLn . solveAndPrint) (tail boards)
                putStrLn "******************\n"
cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols
unitlist :: [[String]]
unitlist = [cross rows [c] | c <- cols]
        ++ [cross [r] cols | r <- rows]
        ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]
infAllDigits = repeat allDigits
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

-- Splits a string at given delimiter. Returns list with substrings. 
splitString :: (Char -> Bool) -> String -> [String]
splitString p s = case dropWhile p s of
                    "" -> []
                    s' -> w : splitString p s''
                            where (w, s'') = break p s'

-- Takes a String as input returns input string without new line character.
removeNewLine :: String -> String
removeNewLine xs = [ x | x <- xs, x `notElem` "\n" ]

-- Takes a tuple with two functions and a tuple with two values and maps functions over values in the second tuple. 
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f1, f2) (x, y) = (f1 x, f2 y)

-- Takes two functions and a list, maps the first function over elements in list if second function returns True. 
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p xs = [ f x | x <- xs, p x]

-- Takes two Maybe values and returns the first maybe if it has a just value, otherwise it returns the second maybe.  
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr x y            
    | isJust x = x
    | otherwise = y

-- Takes a list of maybe elements and returns first maybe element that has a Just value.  
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs)  
    | isJust x = x
    | isNothing x = firstJust xs

-- Takes a value and a list of tuples. If list contains a tuple where its first value is equal to the function paramater value, 
-- the function returns the list of that tuple. Otherwise the function returns an empty list.  
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ []          =  []
lookupList key ((x,y):xs)
    | key == x          =  y
    | otherwise         =  lookupList key xs

-- If the first argument is Nothing the function returns Nothing, otherwise it returns the result 
-- of the second parameter function applied on the value inside the first parameters Maybe context.
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind value func = func $ fromJust value

tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
    | x == y = Just (y':xs)
    | otherwise = fmap (x:) $ tryReplace y y' xs

-- Uses recursion with maybeBind to replace the first occurrence of different items in a list
recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] [] xs = Just xs
recursiveReplacement (a:as) (b:bs) xs = tryReplace a b xs `maybeBind` recursiveReplacement as bs

-- Sets an Int value to the string square in the Board and returns a new board.
setValue :: Int -> String -> Board -> Board
setValue n square = map (\p@(sq, _) -> if sq == square then (sq, [n]) else p)

-- Removes an Int value from a square values list of possible values and returns a new board. 
eliminateValue :: Int -> String -> Board -> Board
eliminateValue n square = map (\p@(sq, v) -> if sq == square then (sq, delete n v) else p)

-- Tries to eliminate a value from a square tuple in a board and returns a Maybe board with the value eliminated from the square tuple.
-- If there are no more values in the list of possible values left after removal the funtion returns the 'Nothing' value. 
eliminate :: Int -> String -> Board -> Maybe Board
eliminate n square board
    | null valueList = fail "No more possible values to insert"
    | board == board' = Just board'
    | otherwise = Just board'
    where 
        board' = eliminateValue n square board
        valueList = lookupList square board'

-- Assigns a value to a square and then calls a helper function that propagates elimination of that value from all the peers of the square.
assign :: Int -> String -> Board -> Maybe Board
assign n square board = assign' n (lookupList square peers) (setValue n square board)

-- Help function to assign that recursivly eliminates a value from the peers of a square. 
assign' :: Int -> [String] -> Board -> Maybe Board
assign' _ [] board = Just board
assign' n (p:peers) board = eliminate n p board >>= assign' n peers 

-- Takes a list of squares and tries to assign one of the values from the value list in the board tuple. 
-- If assigning the value is successful then the function continues to the next square and, if none of the possible values
-- can be assigned to the board, the next possible value for the square one recursion call above will be placed instead. 
solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (sq:squares) board = firstJust $ map (\value -> assign value sq board >>= solveSudoku' squares) (lookupList sq board)

solveSudoku :: String -> Maybe Board
solveSudoku board = parseBoard board >>= solveSudoku' squares 

makeStringFromBoard :: Board -> String
makeStringFromBoard board = concat [ if length x == 1 then map intToDigit x else ['0'] | x <- map snd board ]

addBars :: String -> String
addBars [] = []
addBars xs = take 3 xs ++ ['|'] ++ addBars (drop 3 xs)

lineBoard:: String -> String
lineBoard [] = []
lineBoard xs = take 36 xs ++ "---+---+----" ++ lineBoard (drop 36 xs)

splitLines:: String -> String
splitLines [] = []
splitLines xs = take 12 xs ++ "\n" ++ splitLines (drop 12 xs)

solveAndPrint xs = prettyPrint (makeStringFromBoard (fromJust (solveSudoku xs)))
prettyPrint xs = splitLines (lineBoard (addBars xs))
