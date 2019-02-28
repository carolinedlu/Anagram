import System.Environment
import System.IO
import Data.Typeable
import Data.Char
import Data.List

-- This quicksort implementation comes from: http://learnyouahaskell.com/recursion
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

main = do
     input <- getArgs
     inputHandle <- openFile (head input) ReadMode
     contents <- hGetContents inputHandle
     let dictionary = lines contents
         ogWord = concat (tail input)
         lowercaseWord = map toLower ogWord
         sortedWord = quicksort lowercaseWord
         lowerCaseDictionary = (map . map) toLower dictionary
         sortedDictionary = map quicksort lowerCaseDictionary
         listAnaIndices = elemIndices sortedWord sortedDictionary
         listOfAnagrams = [dictionary!!x | x <- listAnaIndices]
         strAnagrams = [show a | a <- listOfAnagrams]
         finalList = intercalate "," strAnagrams
         in putStr ("The anagrams of " ++ ogWord ++ " are [" ++ finalList ++ "]\n")
     hClose inputHandle



