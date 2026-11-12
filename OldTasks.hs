
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name", "Average Number of Steps"] : (map op (tail m))
 where op :: [String] -> [String]   
       op [] = []    
       op (x:xs) = x : ((printf "%.2f") ((toFloatFromString (toStringFromInteger (sum (map toIntegerFromString xs)))) 
                                                    / toFloatFromString (toStringFromInt (length xs)))) : []

-- Function to convert a String to Integer:
toIntegerFromString :: String -> Integer
toIntegerFromString = read

-- Function to convert a String to Int:
toIntFromString :: String -> Int
toIntFromString = read

-- Function to convert a String to Float:
toFloatFromString :: String -> Float
toFloatFromString = read

-- Function to convert an Integer to String:
toStringFromInteger :: Integer -> String
toStringFromInteger = show

-- Function to convert an Int to String:
toStringFromInt :: Int -> String
toStringFromInt = show

-- Function to convert a Float to String:
toStringFromFloat :: Float -> String
toStringFromFloat = show


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = get_passed_people_num_aux m

-- Auxiliary function:
get_passed_people_num_aux :: Table -> Int
get_passed_people_num_aux (x:xs) = foldr op 0 xs
  where op :: Row -> Int -> Int
        op y acc 
          | (get_sum_steps_each_person y) > 999 = 1 + acc
          | otherwise = acc


-- Percentage of people who have achieved their goal:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = get_passed_people_percentage_aux m

-- Auxiliary function:
get_passed_people_percentage_aux :: Table -> Float
get_passed_people_percentage_aux (x:xs) = conversionIntToFloat (get_passed_people_num xs) / (conversionIntToFloat (length xs))

-- Sum of steps of a person:
get_sum_steps_each_person :: Row -> Float
get_sum_steps_each_person (x:xs) = sum (map toFloatFromString xs)

-- Function to convert an Int to Float:
conversionIntToFloat :: Int -> Float
conversionIntToFloat x = toFloatFromString (toStringFromInt x)

-- Function to convert an Integer to Float:
conversionIntegerToFloat :: Integer -> Float
conversionIntegerToFloat x = toFloatFromString (toStringFromInteger x)

-- Average number of daily steps:
get_steps_avg :: Table -> Float
get_steps_avg m = get_steps_avg_aux m

-- Auxiliary function:
get_steps_avg_aux :: Table -> Float
get_steps_avg_aux (x:xs) = (foldr (\y acc -> acc + get_sum_steps_each_person (y)) 0 xs) / conversionIntToFloat (length xs)


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (get_avg_steps_per_h_aux_2 m) 

-- Transposed Table:
get_transposeTable :: Table -> Table
get_transposeTable ([]:_) = []
get_transposeTable x = (map head x) : get_transposeTable (map tail x)

-- Average number of a row of a transposed Table:
get_row_avg :: Row -> Float
get_row_avg (x:xs) = conversionIntToFloat (sum (map toIntFromString xs)) / conversionIntToFloat (length xs)

-- Auxiliary function 1:
get_avg_steps_per_h_aux_1 :: Table -> Row
get_avg_steps_per_h_aux_1 [] = []
get_avg_steps_per_h_aux_1 (x:xs) = ((printf "%.2f") (get_row_avg (x))) : get_avg_steps_per_h_aux_1 xs

-- Auxiliary function 2:
get_avg_steps_per_h_aux_2 :: Table -> Table
get_avg_steps_per_h_aux_2 m = [get_avg_steps_per_h_aux_1 (tail (get_transposeTable m))]


-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = (get_line m) : (get_vam m) : (get_fam m) : (get_lam m) : []

get_line :: Table -> Row
get_line m = ["column", "range1", "range2", "range3"]

get_vam :: Table -> Row
get_vam m = "VeryActiveMinutes" : toStringFromInt (get_range 0 1 m) 
            : toStringFromInt (get_range 0 2 m) : [toStringFromInt (get_range 0 3 m)]

get_fam :: Table -> Row
get_fam m = "FairlyActiveMinutes" : toStringFromInt (get_range 1 1 m) 
            : toStringFromInt (get_range 1 2 m) : [toStringFromInt (get_range 1 3 m)]

get_lam :: Table -> Row
get_lam m = "LightlyActiveMinutes" : toStringFromInt (get_range 2 1 m) 
            : toStringFromInt (get_range 2 2 m) : [toStringFromInt (get_range 2 3 m)]

{- Number of people with active minutes of each range 
   a = 0 is for very active minutes
   a = 1 is for fairly active minutes
   a = 2 is for lightly active minutes
-} 
get_range :: Int -> Int -> Table -> Int
get_range a x m = foldr op 0 (tail ((get_transposeTable m) !! (a + 3)))
    where op :: String -> Int -> Int
          op s acc 
            | x == 1 = if toIntFromString (s) >= 0 && toIntFromString (s) < 50 
                       then acc + 1 
                       else acc
            | x == 2 = if toIntFromString (s) >= 50 && toIntFromString (s) < 100 
                       then acc + 1 
                       else acc
            | otherwise = if toIntFromString (s) >= 100 && toIntFromString (s) < 500 
                          then acc + 1 
                          else acc


-- Task 5

get_ranking :: Table -> Table
get_ranking m = ["Name", "Total Steps"] : get_two_columns m

get_two_columns :: Table -> Table
get_two_columns m = map (take 2) (mergeSort (tail m))

-- Sorting the elements from the Table:
merge :: Table -> Table -> Table
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
      | toIntFromString (x !! 1) < toIntFromString (y !! 1) = x : merge xs (y : ys)
      | toIntFromString (x !! 1) > toIntFromString (y !! 1) = y : merge (x : xs) ys
      | otherwise = if (x !! 0) < (y !! 0) 
                    then x : merge xs (y : ys) 
                    else y : merge (x : xs) ys

-- Spliting the Table in two halves:
halves :: Table -> (Table, Table)
halves xs = (take nr xs, drop nr xs)
    where nr = length xs `div` 2

-- MergeSort function:
mergeSort :: Table -> Table
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left, right) = halves xs

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : get_sorted (tail m)

-- Average of first 4h:
get_avg_first_4h :: Row -> String
get_avg_first_4h r = (printf "%.2f") (sum (map toFloatFromString (take 4 (tail r))) / 4)

-- Average of last 4h:
get_avg_last_4h :: Row -> String
get_avg_last_4h r = (printf "%.2f") (sum (map toFloatFromString (drop 4 (tail r))) / 4)

-- Difference between avg of first 4h and avg of last 4h:
get_difference :: Row -> String
get_difference r = (printf "%.2f") (abs (toFloatFromString (get_avg_last_4h r) - toFloatFromString (get_avg_first_4h r)))
    
-- Getting the row with the 4 columns:
get_row :: Row -> Row
get_row r = (r !! 0) : (get_avg_first_4h r) : (get_avg_last_4h r) : [get_difference r]

-- Getting the Table with all the modified rows:
get_steps_diff_table_aux :: Table -> Table
get_steps_diff_table_aux m = map get_row m
    
-- Sorting the modified Table:
get_sorted :: Table -> Table 
get_sorted m = mergeSortTask6 (get_steps_diff_table_aux m)

-- Sorting the elements from the Table:
mergeTask6 :: Table -> Table -> Table
mergeTask6 [] ys = ys
mergeTask6 xs [] = xs
mergeTask6 (x:xs) (y:ys) 
      | toFloatFromString (x !! 3) < toFloatFromString (y !! 3) = x : mergeTask6 xs (y : ys)
      | toFloatFromString (x !! 3) > toFloatFromString (y !! 3) = y : mergeTask6 (x : xs) ys
      | otherwise = if (x !! 0) < (y !! 0) 
                    then x : mergeTask6 xs (y : ys) 
                    else y : mergeTask6 (x : xs) ys

-- Spliting the Table in two halves:
halvesTask6 :: Table -> (Table, Table)
halvesTask6 xs = (take nr xs, drop nr xs)
    where nr = length xs `div` 2

-- MergeSort function:
mergeSortTask6 :: Table -> Table
mergeSortTask6 [] = []
mergeSortTask6 [x] = [x]
mergeSortTask6 xs = mergeTask6 (mergeSortTask6 left) (mergeSortTask6 right)
    where (left, right) = halvesTask6 xs


-- Task 7

-- Applies the given function to all the values:
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map_aux f m  

-- Applies the given function to a row:
map_row :: (Value -> Value) -> Row -> Row
map_row f = foldr ((:).f) []

-- Auxiliary function:
map_aux :: (Value -> Value) -> Table -> Table
map_aux f [] = []
map_aux f (x:xs) = (map_row f x) : (map_aux f xs) 

-- Function to test vmap function:
correct_table :: Table -> Table
correct_table = vmap (\x -> if x == "" then "0" else x) 


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f m)

get_sleep_total :: Row -> Row
get_sleep_total r = (head r) : ((printf "%.2f") (sum (map toFloatFromString (tail r)))) : []



{-
    TASK SET 2
-}


-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : (mergeSortTask1 (tail table) column (head table))

-- Function to convert a String to Int:
toIntFromString2 :: String -> Int
toIntFromString2 = read

-- Get index of an element in a list:
getIndex :: ColumnName -> Row -> Int
getIndex x xs = getIndex' x xs 0
  where getIndex' :: ColumnName -> Row -> Int -> Int
        getIndex' _ [] _ = -1
        getIndex' y (x:xs) i 
            | x == y = i
            | otherwise = getIndex' y xs (i + 1)

-- Check if a string is a numeric value
isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

-- Sorting the elements from the Table:
mergeT1 :: Table -> Table -> Row -> ColumnName -> Table
mergeT1 [] ys r column = ys
mergeT1 xs [] r column = xs
mergeT1 (x:xs) (y:ys) r column
        | isNumber (x !! (getIndex column r)) == True && isNumber (y !! (getIndex column r)) == True = 
            if toIntFromString2 (x !! (getIndex column r)) < toIntFromString2 (y !! (getIndex column r)) 
                then x : mergeT1 xs (y : ys) r column
                else if toIntFromString2 (x !! (getIndex column r)) > toIntFromString2 (y !! (getIndex column r)) 
                    then y : mergeT1 (x : xs) ys r column 
                    else if isNumber (x !! 0) == True && isNumber (y !! 0) == True
                        then if toIntFromString2 (x !! 0) < toIntFromString2 (y !! 0) 
                            then x : mergeT1 xs (y : ys) r column 
                            else y : mergeT1 (x : xs) ys r column 
                        else if (x !! 0) < (y !! 0) 
                            then x : mergeT1 xs (y : ys) r column 
                            else y : mergeT1 (x : xs) ys r column 
        | otherwise = if x !! (getIndex column r) < y !! (getIndex column r) 
                        then x : mergeT1 xs (y : ys) r column
                        else if x !! (getIndex column r) > y !! (getIndex column r) 
                            then y : mergeT1 (x : xs) ys r column 
                            else if isNumber (x !! 0) == True && isNumber (y !! 0) == True
                                then if toIntFromString2 (x !! 0) < toIntFromString2 (y !! 0) 
                                    then x : mergeT1 xs (y : ys) r column 
                                    else y : mergeT1 (x : xs) ys r column 
                                else if (x !! 0) < (y !! 0) 
                                    then x : mergeT1 xs (y : ys) r column 
                                    else y : mergeT1 (x : xs) ys r column 

-- Spliting the Table in two halves:
halvesTask1 :: Table -> (Table, Table)
halvesTask1 xs = (take nr xs, drop nr xs)
    where nr = length xs `div` 2

-- MergeSort function:
mergeSortTask1 :: Table -> ColumnName -> Row -> Table
mergeSortTask1 [] column r = []
mergeSortTask1 [x] column r = [x]
mergeSortTask1 xs column r = mergeT1 (mergeSortTask1 left column r) (mergeSortTask1 right column r) r column
    where (left, right) = halvesTask1 xs


-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = vunion_aux t1 t2

-- Auxiliary function for task2:
vunion_aux :: Table -> Table -> Table
vunion_aux (x:xs) (y:ys)
    | x == y = (x:xs) ++ ys
    | otherwise = (x:xs)

 
-- Task 3

hunion :: Table -> Table -> Table
hunion t1 t2 = hunion_aux (head t1) (head t2) t1 t2

hunion_aux :: Row -> Row -> Table -> Table -> Table
hunion_aux _ _ [] [] = []
hunion_aux h1 h2 (x:xs) [] = (concatRows h1 h2 x []) : (hunion_aux h1 h2 xs [])
hunion_aux h1 h2 [] (y:ys) = (concatRows h1 h2 [] y) :  (hunion_aux h1 h2 [] ys)
hunion_aux h1 h2 (x:xs) (y:ys) = (concatRows h1 h2 x y) : (hunion_aux h1 h2 xs ys)

-- Function to concatenate two rows:
concatRows :: Row -> Row -> Row -> Row -> Row
concatRows _ h2 r [] = fill (length h2) r
concatRows h1 _ [] r = reverse (fill (length h1) (reverse r))
concatRows _ _ r1 r2 = r1 ++ r2

-- Completing the row with a number of empty strings:
fill :: Int -> Row -> Row
fill x row 
    | x == 0 = row
    | otherwise = fill (x - 1) (row ++ [""])


-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = rmdupl (g (head t1) (head t1) (head t2) 
        (get3DMatrix (head (deleteAllApp [] (concatenateTables key_column (head t1) (head t2) t1 t2))) 
        (tail (deleteAllApp [] (concatenateTables key_column (head t1) (head t2) t1 t2)))))

-- Building a list of tables by combining a row with each row of a table:
-- Ex: a [b,c] -> [[a,b],[a,c]]
get3DMatrix :: Row -> Table -> [Table]
get3DMatrix _ [] = []
get3DMatrix header (x:xs) = (header : [x]) : (get3DMatrix header xs)

tjoin_aux :: String -> Row -> Row -> Table -> Table
tjoin_aux h hs1 hs2 ts
    | (check h hs2 == True) && ((last ts) !! ((length (head ts)) - (getIndex h (reverse (head ts))) - 1)) /= "" = 
        (aux h (head ts) (last ts)) : (swapAndDelete h (head ts) (last ts)) : []
    | (check h hs2 == True) && ((last ts) !! ((length (head ts)) - (getIndex h (reverse (head ts))) - 1)) == "" = 
        (aux h (head ts) (last ts)) : (deleteElem ((last ts) !! ((length (head ts)) - (getIndex h (reverse (head ts))) - 1)) (last ts)) : []
    | otherwise = ts

-- Swapping two elements and deleting the second element from the same row:
aux :: String -> Row -> Row -> Row
aux h hs xs = deleteLastElem (hs !! (getIndex h hs)) 
        (swap (hs !! (getIndex h hs)) (hs !! ((length hs) - (getIndex h (reverse (hs))) - 1)) hs)

-- Swapping two elements and deleting the second element:
swapAndDelete :: String -> Row -> Row -> Row
swapAndDelete h hs xs = deleteLastElem (xs !! (getIndex h hs)) 
        (swap (xs !! (getIndex h hs)) (xs !! ((length hs) - (getIndex h (reverse (hs))) - 1)) xs)

tjoin_aux2 :: Row -> Row -> Row -> Table -> Table
tjoin_aux2 [] _ _ ts = ts
tjoin_aux2 (h:hs) hs1 hs2 ts = tjoin_aux2 hs hs1 hs2 (tjoin_aux h hs1 hs2 ts)

g :: Row -> Row -> Row -> [Table] -> Table
g _ _ _ [] = []
g hs hs1 hs2 (t:ts) = (tjoin_aux2 hs hs1 hs2 t) ++ (g hs hs1 hs2 ts)

-- Check if a string is in a string list:
check :: String -> Row -> Bool
check _ [] = False
check h1 (h2:hs2) 
    | h1 == h2 = True
    | otherwise = (check h1 hs2) `myOR` False

-- Function for OR logic:
myOR :: Bool -> Bool -> Bool 
myOR True _ = True
myOR False x = x

-- Concatenate two rows:
concatenateRowToRow :: ColumnName -> Row -> Row -> Row -> Table -> Row
concatenateRowToRow _ _ _ _ [] = []
concatenateRowToRow key header1 header2 row (t:ts)
    | (row !! (getIndex key header1)) == (t !! (getIndex key header2)) = row ++ t
    | otherwise = concatenateRowToRow key header1 header2 row ts

-- Concatenate two tables:
concatenateTables :: ColumnName -> Row -> Row -> Table -> Table -> Table
concatenateTables _ _ _ [] _ = []
concatenateTables key header1 header2 (x:xs) ts = (concatenateRowToRow key header1 header2 x ts) 
                                                    : (concatenateTables key header1 header2 xs ts)

-- Deleting all occurrences of a row in table:
deleteAllApp :: Row -> Table -> Table
deleteAllApp _ [] = []
deleteAllApp row (t:ts) 
    | row == t = deleteAllApp row ts
    | otherwise = t : (deleteAllApp row ts)

-- Remove duplicates:
rmdupl :: Eq a => [a] -> [a]
rmdupl [] = []
rmdupl (x : xs) = x : rmdupl (filter (/= x) xs)

-- Swapping two elements:
swap :: String -> String -> Row -> Row
swap _ _ [] = []
swap n m (x:xs)
    | n == x = m : (swap n m xs)
    | m == x = n : (swap n m xs)
    | otherwise = x : (swap n m xs) 
 
-- Deleting last occurrence of an element:
deleteLastElem :: Eq a => a -> [a] -> [a]
deleteLastElem el list = reverse $ deleteElem el $ reverse list

-- Deleting first occurrence of an element:
deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem el (x:xs)
    | el == x = xs
    | otherwise = x : deleteElem el xs


-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : (cartesian_table_table new_row_function (tail t1) (tail t2))

-- Cartesian product of a row with a table:
cartesian_row_table :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesian_row_table f _ [] = []
cartesian_row_table f x (y:ys) = (f x y) : (cartesian_row_table f x ys)

-- Cartesian product of 2 tables:
cartesian_table_table :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesian_table_table f [] _ = []
cartesian_table_table f (x:xs) ys = (cartesian_row_table f x ys) ++ (cartesian_table_table f xs ys)


-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose (projection_aux columns_to_extract (transpose t))

-- Auxiliary function:
projection_aux :: [ColumnName] -> Table -> Table
projection_aux [] _ = []
projection_aux (x:xs) (y:ys) 
    | x == head y = y : (projection_aux xs (y:ys))
    | otherwise = projection_aux (x:xs) ys


-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (filterTable_aux condition (getIndex key_column (head t)) key_column (tail t))

-- Auxiliary function:
filterTable_aux :: (Value -> Bool) -> Int -> ColumnName -> Table -> Table
filterTable_aux condition nr key_column [] = []
filterTable_aux condition nr key_column (x:xs)
    | condition (x !! nr) == True = x : (filterTable_aux condition nr key_column xs)
    | otherwise = filterTable_aux condition nr key_column xs
