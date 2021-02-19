module Lib
    ( Grid
    , Matrix
    , Choices
    , singleton
    , blank
    , choices
    , transpose
    , group
    , ungroup
    , rows
    , cols
    , boxs
    , nodups
    , ok
    , search
    , solve
    , parse
    ) where

import Data.List ((\\))

type Row a = [a]
type Matrix a = [Row a]
type Digit = Char
type Grid = Matrix Digit
type Choices = [Digit]
type Endo = Matrix Choices -> Matrix Choices

digits  = "123456789"
blank   = (==) '.'
boxSize = 3

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

choices :: Grid -> Matrix Choices
choices = map $ map chcs
    where chcs digit = if blank digit then digits else [digit]
    
-- ####################################################### ENDOMORPHISMS

transpose :: Matrix a -> Matrix a
transpose [xs]       = map (:[]) xs
transpose (xs : xss) = zipWith (:) xs (transpose xss)

group :: [a] -> [[a]]
group [] = []
group xs = take boxSize xs : group (drop boxSize xs)

ungroup :: [[a]] -> [a]
ungroup = concat

rows :: Endo
rows = id

cols :: Endo
cols = transpose

boxs :: Endo
boxs = matrixUngroup . map transpose . matrixGroup
    where 
        matrixUngroup = map ungroup . ungroup
        matrixGroup   = group . map group

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
    where 
        fixed = [d | [d] <- row]
        remove xs ds = if singleton ds then ds else ds \\ xs

pruneBy :: Endo -> Endo
pruneBy f = f . map pruneRow . f

prune :: Endo
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- ####################################################### PREDICATORS

nodups :: Choices -> Bool
nodups []       = True
nodups (x : xs) = notElem x xs && nodups xs

ok :: Row Choices -> Bool
ok row = nodups [d | [d] <- row] 

valid :: Matrix Choices -> Bool
valid chcs = all ok (rows chcs) && 
             all ok (cols chcs) && 
             all ok (boxs chcs)

completed :: Matrix Choices -> Bool
completed = all (all singleton)

-- ####################################################### PARSER

parse :: String -> Grid
parse "" = []
parse xs = take l xs : parse(drop l xs)
    where l = 3 * boxSize

-- ####################################################### SOLVER

expand :: Matrix Choices -> [Matrix Choices]
expand rows = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where (rows1, row : rows2) = break (any smallest) rows
          (row1, cs : row2)    = break smallest row
          smallest cs          = length cs == n
          break p xs           = span (not . p) xs
          counts               = filter (/= 1) . map length . concat
          n = minimum (counts rows)

search :: Matrix Choices -> [Grid]
search m | not (valid m) = []
         | completed m'  = [map (map head) m']
         | otherwise     = concatMap search $ expand m'
         where m' = prune m

solve :: Grid -> Grid
solve = head . search . choices