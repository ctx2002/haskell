module STAL

(splitList)
where
import           Data.List
import           DB

characters = nub [x | ["play",_,_,x] <- db ]
movies =   [x | ["release",x,_] <- db]
actors = nub [ x | ["play",x,_,_] <- db ]
directors = nub [ x | ["direct",x,_] <- db ]
dates = nub [ x | ["release",_,x] <- db ]
universe = nub (characters++actors++directors++movies++dates)

direct = [ (x,y) | ["direct",x,y] <- db ]
act = [ (x,y) | ["play",x,y,_] <- db ]
play = [ (x,y,z) | ["play",x,y,z] <- db ]
release = [ (x,y) | ["release",x,y] <- db ]

charP x = x `elem` characters
actorP x = x `elem` actors
movieP x = x `elem` movies
directorP x =  x `elem` directors
dateP x = x `elem` dates
actP = \ (x,y) -> elem (x,y) act
releaseP = \ (x,y) -> elem (x,y) release
directP = \ (x,y) -> elem (x,y) direct
playP = \ (x,y,z) -> elem (x,y,z) play

-- Give me the actors that also are directors
ad = [x | x <- actors, directorP x]

-- Give me all actors that also are directors, together with the ﬁlms in which they were acting
p2 = [ (x, y) | (x,y) <- act, directorP x]

q10 = [ x | ("Woody Allen",x) <- direct ] /= []

q11 = [ (x,y) | (x,y) <- act, or [x == "Robert De Niro",x=="Kevin Spacey"] ]

q12 =
    [ (c, d, i) | (c,d) <- release, (i, g) <-
        [ (x,y) | (a,b) <- act, (x,y) <- direct, a=="Quentin Tarantino",x=="Quentin Tarantino" ],
        c == g, d == "1994"
    ]

q13 = nub [ (name, year, movie) | (name, movie) <- act,
    (director,movie) <- direct,
    (movie, year) <-release, name=="Quentin Tarantino", director=="Quentin Tarantino", year=="1994"]

q14 =  [ (name, movie) | (name, movie) <- act,
        (director,movie) <- direct,
        name=="Quentin Tarantino", director=="Quentin Tarantino"]

q8 = [ x | (x,y) <- release, y > "1997", not (actP ("William Hurt",x)) ]

splitList :: Eq a => [a] -> [([a], [a])]
splitList []  = error "less than 2 elements"
splitList [x] = error "less than 2 elements"
splitList xs  = split' xs 1 (length xs)

split' :: [a] -> Int -> Int ->  [([a], [a])]
split' xs firstNth len |  firstNth < len = (take firstNth xs, drop firstNth xs) : split' xs (firstNth+1) len
                       | otherwise = []


naturals = [0..]
evens1 = [n | n <- naturals , even n]
odd1 = [n | n <- naturals , odd n]
even2 = [ 2*n | n <- naturals]
small_squares1 = [n^2 | n <- enumFromTo 0 999]

run:: Integer -> [Integer]
run n
  | n < 1 = error "argument not positive"
  | n == 1 = [1]
  | even n = n : run (div n 2)
  | odd n = n : run (3*n + 1)
