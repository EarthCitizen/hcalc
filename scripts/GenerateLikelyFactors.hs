import Data.List (nub, sort)
import Control.Monad (forM_)

inGroupsOf :: (Show a) => Int -> [a] -> [[a]]
inGroupsOf sz _ | sz <= 0 = []
inGroupsOf _ [] = []
inGroupsOf sz xs = do
    let (xsg, xsr) = splitAt sz xs
     in xsg : inGroupsOf sz xsr

printGroups :: (Show a) => [[a]] -> IO ()
printGroups [[]] = return ()
printGroups []   = return ()
printGroups (xs:xss) = do
    putChar '\t'
    forM_ xs (\x -> putStr ", " >> putStr (show x))
    putChar '\n'
    printGroups xss


primesUntil :: Integer -> [Integer]
primesUntil u = [ x | x <- [2..u], isPrime x]
    where isPrime x = (not . any ((0 ==) . (rem x))) [2..(x-1)]

main = do
    printGroups $ inGroupsOf 10 $ [1] ++ primesUntil 40000
