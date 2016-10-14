import           Prime              as P
import           System.Environment

-- | 'main' runs the main program
main :: IO ()
main = if P.prime0 11 then putStr "yes" else putStr "no"

