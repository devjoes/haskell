module Joe where
import Data.Bits (xor)
import Data.Char (toLower, toUpper, isAlpha, isUpper, ord)
import Data.Set (fromList)

timesTable times to = [i * times | i <- [1..to]]

factoral :: (Integral a) => a -> a
factoral 0 = 1
factoral x = x * factoral (x-1)


rot13 :: String -> String

rot13 s = [ if isLetter c then reUpper c (encode c) else c | c <- s]
    where encode c = toEnum ((mod (fromEnum (toLower c) - 97 + 13) 26) + 97) :: Char
            reUpper o n = if isUpper o then toUpper n else n
            isLetter c = isAlpha c && fromEnum c < 123