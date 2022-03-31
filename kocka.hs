-- Agoston Gergely Vince 521 agim1986
import System.IO
import Control.Monad
import Data.Char (ord)

forgat ca (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:xs)
    | ca =='U' = (m:n:c:d:e:f:g:h:k:i:l:i:x:w:o:p:a:b:s:t:u:v:r:q:xs) 
    | ca =='D' = (a:b:s:t:g:e:h:f:i:j:k:l:m:n:c:d:q:r:v:u:p:o:w:x:xs) 
    | ca =='R' = (a:f:c:h:e:v:g:x:i:b:k:d:o:m:p:n:q:r:s:t:u:j:w:l:xs) 
    | ca =='L' = (i:b:k:d:a:f:c:h:u:j:w:l:m:n:o:p:s:q:t:r:e:v:g:x:xs) 
    | ca =='F' = (c:a:d:b:o:m:g:h:i:j:t:r:k:n:l:p:q:e:s:f:u:v:w:x:xs) 
    | ca =='B' = (a:b:c:d:e:f:q:s:n:p:k:l:m:h:o:g:j:r:i:t:w:u:x:v:xs) --s:t:k:l

forgatasok [] allapot = allapot

forgatasok (x:ap:xs) allapot
    | x == '.' = forgatasok (ap:xs) allapot
    | ord ap == 39 = forgatasok xs (forgat x (forgat x(forgat x allapot)))  
    | ord ap == 50 = forgatasok xs (forgat x (forgat x allapot)) 
    | otherwise = forgatasok (ap:xs) (forgat x allapot) 
forgatasok [x] allapot = (forgat x allapot) 

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let xs = input_line :: String
    let allapot1 = "FFFFDDDDUUUURRRRLLLLBBBB"
    let allapot = forgatasok xs allapot1
    let up = take 2 allapot
    let dw = drop 2 (take 4 allapot)
    putStrLn up
    putStrLn dw
    --putStrLn allapot
    return ()