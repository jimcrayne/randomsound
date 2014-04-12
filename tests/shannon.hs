import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Word
import System.Environment (getArgs)
import System.Exit 

readBinaryFile :: String -> IO [Word8]
readBinaryFile file = fmap BS.unpack $ BS.readFile file

readBinaryStdin:: IO [Word8]
readBinaryStdin = fmap BS.unpack BS.getContents

regroup :: [Word8] -> [Word16]
regroup (x:y:ys) = bytes x y : regroup ys
 where
    bytes low hi = fromIntegral low + 256 * fromIntegral hi
regroup xs = map fromIntegral xs

diffs xs = zipWith (-) xs (drop 1 xs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Expected Shannon Entropy, assuming random 8 bit symbols, is 8."
    entropies <- fmap concat $ mapM (\arg -> do
        x <- readBinaryFile arg
        putStr $ "Shannon entropy of " ++ arg ++ " is "
        let entropOfArg = [ entropy x, entropy (diffs x) ]
        print $ entropOfArg
        return entropOfArg) args
    if all (>7.5) $ entropies
        then do
            putStrLn "Thankyou."
            exitSuccess
        else do
            putStrLn $ "Poor entropy :( entropies="++show entropies
            exitFailure
 
entropy s = 
 sum . map lg' . fq' . map (fromIntegral.length) . group . sort $ s
  where lg' c = (c * ) . logBase 2 $ 1.0 / c
        fq' c = map (\x -> x / sum c) c 
