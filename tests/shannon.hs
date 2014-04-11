import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Word
import System.Environment (getArgs)

readBinaryFile :: String -> IO [Word8]
readBinaryFile file = fmap BS.unpack $ BS.readFile file

readBinaryStdin:: IO [Word8]
readBinaryStdin = fmap BS.unpack BS.getContents

regroup :: [Word8] -> [Word16]
regroup (x:y:ys) = bytes x y : regroup ys
 where
    bytes low hi = fromIntegral low + 256 * fromIntegral hi
regroup xs = map fromIntegral xs
 
main :: IO ()
main = do
    args <- getArgs
    mapM_ (\arg -> do
        x <- readBinaryFile arg
        putStr $ "Shannon entropy of " ++ arg ++ " is "
        print $ entropy x) args
    putStrLn "Thankyou."
 
entropy s = 
 sum . map lg' . fq' . map (fromIntegral.length) . group . sort $ s
  where lg' c = (c * ) . logBase 2 $ 1.0 / c
        fq' c = map (\x -> x / sum c) c 
