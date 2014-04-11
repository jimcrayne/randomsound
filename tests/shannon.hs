import Data.List
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Word
import Data.Bits
import Data.Char (ord)

readBinaryFile :: String -> IO [Word8]
readBinaryFile file = do
    contents <- BS.readFile file
    return $ map (fromIntegral . ord) $ BS.unpack contents
 
main = do
    x <- readBinaryFile "data.bin"
    putStr "Shannon entropy of data.bin is "
    print $ entropy x
    putStrLn "(Press enter to continue.)"
    y <- getChar
    putStrLn "Thankyou."
 
entropy s = 
 sum . map lg' . fq' . map (fromIntegral.length) . group . sort $ s
  where lg' c = (c * ) . logBase 2 $ 1.0 / c
        fq' c = map (\x -> x / (sum c)) c 
