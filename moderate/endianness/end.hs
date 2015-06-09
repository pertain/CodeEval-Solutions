{- end.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com (MODERATE)
 -
 - https://www.codeeval.com/open_challenges/15/
 -}

import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Storable as FS
import qualified Foreign.Ptr as FP
import Data.Word
import System.IO.Unsafe

{-# NOINLINE endianCheck #-}
endianCheck = unsafePerformIO $ FMA.alloca $ \p -> FS.poke p (0x01020304 :: Word32) >> FS.peek (FP.castPtr p :: FP.Ptr Word8)

endianness w
    | w == 4 = putStrLn "LittleEndian"
    | w == 1 = putStrLn "BigEndian"

main :: IO ()
main = endianness endianCheck

