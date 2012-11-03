{- |

Module      : $Header$
Description : Catenates files
Copyright   : (c) 2012 Joseph Abrahamson
License     : MIT
Maintainer  : Joseph Abrahamson <me@jspha.com>

Implements the Plan 9 'cat' command using lazy bytestrings and
proxies. Together, these should allow the implementation to be O(1)
space and to deterministically close files as soon as they have been
read. Unlike many implementations, this 'cat' handles missing file
arguments identically to Plan 9 'cat'.

-}
module Main where

import System.Environment
import System.IO
import Control.Proxy
import Control.Exception
import Data.Maybe
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = runSession $
       argsS >-> mapMD maybeReadFile
       >-> filterD isJust >-> mapD fromJust
       >-> mapMD L.putStrLn
       >-> discard

-- | A safe 'L.readFile' that returns 'Maybe' types instead of
-- throwing exceptions.
maybeReadFile :: FilePath -> IO (Maybe L.ByteString)
maybeReadFile path =
  do x <- try $ L.readFile path
     case x of
       -- We have to specify the type of 'e' here in order to force
       -- the exception handler to generalize over the entire
       -- existentially typed container, 'SomeException'.
       Left e  -> do hPutStrLn stderr ("cat: " ++ show (e :: SomeException))
                     return Nothing
       Right s -> return (Just s)

-- | Serves the commandline arguments
argsS :: () -> Server () String IO ()
argsS p = do files <- lift getArgs
             fromListS files p
