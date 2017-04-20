{-# OPTIONS_GHC -Wall #-}
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.PreProcess

main :: IO ()
main = defaultMainWithHooks hooks
  { hookedPreProcessors = hscDropCpp11 $ hookedPreProcessors hooks
  }
  where
    hooks = simpleUserHooks

    -- gcc shows a warning if -std=c++11 is used when compiling a C file, but
    -- clang is more strict and considers that an error, so we need to filter it
    -- out.
    hscDropCpp11 pps = ("hsc", newHsc) : deleteKey "hsc" pps
      where
        newHsc binfo = fromMaybe ppHsc2hs (lookup "hsc" pps) binfo
          { ccOptions = filter (not . f) (ccOptions binfo)
          }
          where
            f flag = flag == "-std=c++11"

----------------------------------------

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey _ []            = []
deleteKey k (x@(a, _):xs) = if k == a
                            then     deleteKey k xs
                            else x : deleteKey k xs
