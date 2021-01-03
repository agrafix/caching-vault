# caching-vault

A simple [vault](https://hackage.haskell.org/package/vault) style cache implementation based on [stm-containers](https://hackage.haskell.org/package/stm-containers).

## Example

``` haskell
import Data.Time
import qualified Data.Cache.Vault as C

main :: IO ()
main =
  do cache <- C.newCache
     let key :: C.Key String
         key = C.mintLabeledKey "foo"
     C.insert key Nothing "cached value" cache
     
     now <- getCurrentTime
     value <- C.lookup now key cache
     case value of
       Nothing -> putStrLn "Cache miss"
       Just val -> putStrLn ("Cache value is: " <> val)
```
