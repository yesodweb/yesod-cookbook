```haskell
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import           Data.Conduit
import           Data.Conduit.Binary    as CB
import           Network.HTTP.Conduit

main :: IO ()
main = withManager $ \manager -> do
    req <- parseUrl "http://www.yesodweb.com/"
    res <- http req manager
    responseBody res $$+- printProgress =$ CB.sinkFile "yesodweb.html"

printProgress :: Conduit S.ByteString (ResourceT IO) S.ByteString
printProgress =
    loop 0
  where
    loop len = await >>= maybe (return ()) (\bs -> do
        let len' = len + S.length bs
        liftIO $ putStrLn $ "Bytes consumed: " ++ show len'
        yield bs
        loop len')
```