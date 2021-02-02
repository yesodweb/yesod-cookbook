```haskell
import Conduit (ResourceT, runResourceT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary as CB
import Network.HTTP.Conduit

main :: IO ()
main = do
  man <- newManager tlsManagerSettings
  req <- parseRequest "http://www.yesodweb.com/"
  runResourceT $ do
    res <- http req man
    sealConduitT (responseBody res) $$+- printProgress .| CB.sinkFile "yesodweb.html"

printProgress :: ConduitT BS.ByteString BS.ByteString (ResourceT IO) ()
printProgress = loop 0
  where
    loop len = await >>= maybe (return ()) (showConsumed len)
    showConsumed len bs = do
      let len' = len + BS.length bs
      liftIO $ putStrLn $ "bytes consumed " ++ show len'
      yield bs
      loop len'
```
