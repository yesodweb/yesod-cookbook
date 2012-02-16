{-# LANGUAGE OverloadedStrings #-}
import Debug.Trace
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import qualified Data.Map as Map
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Lazy.Builder
import Data.Monoid
import qualified Data.Attoparsec.Text as A
import Control.Applicative (many, (<|>))

type Contents = [Content]

type Title = Text

data Content = Text Text | Link
    { linkPath :: F.FilePath
    , linkText :: Text
    }
    deriving Show

main :: IO ()
main = do
    files <- runResourceT $ traverse False "old" $$ CL.filter (flip F.hasExtension "markdown") =$ CL.mapM load =$ CL.consume
    let titleMap' = Map.unionsWith (++) $ map (\(t, _, c) -> Map.singleton t [c]) files
    titleMap <- fmap Map.fromList $ forM (Map.toList titleMap') (\(k, sv) -> do
        case sv of
            [v] -> return (k, v)
            _ -> error $ "Duplicated title: " ++ show k)

    let fileMap = Map.fromList $ map (\(t, fp, _) -> (fp, t)) files

    mapM_ (output fileMap) $ Map.toList titleMap

--output :: (Title, Contents) -> IO ()
output fileMap (title, cs) =
    TLIO.writeFile (F.encodeString fp) $ toLazyText $ mconcat $ map build cs
  where
    fp = F.fromText title' F.<.> "md"
    title' = T.map fix title
    fix ' ' = '-'
    fix '/' = '-'
    fix c = c

    build (Text t) = fromText t
    build (Link fp t) = mconcat
        [ "[["
        , fromText t
        , "|"
        , fromText link
        , "]]"
        ]
      where
        link =
            case Map.lookup fp fileMap of
                Nothing -> "missing:" `T.append` either id id (F.toText fp)
                Just x -> x

load :: F.FilePath -> IO (Title, F.FilePath, Contents)
load fp = do
    full <- fmap (T.filter (/= '\r')) $ TIO.readFile $ F.encodeString fp
    let (mtitle, body) =
            case T.stripPrefix "Title: " full of
                Nothing -> (Nothing, full)
                Just rest ->
                    let (x, y) = T.break (== '\n') rest
                     in (Just x, T.drop 1 y)
    niceDir <-
        case T.stripPrefix "./old/" $ either id id $ F.toText $ F.directory fp of
            Nothing -> error $ "huh? " ++ show (F.toText (F.directory fp))
            Just x
                | not (T.null x) && T.last x == '/' -> return $ T.init x
                | otherwise -> return x
    let title =
            case mtitle of
                Nothing -> niceDir
                Just title -> fst (T.breakOnEnd "/" niceDir) `T.append` title

    contents <- either error return $ A.parseOnly parseContents body
    return (title, fp, contents)

parseContents = many parseContent

parseContent = A.try parseLink <|>
               fmap Text (A.string "[") <|>
               fmap Text (A.takeWhile1 (/= '['))

parseLink = do
    A.char '['
    linktext <- A.takeWhile (/= ']')
    A.string "]("
    href <- A.takeWhile (/= ')')
    A.char ')'
    href' <- fmap (T.replace "%20" " ") $ stripWiki href
    let fp = "old" F.</> F.fromText href' F.</> "index.markdown"
    return $ Link fp linktext

stripWiki t
    | Just x <- T.stripPrefix "http://www.yesodweb.com/wiki/" t = return x
    | Just x <- T.stripPrefix "/wiki/" t = return x
    | otherwise = {-traceShow t $ -}fail ""
