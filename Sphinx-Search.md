Here is some example sphinx search code that works on Yesod 1.1 and Sphinx 0.6
https://gist.github.com/4100838

Below is a port of the code in the [Sphinx  Search case study blog post](http://www.yesodweb.com/blog/2011/09/case-study-sphinx) by Michael Snoyman. It has been updated to work with Yesod 0.10 with Conduits 0.2, Persistent 0.8

The latest version of sphinx uses the Text data type instead of String/ByteString: to make this work with the latest just remove all the  Text conversions.

~~~ {.haskell}
  build-depends: base
               , yesod
               , persistent-sqlite
               , persistent
               , sphinx
               , xml-conduit
               , http-types
               , wai 
               , xml-types
               , conduit
               , blaze-markup
               , text
               , bytestring
~~~

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
    QuasiQuotes, MultiParamTypeClasses, GADTs, FlexibleContexts
  #-}
import Yesod
import Data.Text (Text, unpack)
import Control.Applicative ((<$>), (<*>))
import Database.Persist.Sqlite
import Database.Persist.Store (PersistValue (PersistInt64))
import qualified Text.Search.Sphinx as S
import qualified Text.Search.Sphinx.Types as ST
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Control.Monad (forM)
import qualified Data.Text as T
import Text.Blaze (preEscapedLazyText)
import Data.Conduit (Source, Flush (Chunk), ($=), transSource)
import qualified Data.Conduit.List as CL
import qualified Data.XML.Types as X
import Network.Wai (Response (ResponseSource))
import Network.HTTP.Types (status200)
import Text.XML.Stream.Render (renderBuilder, def)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Doc
    title Text
    content Textarea
|]

data Searcher = Searcher ConnectionPool

mkYesod "Searcher" [parseRoutes|
/ RootR GET
/doc/#DocId DocR GET
/add-doc AddDocR POST
/search SearchR GET
/search/xmlpipe XmlpipeR GET
|]

instance Yesod Searcher where
    approot _ = ""

instance YesodPersist Searcher where
    type YesodPersistBackend Searcher = SqlPersist

    runDB action = do
        Searcher pool <- getYesod
        runSqlPool action pool

instance RenderMessage Searcher FormMessage where
    renderMessage _ _ = defaultFormMessage

addDocForm :: Html -> MForm Searcher Searcher (FormResult Doc, Widget)
addDocForm = renderTable $ Doc
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing

searchForm :: Html -> MForm Searcher Searcher (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) "Query" Nothing

getRootR :: Handler RepHtml
getRootR = do
    docCount <- runDB $ count ([] :: [Filter Doc])
    ((_, docWidget), _) <- runFormPost addDocForm
    ((_, searchWidget), _) <- runFormGet searchForm
    let docs = if docCount == 1
                then "There is currently 1 document."
                else "There are currently " ++ show docCount ++ " documents."
    defaultLayout [whamlet|
<p>Welcome to the search application. #{docs}
<form method=post action=@{AddDocR}>
    <table>
        ^{docWidget}
        <tr>
            <td colspan=3>
                <input type=submit value="Add document">
<form method=get action=@{SearchR}>
    ^{searchWidget}
    <input type=submit value=Search>
|]

postAddDocR :: Handler RepHtml
postAddDocR = do
    ((res, docWidget), _) <- runFormPost addDocForm
    case res of
        FormSuccess doc -> do
            docid <- runDB $ insert doc
            setMessage "Document added"
            redirect $ DocR docid
        _ -> defaultLayout [whamlet|
<form method=post action=@{AddDocR}>
    <table>
        ^{docWidget}
        <tr>
            <td colspan=3>
                <input type=submit value="Add document">
|]

getDocR :: DocId -> Handler RepHtml
getDocR docid = do
    doc <- runDB $ get404 docid
    defaultLayout $
        [whamlet|
<h1>#{docTitle doc}
<div .content>#{docContent doc}
|]

data Result = Result
    { resultId :: DocId
    , resultTitle :: Text
    , resultExcerpt :: Html
    }

getResult :: DocId -> Doc -> Text -> IO Result
getResult docid doc qstring = do
    excerpt' <- S.buildExcerpts
        excerptConfig
        [T.unpack $ escape $ docContent doc]
        "searcher"
        (unpack qstring)
    let excerpt =
            case excerpt' of
                ST.Ok bss -> preEscapedLazyText $ decodeUtf8With ignore $ L.concat bss
                _ -> return ()
    return Result
        { resultId = docid
        , resultTitle = docTitle doc
        , resultExcerpt = excerpt
        }
  where
    excerptConfig = E.altConfig { E.port = 9312 }

escape :: Textarea -> Text
escape =
    T.concatMap escapeChar . unTextarea
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = T.singleton c

getResults :: Text -> Handler [Result]
getResults qstring = do
    sphinxRes' <- liftIO $ S.query config "searcher" (unpack qstring)
    case sphinxRes' of
        ST.Ok sphinxRes -> do
            let docids = map (Key . PersistInt64 . ST.documentId) $ ST.matches sphinxRes
            fmap catMaybes $ runDB $ forM docids $ \docid -> do
                mdoc <- get docid
                case mdoc of
                    Nothing -> return Nothing
                    Just doc -> liftIO $ Just <$> getResult docid doc qstring
        _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
        { S.port = 9312
        , S.mode = ST.Any
        }

getSearchR :: Handler RepHtml
getSearchR = do
    ((formRes, searchWidget), _) <- runFormGet searchForm
    searchResults <-
        case formRes of
            FormSuccess qstring -> getResults qstring
            _ -> return []
    defaultLayout $ do
        addLucius [lucius|
.excerpt {
    color: green; font-style: italic
}
.match {
    background-color: yellow;
}
|]
        [whamlet|
<form method=get action=@{SearchR}>
    ^{searchWidget}
    <input type=submit value=Search>
$if not $ null searchResults
    <h1>Results
    $forall result <- searchResults
        <div .result>
            <a href=@{DocR $ resultId result}>#{resultTitle result}
            <div .excerpt>#{resultExcerpt result}
|]

getXmlpipeR :: Handler RepXml
getXmlpipeR = do
    Searcher pool <- getYesod
    sendWaiResponse $ ResponseSource status200 [("Content-Type", "text/xml")] $
        transSource (flip runSqlPool pool) fullDocSource 
            $= renderBuilder def
            $= CL.map Chunk

entityToEvents :: Entity Doc -> [X.Event]
entityToEvents (Entity docid doc) =
    [ X.EventBeginElement document [("id", [X.ContentText $ toPathPiece docid])]
    , X.EventBeginElement content []
    , X.EventContent $ X.ContentText $ unTextarea $ docContent doc
    , X.EventEndElement content
    , X.EventEndElement document
    ]

fullDocSource :: Source (SqlPersist IO) X.Event
fullDocSource =
    CL.sourceList startEvents `mappend`
    docSource `mappend`
    CL.sourceList endEvents

docSource :: Source (SqlPersist IO) X.Event
docSource = selectSource [] [] $= CL.concatMap entityToEvents

toName :: Text -> X.Name
toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset, schema, field, document, content :: X.Name
docset = toName "docset"
schema = toName "schema"
field = toName "field"
document = toName "document"
content = "content" -- no prefix

startEvents, endEvents :: [X.Event]
startEvents =
    [ X.EventBeginDocument
    , X.EventBeginElement docset []
    , X.EventBeginElement schema []
    , X.EventBeginElement field [("name", [X.ContentText "content"])]
    , X.EventEndElement field
    , X.EventEndElement schema
    ]

endEvents =
    [ X.EventEndElement docset
    ]

main :: IO ()
main = withSqlitePool "searcher.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ Searcher pool
~~~