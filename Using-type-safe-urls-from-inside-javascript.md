{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Yesod
import           Yesod.Form.Jquery

-- This recipe will demonstrate how to return JSON values that include rendered
-- links. This is the recommended approach: URLs should be generated on the
-- server using type-safe URLs, instead of using string concatenation inside
-- Javascript.
--
-- We'll be keeping a static list of people. In a real application, we would
-- use Persistent to store people in the database.
newtype PersonId = PersonId Int
    deriving (PathPiece, Show, Read, Eq)
data Person = Person String Int

people :: [(PersonId, Person)]
people =
    [ (PersonId 1, Person "Alice" 25)
    , (PersonId 2, Person "Bob" 30)
    , (PersonId 3, Person "Charlie" 35)
    ]

data App = App

-- Typical routes: a homepage which will use AJAX to grab the JSON list of
-- people from the PeopleR route, and then a PersonR route for each individual
-- person.
mkYesod "App" [parseRoutes|
/ HomeR GET
/people PeopleR GET
/people/#PersonId PersonR GET
|]

instance Yesod App
instance YesodJquery App

-- This is the heart of the program. Notice how we pass in the rendering
-- function to toPair below, so it can be used to turn `PersonR pid` into a
-- textual URL.
getPeopleR :: Handler RepJson
getPeopleR = do
    render <- getUrlRender
    jsonToRepJson $ map (toPair render) people
  where
    toPair render (pid, Person name _age) = object
        [ "url" .= render (PersonR pid)
        , "name" .= name
        ]

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    app <- lift getYesod
    addScriptEither $ urlJqueryJs app
    -- Create the element where the list of people will be added.
    [whamlet|<ul #people>|]
    toWidget [julius|
        $(function(){
            // We can use typical URL interpolation of PeopleR inside Julius.
            $.getJSON("@{PeopleR}", function(people){
                var i, person, li, a, ul;
                ul = $("#people");
                for (i = 0; i < people.length; i++) {
                    person = people[i];
                    a = $("<a>");
                    a.text(person.name);
                    a.attr("href", person.url);
                    li = $("<li>");
                    li.append(a);
                    ul.append(li);
                }
            });
        });
    |]

getPersonR :: PersonId -> Handler RepHtml
getPersonR pid = do
    Person name age <- maybe notFound return $ lookup pid people
    defaultLayout [whamlet|
        <h1>#{name}
        <p>#{name} is #{age} years old.
    |]

main :: IO ()
main = warpDebug 3000 App