title: Haskell, Haskell, Haskell


# SQLite Database Access

- HSQL
- Persistent

## HSQL Example

~~~
λ> :module Database.HDBC Database.HDBC.Sqlite3
λ> conn <- connectSqlite3 "test.db"
Loading package HDBC-1.1.5 ... linking ... done.
Loading package HDBC-sqlite3-1.1.4.0 ... linking ... done.
λ> run conn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []
0
λ> run conn "INSERT INTO test (id) VALUES (0)" []
1
λ> commit conn
λ> disconnect conn
~~~



# Persistent Entities (Schema and Migrations)

~~~
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
  Country
    name String
    deriving Show
  Client
    firstName String
    lastName  String
    address   String
    country   CountryId
    age       Int
    deriving Show
...
~~~


# Persistent Example

~~~
import Database.Persist.Sqlite

conn = runSqlite "test.db" $ do
  austria  <- insert $ Country "Austria"
  _client1 <- insert $ Client "Anton" "Dreher" "Brauplatz 1" austria 25
  return ()
~~~


# Persistent Queries

~~~
getAdultsOfSpainAndUS = do -- returns [Entity Client]
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity usId _) <- getBy $ UniqueCountryName "United States of America"
  selectList ( [ ClientCountry ==. spId, ClientAge >=. Just 18 ]
     ||. [ ClientCountry ==. usId, ClientAge >=. Just 21 ] )
     []
~~~


# SQL Queries with Esqueleto

~~~
getPeopleOver25FromSpainOrGermany = -- returns [Entity Client]
  select $
  from $ \(client, country) -> do
  where_ ( client ^. ClientAge >. just (val 25)
    &&. country ^. CountryName `in_` valList [ "Spain", "Germany" ]
    &&. client ^. ClientCountry ==. country ^. CountryId )
  orderBy [ asc (client ^. ClientLastName), asc (client ^. ClientFirstName) ]
  return client
~~~


# SQL Queries with Esqueleto (Join)

~~~
getPeopleOver25FromSpainOrGermanyJoin = -- returns [Entity Client]
  select $
  from $ \(client `InnerJoin` country) -> do
  on (client ^. ClientCountry ==. country ^. CountryId)
  where_ ( client ^. ClientAge >. just (val 25)
    &&. country ^. CountryName `in_` valList [ "Spain", "Germany" ])
  orderBy [ asc (client ^. ClientLastName), asc (client ^. ClientFirstName) ]
  return client
~~~



# Web Frameworks / Libraries

- Yesod   ("Full stack")
- Happstack
- Snap
- Scotty  (Minimal; Sinatra-Inspired)
- Spock
- and many more

## Scotty Example

~~~
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
~~~



# Scotty Example - URL Shortener

~~~
main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware static

    m <- liftIO $ newMVar (0::Int,M.empty :: M.Map Int T.Text)

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! action "/shorten" $ do
                        H.input H.! type_ "text" H.! name "url"
                        H.input H.! type_ "submit"

    post "/shorten" $ do
        url <- param "url"
        liftIO $ modifyMVar_ m $ \(i,db) -> return (i+1, M.insert i (T.pack url) db)
        redirect "/list"

    -- We have to be careful here, because this route can match pretty much anything.
    -- Thankfully, the type system knows that 'hash' must be an Int, so this route
    -- only matches if 'read' can successfully parse the hash capture as an Int.
    -- Otherwise, the pattern match will fail and Scotty will continue matching
    -- subsequent routes.
    get "/:hash" $ do
        hash <- param "hash"
        (_,db) <- liftIO $ readMVar m
        case M.lookup hash db of
            Nothing -> raise $ mconcat ["URL hash #", T.pack $ show $ hash, " not found in database!"]
            Just url -> redirect url

    -- We put /list down here to show that it will not match the '/:hash' route above.
    get "/list" $ do
        (_,db) <- liftIO $ readMVar m
        json $ M.toList db
~~~



# Why Haskell?

Learn something new - state-of-the-art in language design;
many new concepts (compared to classics e.g. C, Perl, Ruby, PHP; etc.):

- Great for learing a "pure" "state-of-the-art" functional programming (FP) language
- Great for learning the "state-of-the-art" for a strongly-typed language
    - Type classes, type families (Typeclassopedia)
    - Algebraic Data types  (ADT)
    - Category theory (Yoneda Lemma, Kleisli Fish, etc.)
    - etc.

- Many new concepts
    - Programming without Null/Nil - Maybe, Just, Nothing, ()
    - "Pure" Functions without Side-Effects and I/O Functions with Side-Effects
    - Monads, Monoids, Applicatives, Functors, Prisms, Lenses, etc.


# Beautiful "Minimalistic" Syntax

~~~
a b c d
~~~

Trivia Quiz -  Only one interpretation possible in Haskell



# Beautiful "Minimalistic" Syntax

Function a with three parameters, that is, b, c and d.

~~~
a b c d          f . g x y    f g( x y )  

a( b, c, d )     f( g(x,y))       -- Classic-style

(a b c d)       (f (g x y))       -- Lisp-style
~~~



# Language Example - Inspired by Haskell  - Elm

## Elm

Why: Runs in your Browser -- (Yet Another) Alternative Generate-to-JavaScript Language

### Who?

Sponsored by Prezi (Budapest, Hungary); Headed by Evan Czaplicki

Example:

~~~
profile : User -> Html
profile user =
    div [] [
        img [ src user.picture ] [],
        text user.name

font : List CssProperty
font = [
    prop "font-family" "futura, sans-serif",
    prop "color" "rgb(42, 42, 42)",
    prop "font-size" "2em"
  ]

background : List CssProperty
background = [
    prop "background-color" "rgb(245, 245, 245)"
  ]

profiles : List User -> Html
profiles users =
    div [ style (font ++ background) ] (map profile users)
~~~




# Language Example - Inspired by Haskell  - Idris

## Idris

Why: Faster, Easier -- Dependent Datatypes, Laziness Optional, and More

A standard example of a dependent type is the type of "lists with length",
conventionally called vectors in the dependent type literature.

Example:

~~~
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil ys = ys
(++) (x :: xs) ys = x :: xs ++ xs   -- broken
~~~



# Other languages inspired by Haskell

- F# .NET (Microsoft)
- Swift (Apple)
- Hack (Facebook)
- Many more



# Haskell Programs - Haskell Goodies - Haskell in the Real World

Pandoc - "Universal" Document Generator; Headed by John MacFarlane (University of Berkeley)

Example:  Generate a book (EPUB) using Markdown

mybook.txt:

~~~
Title: My Book
Author: Ruby Rubacuori

This is my book.

# Chapter One

Chapter one is over.

# Chapter Two

Chapter two has just begun.
~~~

To build your book issue:

~~~
$ pandoc mybook.txt -o mybook.epub
~~~



# Learning Haskell

## Books

- Learn You a Haskell for Great Good
- Real World Haskell
- Developing Web Applications with Haskell and Yesod, 2nd Edition
- Beginning Haskell
- Seven Languages in Seven Weeks (incl. Haskell)
- Seven More Languages in Seven Weeks (incl. Elm and Idris)

## Articles

- How I Start Series - Haskell - by Chris Allen
- School of  Haskell - Basics of Haskell Series -  by Bartosz Milewski (FP Complete)
- What I Wish I Knew When Learning Haskell - by Stephen Diehl

