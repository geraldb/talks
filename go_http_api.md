title: Building Web Services (HTTP APIs) with Go


%css

pre {
  padding: 4px 4px 4px 4px;
  border-top: #bbb 1px solid;
  border-bottom: #bbb 1px solid;
  background: #f3f3f3;
}

%end



# What's football.db? What's sportkit?


Free open public domain football datasets (leagues, teams, matches, etc.) -> [`github.com/openfootball`](https://github.com/openfootball)

~~~
Quarter-finals - 1st Leg

[Tue Apr/1]
  20.45   FC Barcelona        1-1  Atlético Madrid     @ Camp Nou, Barcelona
            [Neymar 71'; Diego 56']
  20.45   Manchester United   1-1  Bayern München      @ Old Trafford, Manchester
            [Vidić 58'; Schweinsteiger 67']
[Wed Apr/2]
  20.45   Real Madrid         3-0  Borussia Dortmund   @ Santiago Bernabéu, Madrid
            [Bale 3' Isco 27' Ronaldo 57']
  20.45   Paris Saint-Germain 3-1  Chelsea FC          @ Parc des Princes, Paris
            [Lavezzi 4' Luiz 61' (o.g.) Pastore 90+3'; Hazard 27' (pen.)]
~~~

or in JSON via HTTP `GET /events`

~~~
[
  {"key":"en.2014/15","title":"English Premier League 2014/15"},
  {"key":"de.2014/15","title":"Deutsche Bundesliga 2014/15"},
  {"key":"at.2014/15","title":"Österr. Bundesliga 2014/15"},
  ...
]
~~~

Includes sportkit - starter kit for HTTP JSON API - Go Edition -> [`github.com/sportkit`](https://github.com/sportkit)



# Go Structs, Structs, Structs

`database_structs.go`:

~~~
type Event struct {
  Key   string
  Title string
}
type Team struct {
  Key   string
  Title string
  Code  string
}
~~~

NOTE: Go Ids (Names) starting with capital letter (upper case) are public (by definition).

~~~
type JsEvent struct {
  Key   string `json:"key"`
  Title string `json:"title"`
}

type JsTeam struct {
  Key   string `json:"key"`
  Title string `json:"title"`
  Code  string `json:"code"`
}
~~~


# Database Queries

`database_finders.go`:

~~~
query :=
 `SELECT
    e.[key] AS event_key,
    l.title || ' ' || s.title AS event_name
  FROM events e
   INNER JOIN seasons s ON s.id = e.season_id
   INNER JOIN leagues l ON l.id = e.league_id`
~~~

NOTE: Go "raw" strings with backquotes (\`) let you use multi-line string literals.


~~~
import (
  "database/sql"
  _ "github.com/mattn/go-sqlite3"
  "log"
)
~~~

NOTE: You can use Go packages "hosted" on Github. Works "out-of-the-box".
Use `go get github.com/mattn/go-sqlite3` to install.


~~~
func FetchEvents() []Event {

  db = sql.Open( "sqlite3", "./football.db" )
  if err != nil {
    log.Fatal(err)
  }

  rows, err := db.Query( query )
  if err != nil {
    log.Fatal(err)
  }
  defer rows.Close()

  events := []Event{}

  for rows.Next() {
    var r Event
    err = rows.Scan( &r.Key, &r.Title )
    if err != nil {
      log.Fatal(err)
    }
    events = append( events, r ) // add new row
  }
  return events
}
~~~

NOTE: Go has NO exceptions; function (almost) always returns err codes.

Why no exceptions?  Go's headline feature is concurrency (w/ Go (co)routines and channels) -
exceptions do NOT really work in an (asynchronous) concurrent world.



# Generate (Encode) JSON

~~~
import (
  "encoding/json"
)
~~~

~~~
func GetEvents() (interface{},error) {
  // step 1: fetch records
  events := FetchEvents()
  log.Println( events )

  // step 2: map to json structs for serialization/marshalling
  type JsEvent struct {
    Key   string `json:"key"`
    Title string `json:"title"`
  }
  data := []*JsEvent{}

  for _,event := range events {
    data = append( data, &JsEvent {
                            Key:   event.Key,
                            Title: event.Title, } )
  }

  return json.Marshal( data )
}
~~~

NOTE: `interface{}` is the Go version of the any object type (`void*`).


# Serve via HTTP

~~~
import (
  "net/http"
  "net/url"
  "fmt"
  "strings"
)
~~~

~~~
func main() {
  addr := ":9292" 
  fmt.Println( "Starting web server listening on " + addr + "..." )
  fmt.Println( "Use Ctrl-C to stop" )
  
  http.HandleFunc( "/", handleFunc )
  http.ListenAndServe( addr, nil )
  fmt.Println( "Bye" )
}
~~~

~~~
func handleFunc( w http.ResponseWriter, r *http.Request ) {
  log.Println( "handle url.path: " + r.URL.Path )

  eventsRoute, _ := regexp.Compile( "^/events$" )

  if eventsRoute.MatchString( r.URL.Path ) {
    b, err := GetEvents()
    if err != nil {
      fmt.Fprintf( w, err.Error() )
    }
    else {
      fmt.Fprintf( w, string(b) )
    }
  }
  else {
    fmt.Fprintf( w, "No route match found for " + r.URL.Path )
  }
}
~~~


# Why Go? - Faster, Faster, Faster

Use what works for you.

Kind of a "better" more "modern" C.

Code gets compiled (to machine-code ahead-of-time) and linked
to let you build (small-ish) zero-dependency
all-in-one binary (file) programs.

No virtual machine or byte code runtime or just-in-time compiler machinery needed;
includes garbage collector.

## Go's Headline Features

- Fast Builds - Imports, Package Structure, etc.
- Concurreny - Go (co)-routines, channels
    e.g. "agent model" - no threads, no shared state/variables - message passing w/ agents, etc.



# That's it. Thanks.

### Questions? Comments?

### Go Links

- [Go SQL Tutorial (Series)](http://go-database-sql.org)
- [sqlx package](https://github.com/jmoiron/sqlx) - general extensions to database/sql e.g. includes `rows.StructScan()`
- [How I start (Go Edition](https://howistart.org/posts/go/1) - builds a small HTTP JSON API wrapper/proxy for weather service
- [Learn Go in 5 Minutes](http://learnxinyminutes.com/docs/go) - single-page "cheatsheet" - "real-world" code w/ comments

### Free Online Go Books

- [The Go Bootcamp Book](http://www.golangbootcamp.com/book) by Matt Aimonetti
- [An Introduction to Programming in Go](http://www.golang-book.com) by Caleb Doxsey

