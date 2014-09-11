title: Building Web Services (HTTP APIs) with Ruby (and Sinatra)





# Agenda

- What's Sinatra?
- Let a Thousand Sinatra Clones Bloom
- Why Sinatra? Goodies
- Example Web Service (HTTP API) - Routes
- Sinatra in Action - `get '/beer/:key'`
- What's JSON? 
- What's JSONP?
- Serializers - From Ruby Objects to JavaScript Objects
- Appendix: Sinatra Styles - Classic or Modern (Modular)
- Appendix: Database Connection Management
- Appendix: Sinatra Books
- Appendix: What's Rack?


# new slides / ideas



# "Real-World" HTTP JSON APIs Examples

- GitHub API  ->  [developer.github.com/v3](https://developer.github.com/v3)
- Basecamp API -> [github.com/basecamp/api](https://github.com/basecamp/api)
- Heroku API  ->  [devcenter.heroku.com/categories/platform-api](https://devcenter.heroku.com/categories/platform-api)
- Travis CI API -> [docs.travis-ci.com/api](http://docs.travis-ci.com/api)




# CORS (Cross-origin resource sharing) - JSON-P v2.0

call-home restriction - same-origin restriction

supports cross-origin resource sharing (CORS)
so that requests can be sent from browsers using JavaScript served from any domain.



# rack-test



# json schema ???

- project site -> [json-schema.org](http://json-schema.org)

What's?

Describe your data structure n types (schema) in JSON. Example:


Why?

Pros:

- More tooling
  - auto-generate docu
  - auto-generate tests
  - auto-generate (test) client libraries
  - auto-generate validator (for required fields, types, etc.)

- More (re)use
  - (re)use "common" schemas



# HTTP JSON API - Rack v1.0 Version




# What's Rack?

# What's Metal (Rack v2.0)?

- Why update (break) Rack v1.0?
    - Streaming is the new black


# What's a Request Tree?

Sinatra - plain "flat" request tree 

~~~
get "/articles/:id" do |id|
  article = Article[id]
end

get "/articles/:article_id/comments/:id" do |article_id, id|
  article = Article[article_id]
  comment = article.comments[id]
end
~~~

vs

nested request tree

Cuba Version:
~~~
on get, "articles/:id" do |id|
  article = Article[id]

  on "comments/:id" do |id|
    comment = article.comments[id]
  end
end
~~~

Roda  version:




# Rum, Cuba, Roda 'n' Friends

More Microframeworks Alternatives

- Rum
   - First version by Rack inventor Christian Neune 

- Cuba  - <cuba.is>
   - uses the idea of rum (thus, the name Cuba) and adds a little more machinery

- Roda - <roda.jeremyevans.net>
   - uses the idea of cuba and adds yet more  machinery (e.g. better request tree, plugins, etc.)

   
- Where's this all headed?  Rail 6.0? - Just kidding.



-- lines of codes

Library   | Lines of Code (LOC)
----------|---------------------
Cuba         |        152
Sinatra      |       1_476
Rails (*)    |      13_181
(Almost) Sinatra |      7

(*) only ActionPack (Rails is over 40_000+ LOC)

Assumption less lines of code => faster code, more requests/secs - only use what you need

(Source: Cuba Slides)

- (Almost) Sinatra  ->  add link here



# HTTP JSON APIs - Go Version

Try another language, for example,

Why Go?

- code gets compiled to zero-dependency (small) machine-code binaries
- kind of a "better" more "modern" C
    - code gets compiled and linked (no virtual machine, or byte code runtime or just-in-time compiler machinery etc. needed)


# HTTP JOSN APIs - NoSQL Version

Try a NoSQL database and get JSON HTTP APIs (almost) for "free".





# HTTP JSON API Guidelines

Heroku API Design Guidelines

- 
- 

(Source: )






# - reuse/update slides from v1 talk

# What's Sinatra?

Simple (yet powerful and flexible) micro webframework.

~~~
require 'sinatra'

get '/' do
  'Hallo Wien!'
end
~~~

Sinatra itself [less than 2000 lines of code](https://github.com/sinatra/sinatra/blob/master/lib/sinatra/base.rb).  


Installation. Type in your terminal (shell):

~~~
$ gem install sinatra
~~~



# What's Sinatra? (Continued)

Example - `hallo.rb`:

~~~
require 'sinatra'

get '/' do
  'Hallo Wien!'
end
~~~

Run script (server):

~~~
$ ruby hallo.rb

>> Sinatra has taken the stage...
>> Listening on 0.0.0.0:4567, CTRL+C to stop
~~~

Open browser:

![](i/hallosinatra.png)



# Let a Thousand Sinatra Clones Bloom

Micro Frameworks Inspired by Sinatra


Express.js (in Server-Side JavaScript w/ Node.js):

~~~
var express = require( 'express' );
var app = express();

app.get( '/', function( req, res ) {
  res.send( 'Hallo Wien!' );
});

app.listen( 4567 );
~~~


Scotty (in Haskell):

~~~
import Web.Scotty

main :: IO ()
main = scotty 4567 $ do
    get "/" $ text "Hallo Wien!"
~~~

Dancer (Perl), Fitzgerald (PHP), Ratpack (Groovy),
Zappa (CoffeeScript), Mercury (Lua), Frank (F#), Nancy (C#/.NET),
Bogart (C), Flask (Python), and [many more](http://en.wikipedia.org/wiki/Sinatra_(software)#Frameworks_inspired_by_Sinatra).



# Why Sinatra?  Goodies

1) Single file scripts


2) Easy to package up into a gem. Example:

    $ gem install beerdb     # Yes, the beerdb includes a Sinatra app.


3) Lets you build command line tools. Example:

    $ beerdb serve           # Startup web service (HTTP API).


4) Lets you mount app inside app (including Rails). Example:

    mount BeerDb::Server, :at => '/api/v1'


# Example Web Service (HTTP API) - Routes

Lets build a beer and brewery API.

Get beer by key `/beer/:key`. Examples:

- `/beer/guinness`
- `/beer/murphysred`
- `/beer/brooklynlager`
- `/beer/ottakringerhelles`

Get brewery by key `/brewery/:key`. Examples:

- `/brewery/guinness`
- `/brewery/fullers`
- `/brewery/brooklyn`
- `/brewery/ottakringer`

Bonus:

Get random beer `/beer/rand` and random brewery `/brewery/rand`.



# Sinatra in Action - `get '/beer/:key'`

`beerdb/server.rb`:

~~~
get '/beer/:key' do |key|

  beer = Beer.find_by_key!( key )
  json_or_jsonp( beer.as_json )

end

get '/brewery/:key' do |key|

  brewery = Brewery.find_by_key!( key )
  json_or_jsonp( brewery.as_json )

end
~~~

That's it.


Bonus:

~~~
get '/beer/:key' do |key|

  if ['r', 'rnd', 'rand', 'random'].include?( key )
    beer = Beer.rnd.first
  else
    beer = Beer.find_by_key!( key )
  end

  json_or_jsonp( beer.as_json )
end

get '/brewery/:key' do |key|

  if ['r', 'rnd', 'rand', 'random'].include?( key )
    brewery = Brewery.rnd.first
  else
    brewery = Brewery.find_by_key!( key )
  end

  json_or_jsonp( brewery.as_json )
end
~~~



# What's JSON?

JSON = JavaScript Object Notation


Example - `GET /beer/ottakringerhelles`:

~~~
{
  key: "ottakringerhelles",
  title: "Ottakringer Helles",
  synonyms: "16er Blech|16er Hüs'n",
  abv: "5.2",
  og: "11.8",
  tags: [ "lager" ],
  brewery: {
    key: "ottakringer",
    title: "Ottakringer Brauerei"
  },
  country: {
   key: "at",
   title: "Austria"
  }
}
~~~


# What's JSONP?

JSONP = JSON with Padding.  Why?

Call Home Restriction. Cross-Domain Browser Requests Get Blocked. 

Hack: Wrap JSON into a JavaScript function/callback
e.g. `functionCallback( <json_data_here> )`
and serve as plain old JavaScript.


Example - `Content-Type: application/json`:

~~~
{
  key: "ottakringerhelles",
  title: "Ottakringer Helles",
  synonyms: "16er Blech|16er Hüs'n",
  abv: "5.2",
  ...
}
~~~

becomes `Content-Type: application/javascript`:

~~~
functionCallback(
  {
    key: "ottakringerhelles",
    title: "Ottakringer Helles",
    synonyms: "16er Blech|16er Hüs'n",
    abv: "5.2",
    ...
  }
);
~~~

Bonus: Little Sinatra helper for JSON or JSONP response (depending on callback parameter).

~~~
def json_or_jsonp( json )
  callback = params.delete('callback')

  if callback
    content_type :js
    response = "#{callback}(#{json})"
  else
    content_type :json
    response = json
  end
end
~~~


# Serializers - From Ruby Objects (in Memory) to JavaScript Object (in Text) 

JSON built into Ruby 2.0 as a standard library. Example:

~~~
require 'json'

hash =
{
  key:   "ottakringerhelles",
  title: "Ottakringer Helles"
}
~~~~

### 1) `JSON.generate`

~~~
puts JSON.generate( hash )

>> {"key":"ottakringerhelles","title":"Ottakringer Helles"}
~~~

### 2) `#to_json`

~~~
puts hash.to_json

>>  {"key":"ottakringerhelles","title":"Ottakringer Helles"}
~~~



# Serializers - From Ruby Objects (in Memory) to JavaScript Object (in Text) Continued

Serializers for your Models. Example:

~~~
class BeerSerializer

  def initialize( beer )
    @beer = beer
  end

  attr_reader :beer

  def as_json
    data = { key:      beer.key,
             title:    beer.title,
             synonyms: beer.synonyms,
             abv:      beer.abv,
             ...
           }
    data.to_json
  end

end # class BeerSerializer
~~~

And add `as_json` to your Model. Example:

~~~
class Beer < ActiveRecord::Base

  def as_json_v2( opts={} )
    BeerSerializer.new( self ).as_json
  end

end # class Beer
~~~


# That's it. Thanks.


### Questions? Comments?


Learn more about Sinatra @ [`sinatrarb.com`](http://sinatrarb.com)

Learn more about the open beer 'n' brewery database (`beer.db`) @ [`github.com/openbeer`](https://github.com/openbeer)



# Appendix: Sinatra Styles - Classic or Modern (Modular)

~~~
require 'sinatra'

get '/' do
  'Hallo Wien!'
end
~~~

vs.

~~~
require 'sinatra/base'

class Server < Sinatra::Base

  get '/' do
    'Hallo Wien!'
  end

end
~~~


# Appendix: Tip - Database Connection Management


Sinatra will NOT auto-magically close your database connection
after every request. It's up to you.

1) Use the `ConnectionManagement` middleware. Example:

~~~
Server.use ActiveRecord::ConnectionAdapters::ConnectionManagement
~~~

2) Or do it yourself. Example:

~~~
Server.after do
  ActiveRecord::Base.connection.close
end    
~~~
   

# Appendix: Sinatra Books

![](i/sinatra_up_and_running.gif) Sinatra: Up and Running by Alan Harris, Konstantin Haase;
November 2011, O'Reilly, 122 Pages

![](i/jump_start_sinatra.gif)  Jump Start Sinatra by Darren Jones;
January 2013, SitePoint, 150 Pages



# Appendix: What's Rack?

Lets you mix 'n' match servers and apps.

Lets you stack apps inside apps inside apps inside apps inside apps.

Good News: A Sinatra app is a Rack app.

Learn more about Rack @ [`rack.github.io`](http://rack.github.io).

