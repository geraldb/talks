title: (J)Ruby in Action - Web Apps & Services w/ Rails n Jetty



%css

pre {
  padding: 4px 4px 4px 4px;
  border-top: #bbb 1px solid;
  border-bottom: #bbb 1px solid;
  background: #f3f3f3;
}

%end




# Ice Breakers

* Friends of Ruby (Rails)? - Anyone?

* Friends of Functional Programming (Haskell, Yesod) - Anyone?

* Friends of Scala, Play! - Anyone?

* Friends of Server-Side JavaScript (Node.js) - Anyone?

* Friends of Dart - Anyone?



# Factories, Factories, Factories

Java Enterprise Architecture 

- Factories, Factories, Factories
- Over-engineering, Cult of Complexity
- The COBOL of the 21st Century (Java is Old technology, No Longer Hot Java or the New New Thing => Innovation Happens Elsewhere)
- There is only Java, Java, Java - 1,000,0000,000 % Java, The End of History, Java Rules the World Hybris



#  Why Ruby?

- Code Blocks (Lambda Expressions) - 20 years in Ruby! Coming to Java 8 in 2014
- Open Classes
- Mixins
- Everything is a Object
- Meta Programming (e.g. Ruby Code Creates Code at Runtime on Demand)
- List, Tree, Map Data Structures in Ruby
- Templates in Ruby
- Culture - Programmer Happiness (Productivity, Keep it Simple), Innovation

<!--- finally -->

- Open, Free World (Not a Product and Trademark of Oracle, Inc.)



# Keep it Simple - Web Apps Case Study

1. No Enterprise Java Application Server
    * Use Embedded Jetty Library to Run Container-Less 
2. No Enterprise Java Database Server
    * Use Embedded SQLite Library
3. No Enterprise Java Application Framework
    * Use Embedded Ruby Library
4. No Enterprise Java IDE
    * Use Command Line, Programmer's Editor



# Case Study: How to Install Container-Less Jetty Web Server


Step 1: Copy  `jetty-webapp-7.x.jar`  (~ 1 Meg)

Step 2: There is no step 2. 



# Think Different - What's Container-Less?

![](i/jetty-container-less.png)

Container-Less? Run your web application as a plain old Java process.

Why:

* Simpler Development
* Simpler Testing
* Simpler Packaging
* Simpler Deployment



# A Simple Web App using Jetty

The simplest possible Jetty server:

```
import org.eclipse.jetty.server.Server;
    
public class SimpleServer
{
  public static void main(String[] args) throws Exception
  {
    Server server = new Server(8080);
    server.start();
    server.join();
  }
}
```


# A Simple Web App using Jetty  (Cont.)


```
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.webapp.WebAppContext;

public class AppServer
{

  public static void main(String[] args) throws Exception
  {
    Server server = new Server(8080);
 
    WebAppContext webapp = new WebAppContext();
    webapp.setContextPath("/");
    webapp.setWar( "./links.war");
    server.setHandler(webapp);
 
    server.start();
    server.join();
  }
}
```

## Compile 


    javac -cp .;./jetty-webapp-7.x.jar AppServer.java



# Let's Build a Link Bookmarking and Voting Web App

* Post links
* Vote links up or down
* Sort links by the newest or hot

![](i/build-reddit-in-ruby.png)



#  Create a Web App using Ruby on Rails

## Create Web App Structure

    $ rails links

## Create SQLite Schema (`db/migrate/create_db.rb`)

```
create_table :links do |t|
  t.string  :title,  :null => false 
  t.string  :url,    :null => false
  t.integer :points, :default => 0
  t.timestamps   # note: macro adds created_at, updated_at
end
```

## Create SQLite DB

    $ rake db:setup     # note: rake is ruby make (simple build tool)



# Model / View / Controller (MVC)

## Model (`models/link.rb`)

```
class Link < ActiveRecord::Base

  # self.table_name = 'links'
 
  attr_accessor :score 

  def recalc_score
    time_elapsed = (Time.now - self.created_at) / 36000  # in hours
    self.score = ((self.points-1) / (time_elapsed+2)**1.8)
  end
 
  def self.hot
    self.all.each { |rec| rec.recalc_score }.sort { |l,r| l.score <=> r.score }.reverse
  end
  
end
```


# Model / View / Controller (MVC)

## Controller (`controllers/links_controller.rb`)

```
class LinksController < ApplicationController
  
  # GET /
  def index    
    @links = Link.order( 'created_at desc' ).all
  end

  # GET /hot
  def hot
    @links = Link.hot
    render :index
  end

  # POST /
  def create
    l = Link.new( params[:link] )
    l.save!
    redirect_to :back    
  end

  # PUT /:id/vote/:type
  def vote
    l = Link.find( params[:id] )
    l.points += params[:type].to_i
    l.save!
    redirect_to :back    
  end
  
end
```

# Model / View / Controller (MVC)

## View (`views/links/index.html.erb`)

```
<table id='links'>
<%% @links.each do |l| %>

<tr>
  <td class='points'>
    <%%= form_for l, :url => vote_link_path( l, :type => '1' ), :method => 'PUT' do |f| %>
      <%%= f.submit '+1' %>
    <%% end %>
      
    <%%= l.points %>

    <%%= form_for l, :url => vote_link_path( l, :type => '-1' ), :method => 'PUT' do |f| %>
      <%%= f.submit '-1' %>
    <%% end %>            
  </td>
  <td><span class='title'><%%= link_to l.title, l.url %></span>
      <span class='host'>(<%%= l.url_host %>)</span>
      <span class='created-at'>posted <%%= time_ago_in_words(l.created_at) %> ago</span>
  </td>
</tr>
  
<%% end %>
</table>

<div id='post-link'>

  <%%= form_for :link, :url => links_path() do |f| %>
    <%%= f.text_field :title, :placeholder => 'Title' %>
    <%%= f.text_field :url,   :placeholder => 'URL' %>
    <%%= f.submit 'Save Link' %>
  <%% end %>
  
</div>
```


# Zip Up and Run It


## Zip Up as Java Web Archive (links.war)

    $ rake war      # warble

## Run it

    $ java -cp .;./jetty-webapp-7.x.jar AppServer

## Surprise ??

=> One Plain Old Java Process - Embedded Jetty, Embedded SQLite, Embedded Ruby



# Case Study: How to Install Embedded Ruby 


Step 1: Copy `jruby-complete-1.7.x.jar`

Step 2: There is no step 2.


# Java Web Archive (`.war`) Directory Structure 

```
links.war

|_ images
|  | _<empty>
|_ stylesheets
|  |_ application.css
|_ javascripts
|  | _<empty>
|_ WEB-INF
   |_ app
   |  |_ controllers
   |  |  |_ links_controller.rb
   |  |_ models
   |  |  |_ link.rb
   |  |_ views
   |     |_ layouts
   |     |  |_ application.html.erb
   |     |_ links
   |        |_ index.html.erb
   |_ config
   |  |_ database.yml
   |  |_ routes.rb
   |_ db
   |  |_ links.sqlite3
   |_ lib
      |_ jruby-complete-1.7.x.jar
      |_ jruby-rack-1.x.jar
      |_ gems.jar
```



# What's missing in Java? What's wrong with Java?

Anyone?


# Simple Data Structures (Maps, Arrays)

Example:

```
{ customer:
  { id: '12345',
    first_name: 'Carlos',
    last_name:  'Lafata',
    address:
    [
      { typ:      'home',
        line1:    'Burgring 11',
        city:     'Wien',
        zip_code: '1010' },
      { typ:      'work',
        line1:    'Nestroyplatz 12',
        city:     'Wien',
        zip_code: '1020' }
    ] }
}
```

## Quiz:  Built into what modern languages?

`[ X ]` JavaScript

`[ X ]` Ruby

`[ X ]` Scala

`[  ]` ~~Java~~

`[  ]` ~~COBOL~~



# Simple Code Blocks (Higher Order Functions, Functions as First Class Types)

Example:

```
// Assign Function to Variable

var greet =  function() {  document.write( 'Welcome. Benvenuti. Willkommen.' ); }

// Call Function Stored in Variable

greet();

// Function as Function Parameter

function say( what )
{
  what();
  what();
  what();
}

// Pass Function Stored in Variable to Function

say( greet );
```

## Quiz: Built into what modern languages?

`[ X ]` JavaScript

`[ X ]` Ruby

`[ X ]` Scala

`[  ]` ~~Java~~

`[  ]` ~~COBOL~~


# Part II


Building Web Services (HTTP APIs) with Ruby (and Sinatra)

## Agenda

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


# Appendix: What's Rack?

Lets you mix 'n' match servers and apps.

Lets you stack apps inside apps inside apps inside apps inside apps.

Good News: A Sinatra app is a Rack app.

Learn more about Rack @ [`rack.github.io`](http://rack.github.io).
   

# Appendix: Sinatra Books

![](i/sinatra_up_and_running.gif) Sinatra: Up and Running by Alan Harris, Konstantin Haase;
November 2011, O'Reilly, 122 Pages

![](i/jump_start_sinatra.gif)  Jump Start Sinatra by Darren Jones;
January 2013, SitePoint, 150 Pages




# What's vienna.rb? Ruby User Group Vienna

Next meetup:  Fri, October 11th @ Sektor5 

Talks (*)

Jakob Sommerhuber - sponsor talk

Martin Schürrer - Erlang/OTP in production for highly-available, scalable systems

Markus Prinz - How to improve your code

Gerald Bauer - working with Sinatra

Kathrin Folkendt - 'Chapter one' (lightning talk on getting started with Rails, and building her first web app)

(*) preliminary program



# That's it. Thanks.

1) Don't put yourself in the Java ghetto (or Java rules the world hybris)

=> Learn new concepts or languages 

2) Web architecture is more than Java enterprise architecture

=> Learn HTML, JS, CSS, HTTP (REST), SQL/NoSQL, etc.


### Questions? Comments?


