title:  football.db - Using Open Football Data in JavaScript

%css

pre {
  padding: 4px 4px 4px 4px;
  border-top: #bbb 1px solid;
  border-bottom: #bbb 1px solid;
  background: #f3f3f3;
}

%end

# `football.db` - Using Open Football Data in JavaScript

### Agenda

- What's `football.db`?
- Example: La Liga - `es/teams.txt`
- Example: Champions League - `club/europe/2012_13/cl_ii.txt`
- Leagues and Tournaments / National Teams
- Intn'l Club Tournaments (Champions Leagues & Friends)
- National Club Leagues & Cups
- How to use `football.db` in JavaScript
- How to use HTTP JSON API - `GET /event/euro.2012/teams`
- How to use HTTP JSON API II - `GET /event/euro.2012/rounds`
- How to use HTTP JSON API III - `GET /event/euro.2012/round/6`
- How to use HTTP JSON(P) API in JavaScript & jQuery
- All Together Now - Machday Widget Example -  `footballdb.widget.js`



# What's `football.db`?

A free open public domain football (soccer) database & schema
for use in any (programming) language

(e.g. uses plain text fixtures/data sets)

```
### Teams

barcelona, Barcelona|FC Barcelona|Fútbol Club Barcelona, BAR, city:barcelona
```

```
### Matches

2013-03-12 20:45  Barcelona  4-0  Milan
```


# Example: La Liga - `es/teams.txt`

```
################################
# Primera División de España - La Liga

barcelona, Barcelona|FC Barcelona|Fútbol Club Barcelona,  BAR, city:barcelona
espanyol,  Espanyol Barcelona|Real CD Espanyol,  ESP, city:barcelona

madrid,    Real Madrid|Real Madrid CF, RMD, city:madrid
atletico,  Atlético Madrid|Club Atlético de Madrid,   ATL, city:madrid
getafe,    Getafe|Getafe CF|Getafe Club de Fútbol,    GET, city:madrid 
rayo,      Rayo Vallecano|Rayo Vallecano de Madrid, RAY, city:madrid

realbetis, Real Betis Sevilla|Real Betis|Real Betis Balompié, BET, city:sevilla
sevilla,   Sevilla|Sevilla FC|Sevilla Fútbol Club, SEV, city:sevilla

valencia,  Valencia|FC Valencia|CF Valencia|Valencia CF, VAL, city:valencia
levante,   Levante|Levante UD|Levante Unión Deportiva, LEV, city:valencia

malaga,    Málaga|FC Málaga|Málaga CF|CF Málaga,   MAG, city:malaga
athletic,  Athletic Bilbao,              ATH,   city:bilbao
granada,   Granada|Granada CF|Granada Club de Fútbol, GRA, city:granada
osasuna,   Osasuna|CA Osasuna|Club Atlético Osasuna, OSA, city:pamplona
...
```

Source: [`es/teams.txt`](https://github.com/geraldb/football.db/blob/master/es/teams.txt)



# Example: Champions League - `club/europe/2012_13/cl_ii.txt`

```
####################################
# Champions League 2012/13 - Finalrunden

(8)  Achtelfinale Rückspiele  // Di./Mi., 5.+6./12.+13. Mär 2013

2013-03-05 20:45  Manchester United  1-2  Real Madrid
2013-03-05 20:45  Borussia Dortmund  3-0  Schachtar Donezk

2013-03-06 20:45  Paris Saint-Germain  1-1  Valencia
2013-03-06 20:45  Juventus             2-0  Celtic Glasgow

2013-03-12 20:45  Schalke 04  2-3  Galatasaray
2013-03-12 20:45  Barcelona  4-0  Milan

2013-03-13 20:45  Málaga  2-0  Porto
2013-03-13 20:45  Bayern München  0-2  Arsenal
...
```

Source: [`club/europe/2012_13/cl_ii.txt`](https://github.com/geraldb/football.db/blob/master/club/europe/2012_13/cl_ii.txt)



# Leagues and Tournaments / National Teams

### World

- FIFA World Cup 2010, 2014

```
###########
# Groups

Group A  |  South Africa    Mexico              Uruguay          France
Group B  |  Argentina       Nigeria             South Korea      Greece
Group C  |  England         United States       Algeria          Slovenia
Group D  |  Germany         Australia           Serbia           Ghana
Group E  |  Netherlands     Denmark             Japan            Cameroon
Group F  |  Italy           Paraguay            New Zealand      Slovakia
Group G  |  Brazil          North Korea         Côte d'Ivoire    Portugal
Group H  |  Spain           Switzerland         Honduras         Chile


################
# Group A

Matchday 1 / Group A

(1) Fr  2010-06-11 16:00    South Africa - Mexico     1-1
(2) Fr  2010-06-11 20:30    Uruguay - France   0-0

...
```

Source: [`world/2010/cup.txt`](https://github.com/geraldb/football.db/blob/master/world/2010/cup.txt)


# Leagues and Tournaments / National Teams (Cont.)

### World (Cont.)


- FIFA World Cup Quali 2014
- FIFA Confederations Cup 2009, 2013

### America (North/Central America and the Caribeans, South America)

- CONCACAF Copa Oro / Gold Cup 2011, 2013
- CONMEBOL Copa América 2011, 2015

### Europe

- UEFA European Football Championship (Euro) 2012, 2008



# Intn'l Club Tournaments (Champions Leagues & Friends)

### Amercia (North/Central America and the Caribeans, South America)

- CONCACAF Champions League 2011/12, 2012/13
- CONMEBOL Copa Sudamericana 2012
- CONMEBOL Copa Libertadores 2012, 2013

### Europe

- UEFA Champions League 2011/12, 2012/13
- UEFA Europa League 2011/12


# National Club Leagues & Cups

### Europe

- Österreichische Bundesliga 2011/12, 2012/13
- ÖFB Cup 2011/12, 2012/13

```
######################################################
# Österreichische Bundesliga 2012/13 -  Frühjahr

21. Runde  // Sa+So 16.+17. Feb 2013

2013-02-16 16:00  SV Ried  -  FC Wacker Innsbruck   3:0
2013-02-16 18:30  SV Mattersburg  -  Sturm Graz    0:0
2013-02-16 18:30  FC Admira Wacker  -  SC Wiener Neustadt  1:2
2013-02-16 18:30 => 2013-03-06 18:30   Wolfsberger AC  -  RB Salzburg   1:1

2013-02-17 16:00  Rapid Wien  -  Austria Wien    1:2


22. Runde // Sa+So 23.+24. Feb 2013

2013-02-23 16:00  FC Wacker Innsbruck  -  SV Mattersburg   2:0
2013-02-23 18:30  SC Wiener Neustadt  -  SV Ried  0:0
2013-02-23 18:30  Sturm Graz  -  Wolfsberger AC    1:3
2013-02-23 18:30  Austria Wien  -  FC Admira Wacker    4:0

2013-02-24 16:00  RB Salzburg  -  Rapid Wien     3:3

...
```

Source: [`at/2012_13/bl_ii.txt`](https://github.com/geraldb/football.db/blob/master/at/2012_13/bl_ii.txt)



# National Club Leagues & Cups (Cont.)

### Europe (Cont.)

- English Permier League 2012/13
- Deutsche Bundesliga 2012/13
- Romania Liga 1 2012/13

### North America

- México Primera División Apertura 2012, Clausura 2013

### South America

- Campeonato Brasileiro Série A

Anything missing? Add your leagues, teams, fixtures and more.




# How to use `football.db` in JavaScript

Use the `football.db` HTTP JSON(P) API - Examples

List all teams for an event (league+season) `/event/:key/teams`

- `/event/de.2012_13/teams`  | Deutsche 1. Bundesliga 2012/13  
- `/event/world.2010/teams`  | World Cup 2010                  

List all rounds for an event (league+season) `/event/:key/rounds`

- `/event/de.2012_13/rounds`  | Deutsche 1. Bundesliga 2012/13  
- `/event/world.2010/rounds`  | World Cup 2010  


List all games in a round for an event (league+season) `/event/:key/round/:pos`

- `/event/de.2012_13/round/5`  | Deutsche 1. Bundesliga 2012/13 - 5th Round  
- `/event/world.2010/round/20` | World Cup 2010 - 20th Round (=> Final) 


Try it live @ [`footballdb.herokuapp.com/api`](http://footballdb.herokuapp.com/api)



# How to use HTTP JSON API - `GET /event/euro.2012/teams`

List all teams for an event `/event/:key/teams`

```
GET /event/euro.2012/teams

{
  "event":
  {
    "key":"euro.2012",
    "title":"Euro 2012"
  },
  "teams":
  [
    {"key":"pol","title":"Poland","code":"POL"},
    {"key":"gre","title":"Greece","code":"GRE"},
    {"key":"rus","title":"Russia","code":"RUS"},
    {"key":"cze","title":"Czech Republic","code":"CZE"},
    {"key":"ned","title":"Netherlands","code":"NED"},
    {"key":"den","title":"Denmark","code":"DEN"},
    {"key":"ger","title":"Germany","code":"GER"},
    {"key":"por","title":"Portugal","code":"POR"},
    {"key":"esp","title":"Spain","code":"ESP"},
    {"key":"ita","title":"Italy","code":"ITA"},
    {"key":"irl","title":"Irland","code":"IRL"},
    {"key":"cro","title":"Croatia","code":"CRO"},
    {"key":"ukr","title":"Ukraine","code":"UKR"},
    {"key":"swe","title":"Sweden","code":"SWE"},
    {"key":"fra","title":"France","code":"FRA"},
    {"key":"eng","title":"England","code":"ENG"}
  ]
}
```



# How to use HTTP JSON API (Cont.) - `GET /event/euro.2012/rounds`

List all rounds for an event `/event/:key/rounds`

```
GET /event/euro.2012/rounds

{
  "event":
  {
    "key":"euro.2012",
    "title":"Euro 2012"
  },
  "rounds":
  [
    {"pos":1,"title":"1. Round"},
    {"pos":2,"title":"2. Round"},
    {"pos":3,"title":"3. Round"},
    {"pos":4,"title":"Quarter-finals"},
    {"pos":5,"title":"Semi-finals"},
    {"pos":6,"title":"Final"}
  ]
}
```



# How to use HTTP JSON API (Cont.) - `GET /event/euro.2012/round/6`

List all games in a round for an event `/event/:key/round/:pos`

```
GET /event/euro.2012/round/6

{
  "event": { "key":"euro.2012", "title":"Euro 2012" },
  "round": { pos": 6, "title": "Final" },
  "games":
  [
    {
    "team1_key": "esp",
    "team1_title": "Spain",
    "team1_code": "ESP",
    "team2_key": "ita",
    "team2_title": "Italy",
    "team2_code": "ITA",
    "play_at": "2012/07/01",
    "score1": 4,
    "score2": 0,
    "score3": null,
    "score4": null,
    "score5": null,
    "score6": null
    }
  ]
}
```


# How to use HTTP JSON(P) API in JavaScript & jQuery

In your hypertext (HTML) document using a plain vanilla cross-domain JavaScript request
(using the JSONP technique):

```
<script>
  function handleGames( json ) {
     // Do something with the returned data
  }
</script>

<script src="http://footballdb.herokuapp.com/api/v1/event/en.2012_13/round/2?callback=handleGames"></script>
```

Or using the jQuery library using the getJSON function:

```
$.getJSON('http://footballdb.herokuapp.com/api/v1/event/en.2012_13/round/2?callback=?', function(json) {
  // Do something with the returned data
});
```

Note: Add the `callback=?` query parameter to tell jQuery to use a cross-domain JSONP request.

That’s it.



# All Together Now - Machday Widget Example -  `footballdb.widget.js`

Use like:

```
var footballdb_widget = footballdb_widget_new( '#widget', '/api/v1' )
footballdb_widget.update( 'euro.2012', '1' )
footballdb_widget_update( 'euro.2012', '2' )
etc.
```

Results in:

```
Euro 2012 - Semi-finals

2012/06/27 | Portugal (POR) 2-4 iE / 0-0 nV / 0-0 Spain (ESP)
2012/06/28 | Germany (GER) 1-2 Italy (ITA)
```

Source:

```
var footballdb_widget_new = function( widget_id, api_path_prefix ) {
  
  var _api_path_prefix = '';
  var _$widget;
  
  function _init( widget_id, api_path_prefix )
  {
    _api_path_prefix = api_path_prefix;
    _$widget  = $( widget_id );
  }

  function _update( event_key, round_pos )
  {
    var api_link = _api_path_prefix + "/event/" + event_key + "/round/" + round_pos; 
    
    $.getJSON( api_link, function(json) {
    
      var snippet = "";  // build up a hypertext (html) snippet to add/append
  
      snippet += "<h3>";
      snippet += json.event.title;
      snippet += " - ";
      snippet += json.round.title;
      snippet += "</h3>";
  
      snippet += "<ul>";
  
      $.each( json.games, function( index, game ) {
        snippet += "<li>";
        snippet += game.play_at + " | ";
        snippet += game.team1_title + " (" + game.team1_code +")";

       if( game.score1 != null && game.score2 != null ) {
         if( game.score3 != null && game.score4 != null ) {
           if ( game.score5 != null && game.score6 != null ) {
             snippet += " " + game.score5 + "-" + game.score6 + " iE /";
           }
           snippet += " " + game.score3 + "-" + game.score4 + " nV /";
         }
         snippet += " " + game.score1 + "-" + game.score2;
      }
      else
        snippet += " - ";
      
      snippet += " " + game.team2_title + " (" + game.team2_code +")";
      snippet += "</li>";
    });

    snippet += "</ul>";
  
    _$widget.html( snippet );
    });  // getJSON
  }  // fn _update

  // call c'tor/constructor
  _init( widget_id, api_path_prefix );

  // return/export public api
  return {
     update: _update
  }
  
} // fn football_widget_new
```


# That's it. Thank you.

### Links 

- [github.com/openfootball](https://github.com/openfootball)

More Open Data Projects

- [github.com/openbeer](https://github.com/openbeer)
- [github.com/openwine](https://github.com/openwine)
- [github.com/openmundi](https://github.com/openmundi)

### Questions? Comments?



# Bonus: What's `beer.db`?

A free open public domain beer database & schema
for use in any (programming) language


(e.g. uses plain text fixtures/data sets)

```
### Brewery

guinness, St. James's Gate Brewery / Guinness Brewery, 1759, D, city:dublin
```

```
### Beer

Guinness|Guinness Draught, 4.2%, by:guinness, irish_dry_stout|dry_stout|stout
```

# Bonus: How to use HTTP JSON API - `GET /beer/guinness`

Get beer by key `/beer/:key`

```
GET /beer/guinness

{
  "beer":
  {
    "key":"guinness",
    "title":"Guinness",
    "synonyms": "Guinness Draught",
    "abv":"4.2",
    "srm":null,
    "og":null,
    "tags":["irish_dry_stout","dry_stout","stout"],
    "brewery":
    {
      "key": "guinness",
      "title": "St. James's Gate Brewery / Guinness Brewery"
    },
    "country":
    {
      "key":"ie",
      "title":"Irland"
    }
  }
}
```
