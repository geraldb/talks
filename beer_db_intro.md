# `beer.db` - Using Open Beer & Brewery Data in Ruby

### Agenda

- What's `beer.db `?
- Example: Breweries - `breweries.txt`
- Example: Beers  - `beers.txt`
- How to use `beer.db` in Ruby
- What's `world.db `?
- How to use `beer.db` & `world.db` in Ruby
- How to use HTTP JSON API - `GET /beer/guinness`
- Design - Plain text format options
- Design - Better comma separated values (CSV) format
- Design - "Magic" - Use folder and file names for values and structure
- Design - "Magic" - Use text patterns (regex) for fields



# What's `beer.db `?

A free open public domain beer database & schema
for use in any (programming) language


(e.g. uses plain text fixtures/data sets)

~~~
### Brewery

guinness, St. James's Gate Brewery / Guinness Brewery, 1759, city:dublin
~~~

~~~
### Beer

Guinness|Guinness Draught, 4.2%, by:guinness, irish_dry_stout|dry_stout|stout
~~~


# Example: Breweries - `breweries.txt`

~~~
##############################
# Wien (W)

[ottakringer]
  Ottakringer Brauerei, 1838
  1160 Wien // Ottakringer Straße 91
~~~

Source: [`at-austria/w-wien/breweries.txt`](https://github.com/openbeer/at-austria/blob/master/1--w-wien--eastern/breweries.txt)

~~~
###########################
# Niederösterreich (N)

[zwettler]
  Zwettler Brauerei, 1709
  3910 Zwettl // Syrnauer Straße 22-25

[weitra]
  Weitra Bräu Bierwerkstatt, 1321
  3970 Weitra // Sparkasseplatz 160
  zwettler
~~~

Source: [`at-austria/n-niederoesterreich/breweries.txt`](https://github.com/openbeer/at-austria/blob/master/1--n-niederoesterreich--eastern/breweries.txt)



# Example: Beers  - `beers.txt`

~~~
__________________________
- Ottakringer Brauerei

Ottakringer Helles,                  5.2 %, 11.8°
Ottakringer Gold Fassl Spezial,      5.6 %, 12.7°
Ottakringer (Gold Fassl) Pur,        5.2 %, 11.8°, bio
Ottakringer (Gold Fassl) Pils,       4.6 %, 11.2°
Ottakringer (Gold Fassl) Zwickl,     5.2 %, 12.2°
~~~

Source: [`at-austria/w-wien/beers.txt`](https://github.com/openbeer/at-austria/blob/master/1--w-wien--eastern/beers.txt)

~~~
_______________________
- Zwettler Brauerei

Zwettler Original,     5.1 %,  11.9°
Zwettler Pils,         4.9 %,  11.5°
Zwettler Zwickl,       5.5 %,  12.5°
Zwettler Dunkles,      3.4 %,  11.5°

_________________
- Weitra Bräu

Weitra Helles,  5.0 %,   11.8°
Hadmar,         5.2 %,   12.5°,  bio
~~~

Source: [`at-austria/n-niederoesterreich/beers.txt`](https://github.com/openbeer/at-austria/blob/master/1--n-niederoesterreich--eastern/beers.txt)



# How to use `beer.db` in Ruby

Brewery Model

~~~
by = Brewery.find_by( key: 'guinness' )

by.title
=> 'St. James's Gate Brewery / Guinness Brewery'

by.country.key
=> 'ie'

by.country.title
=> 'Irland'

by.city.title
=> 'Dublin'

by.beers.first
=> 'Guinness', 4.2

...
~~~

# How to use `beer.db` in Ruby (Cont.)

Beer Model

~~~
b = Beer.find_by( key: 'guinness' )

b.title
=> 'Guinness'

b.abv
=> 4.2

b.tags
=> 'irish_dry_stout', 'dry_stout', 'stout'

b.brewery.title
=> 'St. James's Gate Brewery / Guinness Brewery'

...
~~~


# What's `world.db `?

A free open public domain world database & schema
for use in any (programming) language

(e.g. uses plain text fixtures/data sets)


~~~
###  Countries

at, Austria, AUT, 83_871 km², 8_414_638, un|eu|euro|schengen|central_europe|western_europe
~~~

~~~
###  Regions (States/Länder/Provinces)

w, Wien [Vienna], 415 km², eastern austria
~~~

~~~
### Cities

Wien [Vienna],  W,  1_731_236, m:1_724_000
~~~


# How to use `beer.db` & `world.db` in Ruby

Country Model

~~~
at = Country.find_by( key: 'at' )

at.title
=> 'Austria'

at.regions.count
=> 9

at.beers
=> 'Weitra Helles', 'Hadmar', 'Zwettler Original', ...

at.breweries
=> 'Weitra Bräu Bierwerkstatt', 'Zwettler Brauerei', ...

...
~~~


# How to use `beer.db` & `world.db` in Ruby (Cont.)

City Model

~~~
wien = City.find_by( key: 'wien' )

wien.title
=> 'Wien'

wien.beers
=> 'Ottakringer Helles', 'Ottakringer (Gold Fassl) Zwickl', ...

wien.breweries
=> 'Ottakringer Brauerei'

...
~~~


# How to use HTTP JSON API - `GET /beer/guinness`

Get beer by key `/beer/:key`

~~~
GET /beer/guinness

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
~~~


# How to use HTTP JSON API (Cont.) - `GET /brewery/guinness`

Get brewery by key `/brewery/:key`

~~~
GET /brewery/guinness

{
  "key": "guinness",
  "title": "St. James's Gate Brewery / Guinness Brewery",
  "synonyms": null,
  "since": 1759,
  "address": "St. James's Gate // Dublin",
  "tags": ["diageo"],
  "beers":
  [
    {
      "key": "guinness",
      "title": "Guinness"
    }
  ],
  "country":
  {
    "key": "ie",
    "title": "Irland"
  }
}
~~~


# Design - Plain text format options

- CSV (Comma Separated Values)
- TSV (Tab Separated Values)
- JSON (JavaScript Object Notation)
- XML (eXtensible Markup Language)
- YML (Yet Another Markup Language)
- SQL (Structured Query Language)


# Design - Better comma separated values (CSV) format

What's great with CSV?

- Simple plain text
- One line, one record

What's wrong with CSV?

- No comments
- Escaping rules for quotes and commas ugly and not really simple
- No optional fields
- Fixed order of fields
- No "magic" e.g.
    - No auto key generation from title field if leading key field missing (e.g. Guinness => `guinness`)
    - No autofill field values from folder names or file names
        - e.g. `europe/at` sets continent to `europe` and country to `at`
        - e.g. `beers.2.txt` sets type/class to `beer` and grade to `2`


# Design -"Magic" - Use folder and file names for values and structure

Do __NOT__ put everything into a single file e.g. `beers.csv`. Better use folders
 and use folder and file names for adding some "magic" e.g. to autofill values
 e.g. set values for continent, country, city, district, etc. Example:

`beer.db`:

~~~
europe/
    at-austria/
      n-niederoesterreich/
         beers.txt
         breweries.txt
      o-oberoesterreich/
         beers.txt
         breweries.txt
         ...
north-america/
    mx-mexico/
         beers.txt
         breweries.txt
         ...
pacific/
    au-australia/
         ...
...
~~~

or

`wien.db`:

~~~
1-innere-stadt/
               brewpubs.txt
               cafes.txt
               cinemas.txt
2-leopoldstadt/
              ...
3-landstraße/
            ...

~~~


# Design - "Magic" - Use text patterns (regex) for fields

### Year brewery established

e.g. 1759 or 1321

Always four digits => `/[0-9]{4}/`


### Region (e.g. state/bundesland/etc.)

e.g. TX => Texas, or  N => Niederösterreich

Always one or two uppercase letters  => `/[A-Z]{1,2}/`


### Alcohol by volume (ABV)

e.g. 5.4% => 5.4

Numbers ending with precent (%) => `/[0-9]+(\.[0-9]+)?%/`


And many more text patterns in use.



# That's it. Thanks.

### Links

- [github.com/openbeer](https://github.com/openbeer) - `beer.db` repos
- [github.com/openfootball](https://github.com/openfootball) - `football.db` repos
- [github.com/openmundi](https://github.com/openmundi) - `world.db` repos


### Questions? Comments?

Send them along to the [Open Beer, Brewery n Brewpub Database Forum/Mailing List](http://groups.google.com/group/beerdb).
Thanks!

