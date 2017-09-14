title: Meet Jason - The Universal Data Exchange Format - What's Next?


# Agenda

- (Non-Binary) "Human" Data Formats in Plain Text
- What's JSON (JavaScript Object Notation)?
  - JSON v1.0 - The New XML - XML for Script Kiddies Punks!?
  - JSON: The Fat-Free Alternative to XML
- Aside / Excursion - XML Evolved!?
- JSON v1.1 - JSON What's Next? - JSON v1.0 Evolved
  - JSON Extensions - JSON5 • HJSON • HanSON • SON • CSON • USON • JSONX • JSON11
  - HanSON - JSON for Humans
  - SON - Simple Object Notation
  - JSONX - JSON with Extensions (JSONX)
  - Fixing / Evolving JSON - Is it too late?
  - JSON 1.1 (JSONX) - Real-World Examples - JSON 1.0 vs JSON 1.1 - Less is More
- Case Study I - GraphQL
  - Gatsby GraphQL Example - Query Site Metadata
  - Gatsby GraphQL Example - Query Filesystem
  - GraphQL - What about Comments? What about Commas?
- Case Study II - JSON Feed - Universal Content Microservice
  - What's a Web Feed?
  - Web Feed Format == Universal Content Management Microservice JSON HTTP API
  - Triva Quiz - News Feeds in 2009 - What's RSS?
  - Web Feed Formats - XML, JSON, YAML, HTML, TXT
  - Triva Quiz - The Wonders of RSS 2.0 - Find the Content
  - Meet Jason Feed - A New Feed Format in JSON - feed.json
  - Why JSON? Why not XML?
  - Rules for Standard Makers - Evolve or Die ;-) - Break Things - Many Worlds Possible
  - Bonus: JSON Feed Podcast Example
  - Bonus: JSON Feed - Who's Who - Meet Brent Simmons n Manton Reece
- JSON Feed - What's Next?
- Beyond JSON - .TXT is the new JSON



# What's JSON (JavaScript Object Notation)?

The (New) Universal Data Exchange Format. Example:

```
{
  "Image": {
    "Width":  800,
    "Height": 600,
    "Title":  "View from 15th Floor",
    "Thumbnail": {
      "Url":    "http://www.example.com/image/481989943",
      "Height": 125,
      "Width":  100
    },
    "Animated" : false,
    "IDs": [116, 943, 234, 38793]
  }
}
```


# (Non-Binary) "Human" Data Formats in Plain Text

Ice Breaker Quiz

Q: What's your favorite text ("human"/non-binary)
data exchange format?

- (A) JSON
- (B) XML
- (C) HTML
- (D) YAML
- (E) TOML/INI
- (F) SQL
- (G) CSV
- (H) Other, please tell!


# JSON v1.0 - The New XML - XML for Script Kiddies Punks!?

> XML already does everything JSON does!
> And there's no way to differentiate between nodes and attributes!
> And there are no namespaces!
> And no schemas!
> What's the point of JSON?
>
> -- Anonymous


# JSON: The Fat-Free Alternative to XML

by Douglas Crockford (Inventor of JSON)

XML is not well suited to data-interchange, much as a wrench is not well-suited to driving nails. It carries a lot of baggage, and it doesn't match the data model of most programming languages. When most programmers saw XML for the first time, they were shocked at how ugly and inefficient it was. It turns out that that first reaction was the correct one. There is another text notation that has all of the advantages of XML, but is much better suited to data-interchange. That notation is JavaScript Object Notation (JSON).

...

XML is easily readable by both humans and machines

**JSON is easier to read for both humans and machines.**

XML is object-oriented

**Actually, XML is document-oriented. JSON is data-oriented. JSON can be mapped more easily to object-oriented systems.**

XML is being widely adopted by the computer industry

**JSON is just beginning to become known. Its simplicity and the ease of converting XML to JSON makes JSON ultimately more adoptable.**

...

(Source: [json.org/xml](http://www.json.org/xml.html))




# Aside / Excursion - XML Evolved!?

Triva Quiz: What's the difference between XML 1.0 and 1.1?

Bonus Question: Did you know there's XML version 1.1 :-)

What's next?  MicroXML - Anyone?

```
<memo lang="en" date="2017-05-01">
I <em>love</em> &#xB5;<!-- MICRO SIGN -->XML!<br/>
It's so clean &amp; simple.</memo>
```

Simplified XML with official JSON representation / mapping!

```
[ "memo",
  {  "date": "2017-05-01", "lang": "en" },
  [ "\nI ",
    ["em", {}, ["love"]],
    " \u03BCXML!",
    ["br", {}, []],
    "\nIt's so clean & simple."
  ]
]
```

(Source: [xml.com/articles/simplifying-xml-microxml](https://www.xml.com/articles/2017/06/03/simplifying-xml-microxml/))



# Aside / Excursion - XML Evolved!? (Cont.)

Q: What's JSONx?

A: JSONx is an IBM standard format to represent JSON as XML.

```
{
  "name": "Jason Feed",
  "address": {
    "streetAddress": "Siebenbrunnengasse 44",
    "city": "Vienna",
    "postalCode": 1050,
    "country": "AT"
  }
}
```

in XML (JSONx w/ default namespace):

```
<?xml version="1.0" encoding="UTF-8"?>
<object xsi:schemaLocation="http://www.datapower.com/schemas/json jsonx.xsd"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns="http://www.ibm.com/xmlns/prod/2009/jsonx">
  <string name="name">Jason Feed</string>
  <object name="address">
    <string name="streetAddress">Siebenbrunnengasse 44</string>
    <string name="city">Vienna</string>
    <number name="postalCode">1050</number>
    <string name="country">AT</string>
  </object>
</object>
```

in XML (JSONx w/ json namespace) - official IBM "enterprise" example:

```
<?xml version="1.0" encoding="UTF-8"?>
<json:object xsi:schemaLocation="http://www.datapower.com/schemas/json jsonx.xsd"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns=json="http://www.ibm.com/xmlns/prod/2009/jsonx">
  <json:string name="name">Jason Feed</json:string>
  <json:object name="address">
    <json:string name="streetAddress">Siebenbrunnengasse 44</json:string>
    <json:string name="city">Vienna</json:string>
    <json:number name="postalCode">1050</json:number>
    <json:string name="country">AT</json:string>
  </json:object>
</json:object>
```


# JSON v1.1 - JSON What's Next? - JSON v1.0 Evolved

What's missing in JSON v1.0?

> We can easily agree on what’s wrong with JSON,
> and I can't help wondering if it'd be worth fixing it.
>
> -- Tim Bray ([Fixing JSON](https://www.tbray.org/ongoing/When/201x/2016/08/20/Fixing-JSON))


> We need to fix engineers that try to 'fix JSON',
> absolutely nothing is broken with JSON.
>
> -- Anonymous



# JSON v1.1 - JSON What's Next? - JSON v1.0 Evolved

What's missing in JSON v1.0?

1. Comments, Comments, Comments, Please!
2. Unquoted Keys
3. Multi-Line Strings
   - a) Folded -- Folds Newlines (like HTML)
   - b) Unfolded
4. Trailing Commas in Arrays and Objects


More:

- Date/DateTime/Timestamp Type
- Optional Commas
- Optional Unquoted String Values
- "Raw" String (e.g. `''` instead of `""`)
  - No need to escape `\` or `"` etc. To escape `'` use `'''` e.g. `''''Henry's Themes'''`
- More Data Types (`set`, `map`, `symbol`, etc.)
- And Much More



# JSON Extensions - JSON5 • HJSON • HanSON • SON • CSON • USON • JSONX • JSON11

What's your favorite JSON extension format/flavor?

- (A) JSON5 - JSON for the ES5 Era
- (B) HJSON - A "Human" User Interface for JSON
- (C) HanSON - JSON for Humans
- (D) SON - Simple Object Notation
- (E) CSON - CoffeeScript-Object-Notation
- (F) USON - μson
- (G) JSONX / JSON11 - JSON with Extensions (JSONX) or JSON v1.1
- (H) Other, please tell!

(Source: [Awesome JSON - What's Next?](https://github.com/jsonii/awesome-json-next))



# HanSON - JSON for Humans

github: [timjansen/hanson](https://github.com/timjansen/hanson)

Adds:

- HanSON is JSON with comments, multi-line strings and unquoted property names.
- Comments use JavaScript syntax (`//`, `/**/`).
- Supports backticks as quotes (\`\`) for multi-line strings.
- You can use either double-quotes (`""`) or single-quotes (`''`) for single-line strings.
- Property names do not require quotes if they are valid JavaScript identifiers.
- Commas after the last list element or property will be ignored.
- Every JSON string is valid HanSON.


```js
{
  listName: "Sesame Street Monsters", // note that listName needs no quotes
  content: [
    {
      name: "Cookie Monster",
      /* Note the template quotes and unescaped regular quotes in the next string */
      background: `Cookie Monster used to be a
monster that ate everything, especially cookies.
These days he is forced to eat "healthy" food.`
    }, {
      // You can single-quote strings too:
      name: 'Herry Monster',
      background: `Herry Monster is a furry blue monster with a purple nose.
He's mostly retired today.`
    },    // don't worry, the trailing comma will be ignored
   ]
}
```


# HanSON - JSON for Humans (Cont.)

Want to use HanSON in your program, without including any libraries?

Use this function to convert
HanSON to JSON. It returns a JSON string that can be read using `JSON.parse()`.

```js
function toJSON(input) {
		var UNESCAPE_MAP = { '\\"': '"', "\\`": "`", "\\'": "'" };
		var ML_ESCAPE_MAP = {'\n': '\\n', "\r": '\\r', "\t": '\\t', '"': '\\"'};
		function unescapeQuotes(r) { return UNESCAPE_MAP[r] || r; }

		return input.replace(/`(?:\\.|[^`])*`|'(?:\\.|[^'])*'|"(?:\\.|[^"])*"|\/\*[^]*?\*\/|\/\/.*\n?/g, // pass 1: remove comments
							 function(s) {
			if (s.charAt(0) == '/')
				return '';
			else  
				return s;
		})
		.replace(/(?:true|false|null)(?=[^\w_$]|$)|([a-zA-Z_$][\w_$]*)|`((?:\\.|[^`])*)`|'((?:\\.|[^'])*)'|"(?:\\.|[^"])*"|(,)(?=\s*[}\]])/g, // pass 2: requote
							 function(s, identifier, multilineQuote, singleQuote, lonelyComma) {
			if (lonelyComma)
				return '';
			else if (identifier != null)
					return '"' + identifier + '"';
			else if (multilineQuote != null)
				return '"' + multilineQuote.replace(/\\./g, unescapeQuotes).replace(/[\n\r\t"]/g, function(r) { return ML_ESCAPE_MAP[r]; }) + '"';
			else if (singleQuote != null)
				return '"' + singleQuote.replace(/\\./g, unescapeQuotes).replace(/"/g, '\\"') + '"';
			else
				return s;
		});
}
```

(Source: [github.com/timjansen/hanson](https://github.com/timjansen/hanson))



# SON - Simple Object Notation

github: [aleksandergurin/simple-object-notation](https://github.com/aleksandergurin/simple-object-notation)

Adds:

- comments starts with # sign and ends with newline (\n)
- comma after a key-value pair is optional
- comma after an array element is optional

```
{
  # Personal information

  "name": "Alexander Grothendieck"
  "fields": "mathematics"
  "main_topics": [
    "Etale cohomology"
    "Motives"
    "Topos theory"
    "Schemes"
  ]
}
```


# JSONX - JSON with Extensions (JSONX)

JSON v1.1 - JSON Evolved for Humans - Easy-to-Write, Easy-to-Read

github: [jsonii](https://github.com/jsonii)

JSON v1.1 includes all JSON extensions from HanSON (JSON for Humans):

- quotes for strings are optional if they follow JavaScript identifier rules.
- you can alternatively use backticks, as in ES6's template string literal, as quotes for strings.
  A backtick-quoted string may span several lines and you are not required to escape regular quote characters,
  only backticks. Backslashes still need to be escaped, and all other backslash-escape sequences work like in
  regular JSON.
- for single-line strings, single quotes (`''`) are supported in addition to double quotes (`""`)
- you can use JavaScript comments, both single line (`//`) and multi-line comments (`/* */`), in all places where JSON allows whitespace.
- Commas after the last list element or object property will be ignored.

Plus all JSON extensions from SON (Simple Object Notation):

- comments starts with `#` sign and ends with newline (`\n`)
- comma after an object key-value pair is optional
- comma after an array item is optional

Example:

```
{
  #  use shell-like comments

  listName: "Sesame Street Monsters"   # note: comments after key-value pairs are optional  
  content: [
    {
      name: "Cookie Monster"
      // note: the template quotes and unescaped regular quotes in the next string
      background: `Cookie Monster used to be a
monster that ate everything, especially cookies.
These days he is forced to eat "healthy" food.`
    }, {
      // You can single-quote strings too:
      name: 'Herry Monster',
      background: `Herry Monster is a furry blue monster with a purple nose.
He's mostly retired today.`
    },    /* don't worry, the trailing comma will be ignored  */
   ]
}
```


# Fixing / Evolving JSON - Is it too late?

Yes, it's too late for JSON 1.0 (that is, `JSON.parse`).

No, it's never too late for JSON 1.1 ;-). Evolve or die.

Use `JSONX.parse` for version 1.1 ;-)

or

use a JSON preprocessor.

Note: Use `JSONX.convert` (or `JSON11.convert`) to convert JSONX text to ye old' JSON text.
`JSONX.parse == JSON.parse( JSONX.convert( text ))`.



#  JSON 1.1 (JSONX) - Real-World Examples - JSON 1.0 vs JSON 1.1 - Less is More

Code Libraries, Web Feeds, Tables & Schemas, Map Features, & More

package.json • feed.json • datapackage.json • geojson

See [jsonii.github.io »](https://jsonii.github.io).




# Case Study I - GraphQL

What's GraphQL?

A data query language (by example).

QL => Query Language  

![](i/graphqlorg-website.png)

(Source: [graphql.org](http://graphql.org))


# Gatsby GraphQL Example - Query Site Metadata

```
{
    site {
      siteMetadata {
        title
      }
    }
}
```

resulting in:

```
{
    "site": {
      "siteMetadata": {
        "title": "Gatsby (+GraphQL) Stay Static Site Sample"
      }
    }
}
```


# Gatsby GraphQL Example - Query Filesystem

```
{
  allFile
  { edges
    { node {
        relativePath
        prettySize
        birthTime
  }}}
}
```

resulting in:

```
{
    "allFile": {
      "edges": [
        {
          "node": {
            "relativePath": "pages/about.js",
            "prettySize": "343 B",
            "birthTime": "2017-09-02T16:21:53.799Z"
          }
        },
        {
          "node": {
            "relativePath": "pages/index.js",
            "prettySize": "863 B",
            "birthTime": "2017-09-02T16:53:52.821Z"
          }
        },
        {
          "node": {
            "relativePath": "pages/posts/new-repo-maps.md",
            "prettySize": "516 B",
            "birthTime": "2017-09-02T16:21:53.799Z"
          }
        },
        {
          "node": {
            "relativePath": "pages/posts/new-build-system.md",
            "prettySize": "892 B",
            "birthTime": "2017-09-02T16:21:53.799Z"
          }
        },
        {
          "node": {
            "relativePath": "pages/posts/new-season.md",
            "prettySize": "1.03 kB",
            "birthTime": "2017-09-02T16:21:53.799Z"
          }
        }
      ]
    }
}
```

# GraphQL - What about Comments? What about Commas?

GraphQL source documents may contain single‐line comments, starting with the # marker.

A comment can contain any Unicode code point except LineTerminator
so a comment always consists of all code points starting with the # character
up to but not including the line terminator.

Comments behave like white space and may appear after any token, or before a line terminator,
and have no significance to the semantic meaning of a GraphQL query document.

(Source: [facebook.github.io/graphql/#Comment](https://facebook.github.io/graphql/#Comment))

Insignificant Commas. Similar to white space and line terminators, commas (,) are used to improve
the legibility of source text and separate lexical tokens but are otherwise syntactically and semantically insignificant within GraphQL query documents.

Non‐significant comma characters ensure that the absence or presence of a comma does not meaningfully alter the interpreted syntax of the document, as this can be a common user‐error in other languages. It also allows for the stylistic use of either trailing commas or line‐terminators as list delimiters which are both often desired for legibility and maintainability of source code.

(Source: [facebook.github.io/graphql/#Comma](https://facebook.github.io/graphql/#Comma))


# Case Study II - JSON Feed - Universal Content Microservice

What's a Web Feed? What's JSON Feed?



# What's a Web Feed?

<img src='i/web-feed-icon.png' style='float: right;'>

A web feed (or news feed) is a (simple) document/text format
that:

(1) lets you publish a list of:

- status updates, blog postings, articles, pictures, cartoons, recordings, etc.

and that

(2) lets others subscribe to your updates.



# Web Feed Format == Universal Content Management Microservice JSON HTTP API

Feed format sounds boring!? How about:

A Univeral Server-less Head-less Microservice
Content Management HTTP JSON API.

Easy. Free. Simple.



# Triva Quiz - News Feeds in 2009 - What's RSS?

- (A) RDF Site Summary
- (B) Rich Site Summary
- (C) Really Simple Syndication
- (D) Really Simple, Stupid
- (E) Rapid Syndicaton Solution

RDF = Resource Description Framework


# Web Feed Formats - XML, JSON, YAML, HTML, TXT

- RSS 2.0 (0.91, 0.92) a.k.a. Really Simple Syndication - in XML
- RSS 1.0 a.k.a. RDF Site Summary - in RDF/XML
- Atom - in XML
- JSON Feed - in - surprise, surprise - JSON
- Microformats (h-feed/h-entry) - in HTML
- Feed.TXT - in plain text; metadata in (simplified) YAML or JSON; Markdown

And some more. Let's celebrate diversity! Live and let live!



# Triva Quiz - The Wonders of RSS 2.0 - Find the Content

What's your favorite way to add content in hypertext to RSS 2.0?

- (A) `<description>`
- (B) `<content:encoded>`  from RDF/RSS 1.0 content module extension
- (C) `<media:content>`  from Yahoo! search extension
- (D) Other?  Please, tell!

Bonus: Is your content in plain text, in html, in xhtml, in html escaped?
Is your content a summary? or full text?



# Meet Jason Feed - A New Feed Format in JSON - feed.json

![](i/jsonfeed.png)

More [jsonfeed.org](https://jsonfeed.org) »



# Meet Jason Feed - A New Feed Format in JSON - feed.json (Cont.)

![](i/jsonfried.png)

```
{
    "version": "https://jsonfeed.org/version/1",
    "title": "Jason Fried's Microblog",
    "home_page_url": "https://micro.blog/jasonfried/",
    "feed_url": "https://micro.blog/jasonfried/feed.json",
    "author": {
        "name": "Jason Fried",
        "url": "https://micro.blog/jasonfried/",
        "avatar": "https://micro.blog/jasonfried/avatar.png"
    },
    "items": [
        {
            "id": "865767227416612864",
            "url": "https://micro.blog/jasonfried/status/865767227416612864",
            "content_text": "JSON Feed? I know that guy.",
            "date_published": "2017-05-19T20:12:00-00:00"
        }
    ]
}
```


# Meet Jason Feed - A New Feed Format in JSON - feed.json (Cont.)

Use `content_text` for text

```
"content_text": "JSON Feed? I know that guy."
```

and `content_html` for hypertext.

```
"content_html": "<p>JSON Feed? I know that guy.</p>"
```

No artificial intelligence (AI) needed ;-)



# Meet Jason Feed - A New Feed Format in JSON - feed.json (Cont.)

Bonus: Use `summary` for summaries

and `content_text` or `content_html` for full content.

Note: "Full" content might just be:

```
"content_text": "Me!"
```


# Why JSON? Why not XML?

JSON is the new XML ;-)  Easier. Free. Simpler.

![](i/google-trends-xml-json.png)





#  Rules for Standard Makers - Evolve or Die ;-) - Break Things - Many Worlds Possible

Standards - Standards - Standards  

![](i/xkcd-date.png)

(Source: [xkcd.com/1179](https://xkcd.com/1179/))


#  Rules for Standard Makers - Evolve or Die ;-) - Break Things - Many Worlds Possible (Cont.)

One (format) to rule them all.
All your base are belong to us.
JavaScript is eating the world ;-)

![](i/xkcd-standards.png)

(Source: [xkcd.com/927](https://xkcd.com/927/))



# Bonus: JSON Feed Podcast Example

Yes, works "out-of-the-box" for podcasts too.
Use attachments for audio recordings (media files).

```
{
    "version": "https://jsonfeed.org/version/1",
    "user_comment": "This is a podcast feed. You can add this feed to your podcast client using the following URL: http://therecord.co/feed.json",
    "title": "The Record",
    "home_page_url": "http://therecord.co/",
    "feed_url": "http://therecord.co/feed.json",
    "items": [
        {
            "id": "http://therecord.co/chris-parrish",
            "title": "Special #1 - Chris Parrish",
            "url": "http://therecord.co/chris-parrish",
            "content_text": "Chris has worked at Adobe and as a founder of Rogue Sheep, which won an Apple Design Award for Postage. Chris’s new company is Aged & Distilled with Guy English — which shipped Napkin, a Mac app for visual collaboration. Chris is also the co-host of The Record. He lives on Bainbridge Island, a quick ferry ride from Seattle.",
            "content_html": "Chris has worked at <a href=\"http://adobe.com/\">Adobe</a> and as a founder of Rogue Sheep, which won an Apple Design Award for Postage. Chris’s new company is Aged & Distilled with Guy English — which shipped <a href=\"http://aged-and-distilled.com/napkin/\">Napkin</a>, a Mac app for visual collaboration. Chris is also the co-host of The Record. He lives on <a href=\"http://www.ci.bainbridge-isl.wa.us/\">Bainbridge Island</a>, a quick ferry ride from Seattle.",
            "summary": "Brent interviews Chris Parrish, co-host of The Record and one-half of Aged & Distilled.",
            "date_published": "2014-05-09T14:04:00-07:00",
            "attachments": [
                {
                    "url": "http://therecord.co/downloads/The-Record-sp1e1-ChrisParrish.m4a",
                    "mime_type": "audio/x-m4a",
                    "size_in_bytes": 89970236,
                    "duration_in_seconds": 6629
                }
            ]
        }
    ]
}
```


# Bonus: JSON Feed - Who's Who - Meet Brent Simmons n Manton Reece

Brent Simmons? - I know this guy!

Manton Reece? - I know that guy!

Brent Simmons built the NetNewsWire  more than 10+ years ago;
was first (most popular) news reader for Apple Mac OS X.

Blogs at [inessential.com](http://inessential.com)
with [feed.json](http://inessential.com/feed.json) ;-)


Manton Reece builds micro.blog - a timeline and publishing platform for the open web.
Also built the WordPress JSON Feed plugin ;-)

Blogs at [manton.org](http://manton.org) with [feed.json](http://manton.org/feed/json) ;-)




# JSON Feed - What's Next?

JSON Feed uses JSON 1.0!

JSON v1.1 Feed - Anyone?

```
{
  version       : 'https://jsonfeed.org/version/1'
  title         : 'My Example Feed'
  home_page_url : 'https://example.org/'
  feed_url      : 'https://example.org/feed.json'
  items         : [
        {
            id           : '2'
            content_text : 'This is a second item.'
            url          : 'https://example.org/second-item'
        }
        {
            id           : '1'
            content_html : '<p>Hello, world!</p>'
            url          : 'https://example.org/initial-post'
        }
    ]
}
```


# Beyond JSON - .TXT is the new JSON

Feed.TXT (or Journal.TXT) is the new JSON Feed!

See [feedtxt.github.io](https://feedtxt.github.io)
or [journaltxt.github.io »](https://journaltxt.github.io).


```
---
year:  2017
month: July
day:   Wed 19
---
Let's reinvent push-button publishing on the internets!
Use a single-file for your journal / diary / blog. That's it.
---
day:   Thu 20
---
Crazy idea? Let's put up a website and a example blog auto-generated from journal.txt.
---
day:   Fri 21
---
Did you know? The single-file format works great for advent calendars
or beer-of-the-day calendars.
---
day:    Sat 22
---
Let's add another example. A diary about the Oktoberfest 2016. Prost. Cheers.
---
day:    Sun 23
---
Let's rest.
```
