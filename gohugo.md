title: Stay Static - Build Static (Web)sites w/ Hugo and Go templates

# Flash Back

## Flash Back - Oldie but Goldie

Thanks for welcoming back.

We ♥ Programming Language Talk (Nov, 2014)

[**Building Web Services (HTTP JSON APIs) with Go**](https://github.com/geraldb/talks/blob/master/go_http_api.md)
[(Slides)](http://slideshow-s9.github.io/demos/go_http_api.html)


## Flack Back - Oldie but Goldie (Cont.)

**Why Go? - Faster, Faster, Faster**

Use what works for you.

Kind of a "better" more "modern" C.

Code gets compiled (to machine-code ahead-of-time) and linked
to let you build (small-ish) zero-dependency
all-in-one binary (file) programs.

No virtual machine or byte code runtime or just-in-time compiler machinery needed;
includes garbage collector.

### Go's Headline Features

- Fast Builds - Imports, Package Structure, etc.
- Concurreny - Go (co)-routines, channels
    e.g. "agent model" - no threads, no shared state/variables - message passing w/ agents, etc.


## Agenda



## Dynamic (Web)Site Generators

**The Biggies** (PHP Rules!)

- WordPress
- Drupal
- Joomla!

On your live production site requires

- database (e.g. mysql)
- application server (e.g. mod_php)
- web server (e.g. apache)

On every request - (re)builds the page on-the-fly e.g. queries the database, runs scripts,
merges templates, etc.



## Static (Web)Site Generators / Builders

On your live production site requires

- web server (e.g. apache, ngnix, etc.) only

- **NO** database needed
- **NO** application server needed

Note: You can even go "server-less" e.g.
host your site on a web service e.g. Amazon S3 (Simple Storage Service).

You build the complete site, that is, **all pages** "ahead-of-time"
on a "build" machine. You will end-up with a bunch of (static) ready-to-use
HTML, CSS and JS files (and media files e.g. images, etc.). 
Upload to production site and you're live
w/ a kind of "super cache".


## Static (Web)Site Generators / Builders

**The Biggies**

1. ?
2. ?
3. ?



## Hello, Hugo!

by Steve Francia, Bjørn Erik Pedersen et al (★10 166) -
web: [`gohugo.io`](https://gohugo.io),
github: [`spf13/hugo`](https://github.com/spf13/hugo)

![](i/staticgen-hugo.png)


## News Flash - New Hugo Release v0.16 on June 6th, 2016

Over 550 contributions by over 110 contributors to the main Hugo codebase.
Since last release (on November 25th, 2015)
Hugo has gained 3500 stars, 90 contributors and 23 additional themes.

What's news?

- Partial Builds
- Template Improvements e.g. now w/ blocks and many more
  new template functions (e.g. countwords, jsonify, md5, readFile, etc.);
- And Much More

See [Release Notes](http://gohugo.io/meta/release-notes).


## Getting Started w/ Hugo

Hugo is an all-in-one single-file binary (e.g. ~15 Megs) 
Download the package from the [release page](https://github.com/spf13/hugo/releases) 
and unpack the Hugo binary. That's it.

> Linux Tip: Hugo has become part of the official Debian and Ubuntu repositories since January 2016!
> If you run the latest version, simply run `apt-get install hugo` to get started.

Try:

```
$ hugo help
```

prints

```
hugo is the main command, used to build your Hugo site.

Hugo is a Fast and Flexible Static Site Generator
built with love by spf13 and friends in Go.

Complete documentation is available at http://gohugo.io/.

Usage:
  hugo [flags]
  hugo [command]

Available Commands:
  server      A high performance webserver
  version     Print the version number of Hugo
  config      Print the site configuration
  check       Check content in the source directory
  benchmark   Benchmark hugo by building a site a number of times.
  convert     Convert your content to different formats
  new         Create new content for your site
  list        Listing out various types of content
  undraft     Undraft changes the content's draft status from 'True' to 'False'
  import      Import your site from others.
  gen         A collection of several useful generators.

Flags:
  -b, --baseURL string          hostname (and path) to the root, e.g. http://spf13.com/
  -D, --buildDrafts             include content marked as draft
  -F, --buildFuture             include content with publishdate in the future
      --cacheDir string         filesystem path to cache directory. Defaults: $TMPDIR/hugo_cache/
      --canonifyURLs            if true, all relative URLs will be canonicalized using baseURL
      --cleanDestinationDir     Remove files from destination not found in static directories
      --config string           config file (default is path/config.yaml|json|toml)
  -c, --contentDir string       filesystem path to content directory
  -d, --destination string      filesystem path to write files to
      --disable404              Do not render 404 page
      --disableRSS              Do not build RSS files
      --disableSitemap          Do not build Sitemap file
      --forceSyncStatic         Copy all files when static is changed.
      --ignoreCache             Ignores the cache directory
  -l, --layoutDir string        filesystem path to layout directory
      --log                     Enable Logging
      --logFile string          Log File path (if set, logging enabled automatically)
      --noTimes                 Don't sync modification time of files
      --pluralizeListTitles     Pluralize titles in lists using inflect (default true)
      --preserveTaxonomyNames   Preserve taxonomy names as written ("Gérard Depardieu" vs "gerard-depardieu")
      --renderToMemory          render to memory (only useful for benchmark testing)
  -s, --source string           filesystem path to read files relative from
      --stepAnalysis            display memory and timing of different steps of the program
  -t, --theme string            theme to use (located in /themes/THEMENAME/)
      --uglyURLs                if true, use /filename.html instead of /filename/
  -v, --verbose                 verbose output
      --verboseLog              verbose logging
  -w, --watch                   watch filesystem for changes and recreate as needed

Use "hugo [command] --help" for more information about a command.
```


# Hugo Stay Static Sample Site

Shows how-to-use:

- Posts (e.g. Blog Posts with Published Dates)
- Pages  (e.g. About Page)
- Datafiles (e.g. Bookmarks n Links)

![](i/staystatic-samplesite.png)



## Hugo Stay Static site - File Structure




# Hugo Stay Static Site - HTML Templates

**Hugo** - Go Template Language

~~~
<!DOCTYPE html>
<html>
  <%%= partial "partials/head" %>
  <body>
    <%%= partial "partials/header" %>
    <div class="main">
      <%%= yield %>
    </div>
    <%%= partial "partials/footer" %>
  </body>
</html>
~~~

(Source: [`staystatic/middleman/layouts/layout.erb`](https://github.com/staystatic/middleman/blob/master/source/layouts/layout.erb))




## Hugo Stay Static Site - Pages n Posts with Front Matter

**Hugo** - TOML + Markdown

~~~
---
layout: post
title:  beer.db - New Repo /maps - Free Interactive Beer Maps w/ Brewery Listings
---

The beer.db project - offering free public domain beer, brewery
and brewpubs data - added a new repo, that is, `/maps`
for hosting 'full-screen' interactive beer maps with brewery listings.

See an example [beer map for Austria](http://openbeer.github.io/maps/at)
(~200 breweries n brewpubs) live or
[check the source](https://github.com/openbeer/maps) using the mapbox.js mapping library.

...
~~~

(Source: [`staystatic/middleman/source/posts/2014-11-11-new-repo-maps.html.md`](https://github.com/staystatic/middleman/blob/master/source/posts/2014-11-11-new-repo-maps.html.md))




## Markdown Madness

Markdown Library Options in Go

- No (Official) Standard Markdown Library

1. Blackfriday  [Add star here] 1 858 by Russ Ross  (github: [russross/blackfriday](https://github.com/russross/blackfriday))
2. Mmark  [Add star here] 84 by Miek Gieben (github: [miekg/mmark](https://github.com/miekg/mmark))
   - A fork of Blackfriday; adds (even) more markdown extensions e.g. titleblocks, parts, asides, callouts and much more


## Markdown Goodies / Extensions

- Tables
- Fenced Code Blocks ("GitHub"-Style)
- Footnotes
- Typgraphy
  - Smart Quotes
  - Smart Fractions
- Definition Lists
- Strikethrough
- No Intra-Word Emphasis


## Markdown Goodies - Tables

```
Feature                  | Hugo
------------------------ | ------------
Settings / Configuration | TOML
Front Matter / Meta Data | TOML
| Datafiles              | TOML
HTML Templates           | Go Templates
HTML "Shortcodes"        | Markdown
```

becomes

Feature                  | Hugo
------------------------ | ------------
Settings / Configuration | TOML
Front Matter / Meta Data | TOML
| Datafiles              | TOML
HTML Templates           | Go Templates
HTML "Shortcodes"        | Markdown



## Markdown Goodies - Fenced Code Blocks

    ```
    func markdownRender(ctx *RenderingContext) []byte {
	    return blackfriday.Markdown(ctx.Content, getHTMLRenderer(0, ctx),
		         getMarkdownExtensions(ctx))
    }
    ```

## Markdown Goodies - Footnotes

```
This is a footnote.[^1]

[^1]: the footnote text.
```

becomes

This is a footnote. <sup>1</sup>

1. the footnote text. ↩



## Markdown Goodies - Typography

**Smart Quotes**

```
Hugo says "Hello, World!"   # e.g  ".." => “..”
Hugo says 'Hello, World!'   # e.g. '..' => ‘..’
```

becomes

Hugo says "Hello, World!" <br>
Hugo says ‘Hello, World!’


**Smart Fractions**

4/5 => <sup>4</sup>&frasl;<sub>5</sub>




## Markdown Goodies - Definition Lists

```
Markup
: the difference between the cost price and the selling price

Markdown
: a reduction in price, usually to encourage buying
: the amount by which a price is reduced
```

becomes

Markup
: the difference between the cost price and the selling price

Markdown
: a reduction in price, usually to encourage buying
: the amount by which a price is reduced




## Markdown Goodies

**Strikethrough**

```
Markdown Madness 50% Off Now Only €199 was ~~€399~~ Buy Now!!!
```

becomes

Markdown Madness 50% Off Now Only €199 was ~~€399~~ Buy Now!!!


**No Intra-Word Emphasis**

```
This is _emphasized_.
And this say_hello_world method is not.
```

becomes

This is _emphasized_.<br>
And this say_hello_world method is not.



# Hugo Stay Static Site - Datafiles

**Hugo** - TOML

~~~
#############################
#  Links 'n' Bookmarks

- title: football.db - Open Football Data
  url:   https://github.com/openfootball

- title: beer.db - Open Beer, Brewery 'n' Brewpub Data
  url:   https://github.com/openbeer

- title: world.db - Open World Data
  url:   https://github.com/openmundi
~~~

(Source: [`staystatic/middleman/data/links.yml`](https://github.com/staystatic/middleman/blob/master/data/links.yml))



# Hugo Stay Static Site  - HTML Templates - Loops

**Template** - Go Template Language

~~~
<div>
  <b>Links 'n' Bookmarks</b>
  <ul>
    <%% data.links.each do |link| %>
      <li><%%= link_to link.title, link.url %></li>
    <%% end %>           
  </ul>
</div>
~~~

~~~
<div>
  <b>News 'n' Updates</b>
  <ul>
    <%% blog.articles.each do |article| %>
      <li><%%= link_to article.title, article.url %></li>
    <%% end %>
  </ul>
</div>
~~~

(Source: [`staystatic/middleman/source/index.html.erb`](https://github.com/staystatic/middleman/blob/master/source/index.html.erb))



## HTML Template Options in Go

Standard Go Template Language

- [Text Template Package](https://golang.org/pkg/text/template) e.g. `import "text/template"`
- [HTML Template Package](https://golang.org/pkg/html/template) e.g. `import "html/template"`--
  same interface as text/template package but automatically
  secures HTML output against certain attacks
  (e.g. knows HTML, CSS, JavaScript, and URIs.)






## HTML Standard Go template

New in Go 1.6 - Blocks, Blocks, Blocks

`base.html`:
```
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>{{ block "title" . }}
      <!-- Blocks may include default content. -->
      {{ .Site.Title }}
    {{ end }}</title>
  </head>
  <body>
    <!-- Code that all your templates share, like a header -->

    {{ block "main" . }}
      <!-- The part of the page that begins to differ between templates -->
    {{ end }}

    <!-- More shared code, perhaps a footer -->
  </body>
</html>
```

`list.html`:
```
<!-- Note the lack of Go's context "dot" when defining blocks -->
{{ define "main" }}
  <h1>Posts</h1>
  {{ range .Data.Pages }}
    <article>
      <h2>{{ .Title }}</h2>
      {{ .Content }}
    </article>
  {{ end }}
{{ end }}
```

`single.html`:
```
{{ define "title" }}
  {{ .Title }} &ndash; {{ .Site.Title }}
{{ end }}
{{ define "main" }}
  <h1>{{ .Title }}</h1>
  {{ .Content }}
{{ end }}
```



## HTML Shortcodes

Usage:
```
{{< youtube 09jf3ow9jfw >}}
```

"Macro":
```
<div class="embed video-player">
<iframe class="youtube-player" type="text/html" width="640" height="385"
        src="http://www.youtube.com/embed/{{ index .Params 0 }}"
        allowfullscreen frameborder="0">
</iframe>
</div>
```

becomes

```
<div class="embed video-player">
<iframe class="youtube-player" type="text/html" width="640" height="385"
        src="http://www.youtube.com/embed/09jf3ow9jfw"
        allowfullscreen frameborder="0">
</iframe>
</div>
```


##  HTML Shortcodes  - Figure (Image w/ Caption)

```
<figure {{ with .Get "class" }}class="{{.}}"{{ end }}>
    {{ with .Get "link"}}<a href="{{.}}">{{ end }}
        <img src="{{ .Get "src" }}" {{ if or (.Get "alt") (.Get "caption") }}alt="{{ with .Get "alt"}}{{.}}{{else}}{{ .Get "caption" }}{{ end }}"{{ end }} />
    {{ if .Get "link"}}</a>{{ end }}
    {{ if or (or (.Get "title") (.Get "caption")) (.Get "attr")}}
    <figcaption>{{ if isset .Params "title" }}
        <h4>{{ .Get "title" }}</h4>{{ end }}
        {{ if or (.Get "caption") (.Get "attr")}}<p>
        {{ .Get "caption" }}
        {{ with .Get "attrlink"}}<a href="{{.}}"> {{ end }}
            {{ .Get "attr" }}
        {{ if .Get "attrlink"}}</a> {{ end }}
        </p> {{ end }}
    </figcaption>
    {{ end }}
</figure>
```




## HTML Template Options in Go - Alternatives

Inspired by Haml, Slim, Jade and Friends

- **Amber** [add star here] 612 (github: [eknkc/amber](https://github.com/eknkc/amber)) by Ekin Koc et al
- **Ace** [add star here] 474 (github: [yosssi/ace](https://github.com/yosssi/ace)) by Keiji Yoshida et al

```
html
    head
        title Page Title
    body
        div#content
            p
                | This is a long page content
                | These lines are all part of the parent p

                a[href="/"] Go To Main Page
```



# Hugo Stay Static Site - Configuration / Settings (Cont.)

**Hugo** - TOML

~~~
title: 'Jekyll Stay Static Sample Site'

path:  '/sites/jekyll'
url:   'http://staystatic.github.io/sites/jekyll'

markdown: kramdown

exclude:
- README.md
~~~

(Source: [`staystatic/jekyll/_config.yml`](https://github.com/staystatic/jekyll/blob/master/_config.yml))


# Hugo -  Summary

|  -                       | Hugo         |
| ------------------------ | ------------ |
| GitHub Stars (+1s)       | ★5 026       |
|  -                       |  -           |
| Settings / Configuration | TOML         |
| HTML Templates           | Go Templates |
| . Layouts                | Yes          |
| . Includes               | Yes          |
| Front Matter / Meta Data | TOML         |
| Datafiles                | TOML         |
| CSS Preprocessing        | No (*)       |
| HTML "Shortcodes"        | Markdown     |

(*) Use "External" Pipeline e.g. Preprocessor in JavaScript etc.


# Hugo Stay Static Site Demo

```
$ hugo build
```

results in

```
```


## Going Live - Free (Static) Site Hosting Options

- GitHub Pages      - use git push  
- GitLab Pages      - use git push
- Google Firebase (Free Tier)   
- Surge.sh  - Go live with six keystrokes - s u r g e [ENTER]



## Options for Adding Comments

- Disqus
- Facebook Comments
- Google Plus Comments
- GitHub Issues
- Comment on Hacker News
- etc.

Do-it-yourself (DIY) "Server-less" Example - [Lambda Comments](https://jimpick.com/2016/05/05/introducing-lambda-comments/) by Jim Pick



## Why Static? - Static is the New Dynamic

- Fast, Faster, Fastest

- Simple, Simpler, Simplest

- Pretty, Prettier, Prettiest
    - e.g. designer nirvana - do-it-yourself - full control over your design; use Bootstrap, Material, or what not.

Bonus: Secure e.g. just a bunch of (static) files on your server.



## Why Static? - Static is the New Dynamic (Cont.)

Some Articles:

- [**Why Static Website Generators Are The Next Big Thing**](https://www.smashingmagazine.com/2015/11/modern-static-website-generators-next-big-thing) by Mathias Biilmann Christensen, Nov 2015; Smashing Magazine
- [**Seven Reasons to Use a Static Site Generator**](https://www.sitepoint.com/7-reasons-use-static-site-generator) by Craig Buckler, March 2016; Site Point
- [**Nine Reasons Your Site Should Be Static**](https://www.netlify.com/blog/2016/05/18/9-reasons-your-site-should-be-static) by Aaron Autrand, May 2016; Netlify
- [**Five Bullshit Reasons Not to Use a Static Generator**](https://www.netlify.com/blog/2016/05/24/5-bullst-reasons-not-to-use-a-static-generator)
by Aaron Autrand, May 2016; Netlify
  - I want good SEO!
  - Updating content is too hard! I can't use a CMS!
  - There's no way for users to interact with my content!
  - There are too many choices!
  - It takes too long to set up!  



## Best of "Both Worlds"

Example: Let's use WordPress and Hugo. How?

[**WordPress <--> GitHub Sync**](https://github.com/mAAdhaTTah/wordpress-github-sync) by James DiGioia, Ben Balter et al

A WordPress plugin to sync content with a GitHub repository.


[**Wordpress to Hugo Exporter**](https://github.com/SchumacherFM/wordpress-to-hugo-exporter) by Cyrill Schumacher et al

One-click WordPress plugin that converts all posts, pages, taxonomies, metadata,
and settings to Markdown and YAML which can be dropped into Hugo.


[**Hugo Sync**](https://github.com/hiproz/hugo-sync)

Synchronize your WordPress or GitHub to Hugo website automatically

And some more.



# Thanks - Stay Static

**Stay Static Sample Sites (Showcase)**

- [Stay Static](http://staystatic.github.io)
  - [`/middleman`](https://github.com/staystatic/middleman)
  - [`/jekyll`](https://github.com/staystatic/jekyll)

**Static Times**  News Channel

**Vienna.html**  Static Site Meetups




# Appendix

## Spread the JAM! - A New TLA (Three-Letter Acronym)

Rebranding "Static"? Why not?

What's the JAM Stack?

- **J**avaScript
- **A**PIs and
- **M**arkup.

More [`jamstack.org`](http://jamstack.org)



## Static Site Builders / Generators

StaticGen.com

![](i/site-staticgen-com.png)


## What's YAML? What's TOML?

YAML (YAML Ain't Markup Language) - <http://yaml.org>

TOML (Tom's Obvious, Minimal Language) - <https://github.com/toml-lang/toml>



## What's Markdown? What's Markup?

Add link to Use Markdown for Websites, Books, Presentation etc. talk here



## Vienna.html - Join Us - No Database Required

Next meetup (last before the summer)

Add data and talks here
