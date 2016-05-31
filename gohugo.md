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

- web server (e.g. apache) only

- **NO** database needed
- **NO** application server needed

Note: You can even go "server-less" e.g.
host your site on a web service e.g. Amazon S3 (Simple Storage Service).

You build the **complete** site, that is, **all pages** "ahead-of-time"
on a "build" machine. You will end-up with a bunch of (static) ready-to-use
HTML, CSS and JS files.  Upload to production site and you're live
w/ a kind of "super cache".


## Static (Web)Site Generators / Builders

**The Biggies**

1. ?
2. ?
3. ?


## Hello, Hugo!

by Steve Francia et al (★10 039) -
web: [`gohugo.io`](https://gohugo.io),
github: [`spf13/hugo`](https://github.com/spf13/hugo)

[add pic here]  -- check staticgen if already uploaded/included




## Getting Started w/ Hugo

Download the single-file Hugo binary. That's it.

Try:

```
$ hugo
```

prints

```
```


# Hugo Stay Static Sample Site

Shows how-to-use:

- Posts
- Pages
- Datafiles

[add pic here]


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




## Go Template Language




## Go Template Language Alternatives








## Case Study - Adding Comments - Jim Pick's Blog (A Developer in Vancouver)

A Hugo blog

show pic here [https://jimpick.com/2016/05/05/introducing-lambda-comments/]


## Options for Adding Comments

- Disqus
- Facebook Comments
- Google Plus Comments
- GitHub Issues
- Comment on Hacker News
- etc.

Do-it-yourself (DIY) "Server-less"


## What's Lambda Comments?











## Static is the New Dynamic - Why Static?

- Fast, Faster, Fastest

- Simple, Simpler, Simplest

- Pretty, Prettier, Prettiest
    - e.g. designer nirvana - do-it-yourself - full control over your design; use Bootstrap, Material, or what not.

Bonus: Secure e.g. just a bunch of (static) files on your server.


## Static is the New Dynamic - Why Static? (Cont.)

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



## What's TOML? What's YAML?



## What's Markdown? What's Markup?

Markup.  (add up arrow) Markdown.  (add down arrow)

- add markdown intro here



## Vienna.html - Join Us - No Database Required

Next meetup (last before the summer)
