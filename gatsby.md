title: Stay Static - Build Single-Page Static (Web)Sites w/ the Great Gatsby


# Agenda

- Hello, Gatsby!
- Gatsby Stay Static Sample Site - Posts, Pages, Datafiles
- Markdown Madness - Markdown Extensions n Goodies
- Universal ("Isomorphic") HTML Components (Templates) in React w/ JSX
- Inside Gatsby - Building Blocks - React, Webpack, and Friends
- Demo - Go Live - Free (Static) Site Hosting Options
- Why Static?
- Thanks - Stay Static



# Dynamic (Web)Site Generators

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



# Static (Web)Site Generators / Builders

On your live production site requires

- web server (e.g. apache, ngnix, etc.) only

- **NO** database needed
- **NO** application server needed

Note: You can even go "server-less" e.g.
host your site on a web service e.g. Amazon S3 (Simple Storage Service).

You build the complete site, that is, **all pages** "ahead-of-time"
on a "build" machine. You will end-up with a bunch of (static) ready-to-use
HTML, CSS and JS files (*). Upload to production site and you're live
w/ a kind of "super cache".

(*) and media files e.g. graphics, music recordings, etc.


# Static (Web)Site Generators / Builders

**The Biggies** in 1999

1. Macromedia Dreamweaver
2. Microsoft FrontPage
3. Netscape Composer

And today?



# Hello, Gatsby!

by Kyle Mathews et al (★3 071) -
github: [`gatsbyjs/gatsby`](https://github.com/gatsbyjs/gatsby)

![](i/staticgen-gatsby.png)


# How Did It All Get Started?

Why not build (another) blog w/ React.js in 5 minutes?
The world's 1st Gatsby site?

**Bricolage** (web: [bricolage.io](https://bricolage.io), github: [KyleAMathews/blog](https://github.com/KyleAMathews/blog)) - a blog written
 by **Kyle Mathews** who lives and works in San Francisco building useful things.

![](i/bricolage.png)


# Static in the "Real World"

- Books
- Magazines
- Newspapers
- etc.

### Trivia Quiz

Q: The Great Gatsby by ____ ?

- [ A ] Robert Louis Stevenson
- [ B ] Francis Scott Fitzgerald
- [ C ] Stephen Edwin King
- [ D ] Kyle A Mathews

Q: Last Update In (Static Since) ______ ?

- [ A ] 1854
- [ B ] 1886
- [ C ] 1925
- [ D ] 2015



# Getting Started w/ Gatsby

Gatsby is JavaScript package using Webpack, React, React-Router,
Markdown (w/ ) and more to let you build static (web)sites.
Use npm to install e.g.:

```
$ npm install -g gatsby
```


# Getsby Commands

Try:

```
$ gatsby -h
```

prints

```
$ gatsby -h

Usage:  
  gatsby [command] [options]

Available Commands:
  new [rootPath] [starter]  Create new Gatsby project.
  develop [options]         Start development server. Watches files and rebuilds and hot reloads
                              if something changes
  build [options]           Build a Gatsby project.
  serve-build [options]     Serve built site.

Options:
  -h, --help     output usage information
  -V, --version  output the version number
```

See the [Gatsby Quick Reference (Cheat Sheet)](https://github.com/statictimes/quickrefs/blob/master/GATSBY.md)




# Gatsby Quick Starter - Ready-to-Use/Fork Themes

- Simple blog
- Simple documentation site
- Kitchen sink demo site (default)

To get started use:

```
$ gatsby new blog https://github.com/gatsbyjs/gatsby-starter-blog
```

Basically the same as:

```
$ git clone https://github.com/gatsbyjs/gatsby-starter-blog
$ cd gatsby-starter-blog
$ npm install
```

To test drive use:

```
$ gatsby develop  
```

And open the browser. Voila.

