title: Building Static (Web)sites with Jigsaw and Laravel Blade (in PHP)


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

# Case Study: PHP - The Right Way

**PHP - The Right Way**
(web: [phptherightway.com](http://www.phptherightway.com),
github: [codeguy/php-the-right-way](https://github.com/codeguy/php-the-right-way))

Q: Built with _____?

- [ ] PHP
- [ ] Perl
- [ ] Python
- [ ] Ruby
- [ ] JavaScript (Node.js)

![](i/php-the-right-way.png)



# Case Study: PHP - The Wrong Way

**PHP - The Wrong Way**
(web: [phpthewrongway.com](http://www.phpthewrongway.com),
 github: [binarygenius/phpthewrongway](https://github.com/binarygenius/phpthewrongway))

Q: Built with _____?

- [ ] Lisp
- [ ] OCaml
- [ ] Haskell
- [ ] Reason
- [ ] Elm

![](i/php-the-wrong-way.png)


# Case Study Answers

PHP - The Right Way is built with...  Ruby (and Jekyll).

**Jekyll** ★29 707 ([jekyllrb.com](http://jekyllrb.com)) - a popular static (web)site builder -
it's built into GitHub and includes free hosting and more.

PHP - The Wrong Way is built with... Haskell (and Pandoc).

**Pandoc** ★8 687 (web: [pandoc.org](http://pandoc.org)) - a popular plain text (e.g. markdown)
to hypertext (e.g. markup) converter
includes templating too - thus, it's another (great) static (web)site builder.


Q: Any static (web)site builders in PHP?



# Hello, Jigsaw!

by Adam Wathan, Keith Damiani et al (★624) -
web: [jigsaw.tighten.co](http://jigsaw.tighten.co),
github: [`tightenco/jigsaw`](https://github.com/tightenco/jigsaw)

![](i/jigsaw-new-site.png)



# Why Jigsaw? Why (yet) another static site builder?

Build for Laravel (PHP) developers!

Started in April 2016 -
see
["Introducing Jigsaw, a Static Site Generator for Laravel Developers"](https://blog.tighten.co/introducing-jigsaw-a-static-site-generator-for-laravel-developers)
by Matt Stauffer

Q: Do you use (know) Laravel's Blade templating language?

A: Yes. Good news. Now use what you know (and PHP)
to build static (web)sites too.


# Why Blade? Why (yet) another PHP template language?

Plain PHP Templates - Isn't PHP the (best) template language?
Does PHP need a (new) template language?

Why Blade?

- Compiled
- "Simpler Syntax"
  - Built-in Template Inheritance
  - (Content) Blocks
  - "Language-Independent" e.g. looks (more) like other popular template languages
  - "Designer-Friendly"

```
@extends('_layouts.master')

@section('body')
    <h1>Hello World!</h1>
@endsection
```


# Getting Started with Jigsaw

Step 1: Install Jigsaw (using Composer)

```
$ composer global require tightenco/jigsaw
```

Step 2: Try it

```
$ jigsaw
```

prints

```
Jigsaw version 1.0.1

Usage:
  command [options] [arguments]

Options:
  -h, --help            Display this help message
  -q, --quiet           Do not output any message
  -V, --version         Display this application version
      --ansi            Force ANSI output
      --no-ansi         Disable ANSI output
  -n, --no-interaction  Do not ask any interactive question
  -v|vv|vvv, --verbose  Increase the verbosity of messages:
                          1 for normal output,
                          2 for more verbose output and
                          3 for debug

Available commands:
  build  Build your site.
  help   Displays help for a command
  init   Scaffold a new Jigsaw project.
  list   Lists commands
```



# Getting Started with Jigsaw Cont. - Commands

- **Init Command**  - Scaffold a new Jigsaw project
- **Build Command** -  Build your site

Try it:

```
$ jigsaw init mysite
```

will produce

```
│   config.php
│   gulpfile.js
│   package.json
└───source/
    │   index.blade.php
    ├───css/
    │       main.css
    ├───_assets/
    │   └───sass/
    │           main.scss
    └───_layouts/
            master.blade.php
```


# Getting Started with Jigsaw Cont. - Commands

```
$ cd mysite
$ jigsaw build
  Site built successfully!
```

will result into:

```
build_local/
│   index.html
└───css/
       main.css
```


# Layouts - Example: layouts/master.blade.php

```
<!DOCTYPE html>
<html>
    <head>
        <title>Your Site Title</title>
    </head>
    <body>
        <header>
            Static Site Sample
        </header>

        @yield('contents')

        <footer>
            <p>Built w/ Jigsaw</p>
        </footer>
    </body>
</html>
```

# Jigsaw Stay Static Site - HTML Templates - Includes

```
<!DOCTYPE html>
<html>
  <head>
     ...
  </head>
  <body>
    @include('_includes.github')
    @include('_includes.header')
    <div class="main">
       @yield('body')
    </div>
    @include('_includes.footer')
  </body>
</html>

``` 

(Source: [staystatic/jigsaw/source/_layouts/master.blade.php](https://github.com/staystatic/staystatic/blob/master/jigsaw/source/_layouts/master.blade.php))




# Pages in HTML - Example: index.blade.php

```
@extends('_layouts.master')

@section('body')
<h1>Hello world!</h1>
@endsection
```


# Pages in Markdown - Example: about.md

Use `.markdown` or `.md` extension
and start the page with a front matter block (in YAML).


```
---
extends: _layouts.master
section: body
---

Hello, Jigsaw!

Welcome to markdown madness. We hope you **really** enjoy using good old text for writing.

Just type some [markdown](http://en.wikipedia.org/wiki/Markdown)
and Jigsaw (w/ ) will automatically turn it into hypertext markup language (HTML).
*Simple as that.*

> Quote goes here.

A list:

- One
- Two
- Three

Some inline code `to_html` and a preformatted code block:


    <?php echo 'Hello, World!'; ?>


Or try

# Heading 1

## Heading 2

### Heading 3
```


# Markdown Madness - Parsedown  

Jigsaw uses Parsedown (out-of-the-box).

What's Parsdown?

Markdown Parser in PHP. One File  //  Super Fast  //  Extensible  //  GitHub Flavored


```
$Parsedown = new Parsedown();
echo $Parsedown->text('Hello _Parsedown_!');

```

See [`parsedown.org`](http://parsedown.org)



# Markdown Madness - Parsedown Cont.

Includes GitHub Flavored Markdown Extensions e.g.

- Fenced Code Blocks
- Tables
- and more

Includes Markdown Extra Extension e.g.

- Footnotes
- Definition Lists
- and more



# Pages in Markdown w/ Custom Front Matter - Example: layouts/page.blade.php + history.md

layouts/page.blade.php:

```
@extends('_layouts.master')

@section('body')
    <div class="page">
        <h2>{{ $page->title }}</h2>
    </div>
    @yield('page-body')
@endsection
```

history.md:

```
---
extends:   _layouts.page
section:   page-body
title:     The History of Jigsaw
---

Build for Laravel (PHP) developers!

Started in April 2016 (this year) -
see ["Introducing Jigsaw, a Static Site Generator for Laravel Developers"](https://blog.tighten.co/introducing-jigsaw-a-static-site-generator-for-laravel-developers)
by Matt Stauffer (the Laravel lead)
```


# Global site-wide variables - Example: config.php

Add global site-wide variables
to the `config.php`:

```
<?php

return [
    'site_title' => '"Your Site Title',
];
```

And use like (in templates):

```
<head>
  <title>{{ $page->site_title }}</title>
</head>
```


# Collections

Added to Jigsaw in 1.0 in April 2017. Yeah!


[Article: Supercharged Static Sites: Introducing Jigsaw Collections](https://blog.tighten.co/supercharged-static-sites-introducing-jigsaw-collections) by Keith Damiani (Tighten Co)


Add collections to `config.php`. Example:

```
<?php

return [
    'collections' => [
        'posts' => [
            'path' => 'blog/{date|Y-m-d}/{filename}',  // output path
            'sort' => 'date',         
        ],
        'people' => [
            'path' => 'people',
            'sort' => 'last_name',
        ],
    ],
];
```

# Collections Cont.


`posts` collects all document in `_posts` folder e.g.:


```
_posts/
   new-build-system.md
   new-repo-maps.md
   new-season.md
```

`people` collects all document in `_people` folder e.g.:


```
_people/
   adam-wathan.md
   dan-sheetz.md
   dave-hicking.md
   keith-damiani.md
   matt-stauffer.md
   
```


# Jigsaw Stay Static Site - HTML Templates - Loops 


```
<div>
  <b>News 'n' Updates</b>
  <ul class="news">
    @foreach($posts as $post)
        <li><a href="{{ $page->baseUrl }}{{ $post->getPath() }}">{{ $post->title }}</a></li>
    @endforeach
  </ul>
</div>
```

(Source: [staystatic/jigsaw/source/index.blade.php](https://github.com/staystatic/staystatic/blob/master/jigsaw/source/index.blade.php))



# Datafiles


Added assoc arrays to `config.php`. Example:

```
<?php
return [
    'links'  => [
       [ 'title' => "football.db - Open Football Data",
         'url'   => "https://github.com/openfootball",
       ],
       [ 'title' => "beer.db - Open Beer, Brewery 'n' Brewpub Data",
         'url'   => "https://github.com/openbeer",
       ],
       [ 'title' => "world.db - Open World Data",
         'url'   => "https://github.com/openmundi",
       ]
    ],
```

(Source: [staystatic/jigsaw/config.php](https://github.com/staystatic/staystatic/blob/master/jigsaw/config.php))



# Jigsaw Stay Static Site - HTML Templates - Loops


```
<div>
  <b>Links 'n' Bookmarks</b>
  <ul class="links">
    @foreach($page->links as $link)
      <li><a href="{{ $link->url }}">{{ $link->title }}</a></li>
    @endforeach
  </ul>
</div>
```

(Source: [staystatic/jigsaw/source/index.blade.php](https://github.com/staystatic/staystatic/blob/master/jigsaw/source/index.blade.php))



# Jigsaw - Summary

|  -                       | Jigsaw         |
| ------------------------ | ------------ |
| GitHub Stars (+1s)       | ★624     |
|  -                       |  -           |
| Settings / Configuration | PHP          |
| HTML Templates           | Blade        |
| . Layouts                | Yes          |
| . Includes               | Yes          |
| Front Matter / Meta Data | Yaml         |
| Datafiles                | PHP          |
| CSS Preprocessing        | No (*)       |
| HTML "Shortcodes"        | Markdown     |

(*) Use "External" Pipeline e.g. Preprocessor in JavaScript (Node.js) etc.



# Thanks - Stay Static

**Stay Static Sample Sites (Showcase)**

- [Stay Static](http://staystatic.github.io)
  - [`/jigsaw`](https://github.com/staystatic/staystatic/tree/master/jigsaw)
  - [`/hugo`](https://github.com/staystatic/staystatic/tree/master/hugo)
  - [`/jekyll`](https://github.com/staystatic/staystatic/tree/master/jekyll)
  - [`/middleman`](https://github.com/staystatic/staystatic/tree/master/middleman)
  - [`/metalsmith-handlebars`](https://github.com/staystatic/staystatic/tree/master/metalsmith-handlebars)
  - [`/metalsmith-nunjucks`](https://github.com/staystatic/staystatic/tree/master/metalsmith-nunjucks)  
  - [`/gatsby`](https://github.com/staystatic/staystatic/tree/master/gatsby)

And more.



# Bonus: More Jigsaw Sample Sites

- [Jigsaw Site](http://jigsaw.tighten.co) [(Source)](https://github.com/tightenco/jigsaw-site)
- [Jigsaw Collections Demo](https://tightenco.github.io/jigsaw-collections-demo) [(Source)](https://github.com/tightenco/jigsaw-collections-demo)


