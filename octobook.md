title: Build (Online) Books and Documentation w/ Octobook Themes and the Jekyll (Static) Website Compiler


# Agenda

- What's a Jekyll Theme?
- What's Dr Jekyll's?
- Jekyll Themes 'n' GitHub Pages
- What's the Jekyll Remote Theme Plugin
  - Jekyll Remote Theme In Action Example - Ruby Habits
- What's Octobook?
  - Three Octobook Real-World Examples
    1. World Classics  
    2. Project Documentation
    3. Yuki & Moto Press Bookshelf


# What's a Jekyll Theme?

A Jekyll theme is a bundle of ready-to-use (ready-to-fork)
layout templates, design styles,
include building blocks and more
for your next Jekyll website.
See <https://jekyllrb.com/docs/themes>.

Examples:

- Minima (Default Starter Theme)
- Primer (Default GitHub Pages Theme)
- Academic Pages
- Henry's Starter
- Henry's Bootstrap
- Zeppelin (Conference Event Theme)
- and many many more


# What's Dr Jekyll's?

Dr Jekyll's Themes :-). 200+ free, open source
ready-to-use (ready-to-fork) themes.

See <https://drjekyllthemes.github.io>.


# Jekyll Themes 'n' GitHub Pages

Only 12+2 official white-listed (gem-packaged) themes
supported with "(auto-)magic" GitHub Pages build pipeline.
See <https://pages.github.com/themes>,
<https://pages.github.com/versions>
and <https://github.com/pages-themes>.

- Minima (Default Starter Theme)
- Primer (Default GitHub Pages Theme)

Architect  • Cayman    • Dinky  • Hacker   • Leap day  •
Merlot     • Midnight  • Minima • Minimal  • Modernist  •
Slate      • Tactile   • Time machine



# What's the Jekyll Remote Theme Plugin?

Yes! Use any GitHub repo as a Jekyll theme!
See <https://github.com/benbalter/jekyll-remote-theme>.

Usage:

Step 1 - Add the plugin to your config(uration)

```
plugins:
- jekyll-remote-theme
```

Step 2 - Add the github repo

```
remote_theme: benbalter/retlab
```

That's it!



# Jekyll Remote Theme In Action Example - Ruby Habits

Ruby Habits - see <https://github.com/RubyHabits/rubyhabits.github.io>
and live <https://rubyhabits.github.io/>.

Uses Minimal Mistakes by Michael Rose

How does it work?

[`_config.yml`](https://github.com/RubyHabits/rubyhabits.github.io/blob/master/_config.yml):

```
plugins:
- jekyll-remote-theme

remote_theme: mmistakes/minimal-mistakes
```


# What's Octobook?

Ready-to-use  - surprise, surprise - book themes.
See <https://github.com/octobook>.

Examples:

- octobook/book-classics-theme
- octobook/book-2017-theme
- octobook/book-2018-theme


Note: Book == (user) manual == (project) documentation ...



# Three Octobook Real-World Examples - 1) World Classics

1) World Classics - Strange Case of Dr Jekyll and Mr Hyde by Robert Louis Stevenson (1886), see <http://worldclassics.github.io/dr-jekyll-and-mr-hyde>.

[`_config.yml`](https://github.com/worldclassics/worldclassics.github.io/blob/master/_config.yml):

```
plugins:
- jekyll-remote-theme

remote_theme: octobook/book-classics-theme
```


# Three Octobook Real-World Examples - 1) World Classics (Cont.)

Step 1: Add (structured) text files with formatting in markdown - kramdown, really ;-)

```
dr-jekyll-and-mr-hyde/
  01.md     
  02.md     
  03.md
  04.md
  05.md
  ...
```

Step 2: Add [`book.yml`](https://github.com/worldclassics/worldclassics.github.io/blob/master/_data/dr_jekyll_and_mr_hyde/book.yml) - Book (Meta) Info and (Table of) Contents

```
title:  Strange Case of Dr. Jekyll and Mr. Hyde
year:   1886
author:
  name: Robert Louis Stevenson

contents:
- title: Story of the Door
  path:  01.md
- title: Search for Mr. Hyde
  path:  02.md
- title: Dr. Jekyll was Quite at Ease
  path:  03.md
  ...
```

Step 3: Add book page - [`dr-jekyll-and-mr-hyde.html`](https://github.com/worldclassics/worldclassics.github.io/blob/master/dr-jekyll-and-mr-hyde.html)

```
---
layout: book_classics
book:   dr_jekyll_and_mr_hyde   ## book id used for site.data[ page.book ] lookups
---
```

That's it.




# Three Octobook Real-World Examples - 2) Project Documentation

2) Project Documentation  

- Slideshow (S9) - Write Your Slides in Plain Text w/ Markdown Formatting Conventions, see <http://slideshow-s9.github.io>.
- Pluto - Planet (Static) Website Generator - Auto-Build Web Pages From Published Web Feeds, see <http://feedreader.github.io>.

[`_config.yml`](https://github.com/slideshow-s9/slideshow-s9.github.io/blob/master/_config.yml):

```
plugins:
- jekyll-remote-theme

remote_theme: octobook/book-2018-theme
```


# Three Octobook Real-World Examples - 2) Project Documentation (Cont.)

Step 1: Add (structured) text files with formatting in markdown - kramdown, really ;-)

```
slideshow-s9/docs/
  index.md
  more.md
  plugins.md
  code.md
  gallery.md
  themes.md
  ---
```

Step 2: Add [`book.yml`](https://github.com/slideshow-s9/slideshow-s9.github.io/blob/master/_data/slideshow/book.yml) - Book (Meta) Info and (Table of) Contents

```
title:    Slide Show (S9) Guide (Book Edition)
subtitle: Write Your Slides in Plain Text w/ Markdown Formatting Conventions
author:
  name:   Gerald Bauer, et al

contents:
- title: 1. What's Slide Show (S9)?
  path:  index.md
  sections:
  - title: What's Slide Show (S9)?
  - title: Getting Started in 1-2-3 Easy Steps


- title: 2. Settings, Tips, Tricks and More
  path:  more.md
  sections:
  - title: How To Fetch New Template Packages?
  - title: How To Fetch New Template Packs Using git?
  - title: How To List All Installed Template Packages?
  ...
```

Step 3: Add book page - [`index.html`](https://github.com/slideshow-s9/slideshow-s9.github.io/blob/master/index.html)

```
---
layout: book
book:   slideshow      ## book id used for site.data[ page.book ] lookups
---
```

That's it.



# Three Octobook Real-World Examples - 3) Bookshelf

3) Yuki & Moto Press Bookshelf - Free (Online) Books about Ruby & Friends, see <http://yukimotopress.github.io>.

Titles include:

- Programming Cryptocurrencies and Blockchains
- FizzBuzz (1, 2, Fizz, 4, Buzz,...) by Example - There's More Than One Way To Do It
- Hoe Developer's Guide - Build, Package and Publish Gems with Rake Tasks - Ready-to-Use Build Scripts
- Gem Series ++ Project Automation & Database Documentation Tools
- Gem Series ++ Web Services (HTTP JSON APIs) the Modern Micro Way
- and more

[`_config.yml`](https://github.com/yukimotopress/yukimotopress.github.io/blob/master/_config.yml):

```
plugins:
- jekyll-remote-theme

remote_theme: octobook/book-2018-theme
```
