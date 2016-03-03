title: Stay Static - Jekyll vs Middleman -  Build Static (Web) Sites w/ Ruby 


# Agenda

- Hello, Middleman
- Hello, Jekyll
- Middleman or Jekyll? - HTML Templates
- Middleman or Jekyll? - Pages n Posts with Front Matter
- Middleman or Jekyll? - Datafiles 
- Middleman or Jekyll? - HTML Templates - Loops
- Middleman or Jekyll? - Configuration / Settings
- Middleman or Jekyll? - Summary
- More Static Site Builders (in Ruby)
- And the Winner is...



# Hello, Middleman

by Thomas Reynolds et al (★5 026 / 974 690 Downloads) -
web: [`middlemanapp.com`](https://middlemanapp.com),
github: [`middleman/middleman`](https://github.com/middleman/middleman),
gem: [`middleman`](https://rubygems.org/gems/middleman)

![](i/staticgen-middleman.png)

Static Site Spotlight: 

[ROSSConf](https://github.com/rossconf/rossconf.io) •
[EuRuKo 2016](https://github.com/euruko/euruko2016.github.io) •
[Sass Language](https://github.com/sass/sass-site) •
[Stay Static](https://github.com/staystatic/middleman) •
[Many More](https://middlemanapp.com/community/built_using_middleman)



# Hello, Jekyll

by Tom Preston-Werner, Nick Quaranto,
Parker Moore, Jordon Bedwell, Matt Rogers et al (★24 142 / 2 044 522 Downloads) -
web: [`jekyllrb.com`](http://jekyllrb.com),
github: [`jekyll/jekyll`](https://github.com/jekyll/jekyll),
gem: [`jekyll`](https://rubygems.org/gems/jekyll)

![](i/staticgen-jekyll.png)

Static Site Spotlight:

[Vienna.rb](https://github.com/vienna-rb/vienna-rb.github.com) •
[Vienna.html](https://github.com/viennahtml/viennahtml.github.io) •
[Travis Foundation](https://github.com/travis-ci/travis-foundation) •
[Hyde Press](https://github.com/hydepress/hydepress.github.io) •
[Facebook React](https://github.com/facebook/react/tree/master/docs) •
[Bootstrap](https://github.com/twbs/bootstrap/tree/master/docs) •
[Stack Overflow Blog](https://github.com/StackExchange/stack-blog) •
[PHP: The Right Way](https://github.com/codeguy/php-the-right-way) •
[Open Data Handbook v2](https://github.com/okfn/opendatahandbook) •
[Stay Static](https://github.com/staystatic/jekyll) •
[Many More](https://github.com/jekyll/jekyll/wiki/Sites)
[And More](http://planetjekyll.github.io/showcase)



# Middleman or Jekyll?  - HTML Templates

**Middleman** - Embedded Ruby (ERB) Template Language

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


# Middleman or Jekyll?  - HTML Templates (Cont.)

**Jekyll** - Liquid Template Language

~~~
<!DOCTYPE html>
<html>
  {%% include head.html %}
  </head>
  <body>
    {%% include header.html %}
    <div class="main">
      {{{ content }}
    </div>   
    {%% include footer.html %}
  </body>
</html>
~~~

(Source: [`staystatic/jekyll/_layouts/default.html`](https://github.com/staystatic/jekyll/blob/master/_layouts/default.html))



# Middleman or Jekyll? - Pages n Posts with Front Matter


**Middleman** - YAML + Markdown

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



# Middleman or Jekyll? - Pages n Posts with Front Matter

**Jekyll** - YAML + Markdown

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

(Source: [`staystatic/jekyll/_posts/2014-11-11-new-repo-maps.md`](https://github.com/staystatic/jekyll/blob/master/_posts/2014-11-11-new-repo-maps.md))



# Middleman or Jekyll? - Datafiles 

**Middleman** - YAML

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



# Middleman or Jekyll? - Datafiles 

**Jekyll** - YAML

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

(Source: [`staystatic/jekyll/_data/links.yml`](https://github.com/staystatic/jekyll/blob/master/_data/links.yml))


# Middleman or Jekyll?  - HTML Templates - Loops

**Middleman** - Embedded Ruby (ERB) Template Language

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



# Middleman or Jekyll?  - HTML Templates - Loops (Cont.)

**Jekyll** - Liquid Template Language

~~~
<div>
  <b>Links 'n' Bookmarks</b>
  <ul class="links">
    {%% for link in site.data.links %}
      <li><a href="{{{link.url}}">{{{ link.title }}</a></li>
    {%% endfor %}
  </ul>
</div>
~~~

~~~
<div>
  <b>News 'n' Updates</b>
  <ul class="news">
   {%% for post in site.posts %}
    <li><a href="{{{site.path}}{{{post.url}}">{{{ post.title }}</a></li>
   {%% endfor %}
  </ul>
</div>
~~~

(Source: [`staystatic/jekyll/index.html`](https://github.com/staystatic/jekyll/blob/master/index.html))



# Middleman or Jekyll? - Configuration / Settings

**Middleman** - Ruby

~~~
source 'https://rubygems.org'

# Middleman Gems
gem 'middleman',      '>= 4.0.0'
gem 'middleman-blog'
~~~

(Source: [`staystatic/middleman/Gemfile`](https://github.com/staystatic/middleman/blob/master/Gemfile))

~~~
activate :blog do |blog|
  blog.permalink = '/news/{title}.html'
  blog.sources = 'posts/{year}-{month}-{day}-{title}.html'
end

helpers do
  def site_title
    'Middleman Stay Static Sample Site'
  end

  def page_title
    current_page.data.title ? current_page.data.title : nil
  end
end

configure :build do
  set :http_prefix, '/sites/middleman'
end
~~~

(Source: [`staystatic/middleman/config.rb`](https://github.com/staystatic/middleman/blob/master/config.rb))



# Middleman or Jekyll? - Configuration / Settings (Cont.)

**Jekyll** - YAML

~~~
title: 'Jekyll Stay Static Sample Site'

path:  '/sites/jekyll'
url:   'http://staystatic.github.io/sites/jekyll'

markdown: kramdown

exclude:
- README.md
~~~

(Source: [`staystatic/jekyll/_config.yml`](https://github.com/staystatic/jekyll/blob/master/_config.yml))



# Middleman or Jekyll? -  Summary

|  -                       | Middleman  | Jekyll    |
| ------------------------ | ---------- | --------- |
| GitHub Stars (+1s)       | ★5 026    | ★24 142  |
| Gem Downloads            | 974 690    | 2 044 522 |
|  -                       |  -         |  -        |
| Settings / Configuration | Ruby       | YAML      |
| HTML Templates           | Ruby (ERB) | Liquid    |
| . Layouts                | Yes        | Yes       |
| . Includes               | Yes        | Yes       |
| Front Matter / Meta Data | YAML       | YAML      |
| Datafiles                | YAML       | YAML      |
| CSS Preprocessing        | Sass       | Sass      |
| HTML "Shortcodes"        | Markdown   | Markdown  |



# More Static Site Builders (in Ruby)

- [**Nanoc**](https://github.com/nanoc/nanoc) by Denis Defreyne et al (★1 283 / 223 529 Downloads)
- [**Awestruct**](https://github.com/awestruct/awestruct) by Bob McWhirter et al (★221 / 132 949 Downloads) 
- [**webgen**](https://github.com/gettalong/webgen) by Thomas Leitner et al (★90 / 88 059 Downloads)
- [**Bonsai**](https://github.com/benschwarz/bonsai) by Ben Schwarz et al (★275 / 54 502 Downloads)
- [**Ruhoh**](https://github.com/ruhoh/ruhoh.rb) by Jade Dominguez et al (★623 / 24 891 Downloads) - _uses Mustache templates_
- [**ZenWeb**](https://github.com/seattlerb/zenweb) by Ryan Davis et al (★52 / 18 083 Downloads)
- and many more

Note: Sorted by Downloads




# And the Winner is...

Use what works for you.




# Links, Links, Links - Static Site News, Events 'n' More

**Stay Static Sample Sites (Showcase)**

- [Stay Static](http://staystatic.github.io)
  - [`/middleman`](https://github.com/staystatic/middleman)
  - [`/jekyll`](https://github.com/staystatic/jekyll)

**Articles**

- [Static Blogging Tool Face-Off: Middleman vs Jekyll](http://www.sitepoint.com/static-blogging-g-face-middleman-vs-jekyll) by David Turnbull; November 2015; SitePoint

**News**

- [Static Times News @ Twitter](https://twitter.com/statictimes)
- [{static is} The New Dynamic](http://www.thenewdynamic.org)
  - [Middleman](http://www.thenewdynamic.org/tool/middleman)
  - [Jekyll](http://www.thenewdynamic.org/tool/jekyll)

**Events**

- [Vienna.html Meetup](http://viennahtml.github.io) - Next Meetup April 2016 @ sektor5 - Vienna, Austria
- [Static Web Tech Meetup](http://www.staticwebtech.com) - @ San Francisco, California
- [{static is} The New Dynamic Meetup](http://www.meetup.com/The-New-Dynamic) - @ New York City, New York



# Bonus: Many More Static Site Builder / Generators

**Q**: What about JavaScript, Python, PHP, Hugo, Haskell, Rust, C, Swift, Lisp, Bash, _[Your Language Here]_, etc.?

**A**: See the Static Site Builder / Generator Directories:

- [`staticgen.com`](http://www.staticgen.com)
- [`staticsitegenerators.net`](https://staticsitegenerators.net)
- [Static Site Generators @ `static-revival.com`](https://www.static-revival.com/static-site-generators)


