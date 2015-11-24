title: The World's Greatest (Free) Static Site Builders / Generators 


# Agenda

The World's Greatest (Free) Static Site Builders / Generators 

-   I. Ruby       - Middleman or Jekyll?
-  II. Python     - Pelican or Nikola?
- III. JavaScript - Wintersmith or Metallsmith?
-  IV. Go         - Hugo?
-   V. PHP, Haskell, Lisp, Rust, C, etc.

And the Winner is...
    



# Part I. - Ruby, Ruby, Ruby

# Hello, Middleman

![](i/staticgen-middleman.png)

by Thomas Reynolds et al -
[★4 756 GitHub Stars](https://github.com/middleman/middleman),
[870 043 Gem Downloads](https://rubygems.org/gems/middleman)


Static Site Spotlight: 

[ROSSConf](https://github.com/rossconf/rossconf.io) •
[Sass Language](https://github.com/sass/sass-site) •
[Adventure Time! Middleman Sample](https://github.com/remotesynth/Static-Site-Samples/tree/master/middlemansite) •
[Many More](https://middlemanapp.com/community/built_using_middleman)



# Hello, Jekyll

![](i/staticgen-jekyll.png)

by Tom Preston-Werner, Nick Quaranto,
Parker Moore, Jordon Bedwell, Matt Rogers et al -
[★22 380 GitHub Stars](https://github.com/jekyll/jekyll),
[1 756 295 Gem Downloads](https://rubygems.org/gems/jekyll)

Static Site Spotlight:

[Vienna.rb](https://github.com/vienna-rb/vienna-rb.github.com) •
[Vienna.html](https://github.com/viennahtml/viennahtml.github.io) •
[Facebook React](https://github.com/facebook/react/tree/master/docs) •
[Bootstrap](https://github.com/twbs/bootstrap/tree/master/docs) •
[Stack Overflow Blog](https://github.com/StackExchange/stack-blog) •
[PHP: The Right Way](https://github.com/codeguy/php-the-right-way) •
[Open Data Handbook v2](https://github.com/okfn/opendatahandbook) •
[Adventure Time! Jekyll Sample](https://github.com/remotesynth/Static-Site-Samples/tree/master/jekyllsite) •
[Many More](https://github.com/jekyll/jekyll/wiki/Sites)
[And More](http://planetjekyll.github.io/showcase)



# Middleman or Jekyll?  - HTML Templates

**Middleman** - Embedded Ruby (ERB) Template Language

~~~
<!DOCTYPE html>
<html>
  <%= partial "partials/head" %>
  <body>
    <%= partial "partials/header" %>
    <div id="main-wrapper">
      <div class="container">
      <%= yield %>
      </div>
    </div>
    <%= partial "partials/footer" %>
  </body>
</html>
~~~

(Source: [`layouts/layout.erb`](https://github.com/remotesynth/Static-Site-Samples/blob/master/middlemansite/source/layouts/layout.erb))


# Middleman or Jekyll?  - HTML Templates (Cont.)

**Jekyll** - Liquid Template Language

~~~
<!DOCTYPE html>
<html>
  {% include head.html %}
  <body>
    {% include header.html %}
    <div id="main-wrapper">
      <div class="container">
      {{ content }}
      </div>
    </div>
    {% include footer.html %}
  </body>
</html>
~~~

(Source: [`_layouts/default.html`](https://github.com/remotesynth/Static-Site-Samples/blob/master/jekyllsite/_layouts/default.html))



# Middleman or Jekyll? - Post with Front Matter (Categories, etc.)


**Middleman** - YAML + Markdown

~~~
---
layout: post
title:  "Food Chain (Season 6)"
date:   2014-06-12 10:33:56
categories: season6 episodes
shortdesc: Finn and Jake learn about the food chain by becoming the food chain.
banner: /images/foodchain.jpg
---

The episode begins with candy children that have bodies resembling different
shapes sliding down a slide, shouting with excitement. The kids are viewed
from an horizontal angle in the next scene as they go through a playground-like structure.

<!--more-->

Finn and Jake are examining the 'Catapilla Family'.
After watching Princess Bubblegum teach the children about the Food Chain
...
~~~

(Source: [`posts/2014-06-12-season-6-food-chain.md`](https://github.com/remotesynth/Static-Site-Samples/blob/master/middlemansite/source/posts/2014-06-12-season-6-food-chain.markdown))



# Middleman or Jekyll? - Post with Front Matter (Categories, etc.)

**Jekyll** - YAML + Markdown

~~~
---
layout: post
title:  "Food Chain (Season 6)"
date:   2014-06-12 10:33:56
categories: season6 episodes
shortdesc: Finn and Jake learn about the food chain by becoming the food chain.
banner: /images/foodchain.jpg
---

The episode begins with candy children that have bodies resembling different
shapes sliding down a slide, shouting with excitement. The kids are viewed
from an horizontal angle in the next scene as they go through a playground-like structure.

<!--more-->

Finn and Jake are examining the 'Catapilla Family'.
After watching Princess Bubblegum teach the children about the Food Chain
...
~~~

(Source: [`_posts/2014-06-12-season-6-food-chain.md`](https://github.com/remotesynth/Static-Site-Samples/blob/master/jekyllsite/_posts/2014-06-12-season-6-food-chain.markdown))



# Middleman or Jekyll? - Configuration / Settings

**Middleman** - Ruby

~~~
set :css_dir, 'stylesheets'

set :js_dir, 'javascripts'

set :images_dir, 'images'

set :site_title, 'Adventure Time!'
set :banner, '/images/about.jpg'
set :description, 'Adventure Time is an American animated television series...'

activate :blog do |blog|
  blog.sources           = "posts/{year}-{month}-{day}-{title}.html"
  blog.summary_separator = "<!--more-->"
  blog.tag_template      = "tag.html"
  blog.calendar_template = "calendar.html"
end

page "/feed.xml", layout: false
~~~

(Source: [`config.rb`](https://github.com/remotesynth/Static-Site-Samples/blob/master/middlemansite/config.rb))


# Middleman or Jekyll? - Configuration / Settings (Cont.)

**Jekyll** - YAML

~~~
title: Adventure Time!
email: brian.rinaldi@example.com
banner: "/images/about.jpg"
description: > Adventure Time is an American animated television series...
  The series follows the adventures of Finn, a human boy, and his best friend...
  a dog with magical powers to change shape and grow and shrink at will...

baseurl: "" 
url:     "http://yourdomain.com" 

excerpt_separator: "<!--more-->"
~~~

(Source: [`_config.yml`](https://github.com/remotesynth/Static-Site-Samples/blob/master/jekyllsite/_config.yml))



# Middleman or Jekyll? -  Summary

                         | Middleman  | Jekyll
-------------------------| ---------- | ---------
GitHub Stars (+1s)       | ★4 756    | ★22 380
Gem Downloads            | 870 043    | 1 756 295
                         |
Settings / Configuration | Ruby       | YAML
HTML Templates           | Ruby (ERB) | Liquid
. Layouts                | Yes        | Yes
. Includes               | Yes        | Yes
Front Matter / Meta Data | YAML       | YAML
CSS Preprocessing        | Sass       | Sass
HTML "Shortcodes"        | Markdown   | Markdown



# Middleman or Jekyll?  More Static Site Builders (in Ruby)

- [**Nanoc**](https://github.com/nanoc/nanoc) by Denis Defreyne et al (★1 225)
- [**Ruhoh**](https://github.com/ruhoh/ruhoh.rb) by Jade Dominguez et al (★611)
- [**Bonsai**](https://github.com/benschwarz/bonsai) by Ben Schwarz et al (★269)
- [**Awestruct**](https://github.com/awestruct/awestruct) by Bob McWhirter et al (★208) 
- [**WebGen**](https://github.com/gettalong/webgen) by Thomas Leitner et al (★77)
- [**ZenWeb**](https://github.com/seattlerb/zenweb) by Ryan Davis et al (★50)
- and many more

Note: Sorted by GitHub Stars (+1s)




# Part II. - Python, Python, Python

# Hello, Pelican

![](i/staticgen-pelican.png)

by Alexis Metaireau et al -
[★5 231 GitHub Stars](https://github.com/getpelican/pelican),
[14 749 Downloads Last Month](https://pypi.python.org/pypi/pelican)

Static Site Spotlight: 

[Pelican Blog](https://github.com/getpelican/pelican-blog) •
[Linux Kernel Archives](https://www.kernel.org/pelican.html) •
[Many More](https://github.com/getpelican/pelican/wiki/Powered-by-Pelican)


# Hello, Nikola

![](i/staticgen-nikola.png)

by Roberto Alsina et al -
[★874 GitHub Stars](https://github.com/getnikola/nikola),
[6 038 Downloads Last Month](https://pypi.python.org/pypi/Nikola)

Static Site Spotlight: 

[Nikola](https://github.com/getnikola/nikola-site)


# Pelican or Nikola? -  Summary

                         | Pelican  | Nikola
-------------------------| ---------- | ---------
GitHub Stars (+1s)       | ★5 231    | ★874
Downloads Last Month     | 14 749    | 6 038 
                         |
Settings / Configuration | Python     | Python
HTML Templates           | Jinja2     | Jinja2
. Layouts                | Yes        | Yes
. Includes               | Yes        | Yes
Front Matter / Meta Data | reStructuredText   | reStructuredText
CSS Preprocessing        | -          | -
HTML "Shortcodes"        | reStructuredText   | reStructuredText 

Notes:

CSS Preprocessing - LESS or Sass supported with plugins


# Pelican or Nikola? - More Static Site Builders (in Python)

- [**Cactus**](https://github.com/koenbok/Cactus) by Koen Bok et al (★2 450)
- [**Hyde**](https://github.com/hyde/hyde) by Lakshmi et al (★1 234)
- [**Letterpress**](https://github.com/an0/Letterpress) by Ling Wang et al (★513)
- and many more

Note: Sorted by GitHub Stars (+1s)



# Part III. - JavaScript, JavaScript, JavaScript

# Hello, Wintersmith

![](i/staticgen-wintersmith.png)

by Johan Nordberg et al -
[★2 578 GitHub Stars](https://github.com/jnordberg/wintersmith),
[3 113 Downloads Last Month](https://www.npmjs.com/package/wintersmith)

Static Site Spotlight: 

[Wintersmith Examples](https://github.com/jnordberg/wintersmith/tree/master/examples) •
[Adventure Time! Wintersmith Sample](https://github.com/remotesynth/Static-Site-Samples/tree/master/wintersmithsite) •
[Many More](https://github.com/jnordberg/wintersmith/wiki/Showcase)


# Hello, Metalsmith

![](i/staticgen-metalsmith.png)

by Ian Storm Taylor et al -
[★4 070 GitHub Stars](https://github.com/segmentio/metalsmith),
[40 213 Downloads Last Month](https://www.npmjs.com/package/metalsmith)

Static Site Spotlight: 

[Metalsmith](https://github.com/segmentio/metalsmith.io) • 
[Metalsmith Examples](https://github.com/segmentio/metalsmith/tree/master/examples)


# Wintersmith or Metalsmith? - HTML Templates

**Wintersmith** - [Jade Template Language](http://jade-lang.com)

~~~
doctype html
html(lang="en")
    include ./partials/head
  body
    include ./partials/header

    div(id="main-wrapper")
        div(class="container")
            include ./partials/homepagemiddle

    include ./partials/footer
~~~

(Source: [templates/index.jade](https://github.com/remotesynth/Static-Site-Samples/blob/master/wintersmithsite/templates/index.jade))



# Wintersmith or Metalsmith? -  Summary

                         | Wintersmith | Metalsmith
-------------------------| ---------- | ---------
GitHub Stars (+1s)       | ★2 578    | ★4 070
Downloads Last Month     | 3 113    | 40 213
                         |
Settings / Configuration | JSON       | JavaScript
HTML Templates           | Jade       | Handlebars (HBS)
. Layouts                | Yes        | Yes
. Includes               | Yes        | Yes
Front Matter / Meta Data | YAML       | YAML
CSS Preprocessing        | -          | Plugins
HTML "Shortcodes"        | Markdown   | Markdown


# Wintersmith or Metalsmith? - More Static Site Builders (in JavaScript)

- [**Hexo**](https://github.com/hexojs/hexo) by Tommy Chen et al (★7 346)
- [**Harp**](https://github.com/sintaxi/harp) by Brock Whitten et al (★3 417)
- [**Blacksmith**](https://github.com/flatiron/blacksmith) by  et al (★571)
- and many more

Note: Sorted by GitHub Stars (+1s)





# Part IV. - Go, Go, Go

![](i/staticgen-hugo.png)

by Steve Francia et al - 
[★6 701 GitHub Stars](https://github.com/spf13/hugo)

Static Site Spotlight: 

[Balsamiq Documentation](http://blogs.balsamiq.com/ux/2015/11/19/new-documentation-site) •
[Adventure Time! Hugo Sample](https://github.com/remotesynth/Static-Site-Samples/tree/master/hugosite) •
[Many More](https://gohugo.io/showcase)


# Hugo - HTML Templates

**Hugo** - [Go Temmplate Language](https://golang.org/pkg/html/template)

~~~
<!DOCTYPE html>
<html>
  {{ partial "head.html" . }}
  <body>
    {{ partial "header.html" . }}
    <div id="main-wrapper">
        <div class="container">
            <article class="box post">
                <div class="image featured" style="background-image: url('{{ .Site.BaseUrl }}{{ .Params.banner }}');"></div>
                <header>
                  <h2>{{ .Title }}</h2>
                  <p>{{ .Params.shortdesc }}</p>
                </header>
                {{ .Content }}
            </article>
        </div>
    </div>
    {{ partial "footer.html" . }}
  </body>
</html>
~~~

(Source: [layouts/_default/single.html](https://github.com/remotesynth/Static-Site-Samples/blob/master/hugosite/layouts/_default/single.html))


# Hugo - Summary

                         | Hugo
-------------------------| ---------- 
GitHub Stars (+1s)       | ★6 701 
                         |
Settings / Configuration | TOML
HTML Templates           | Go Templates
. Layouts                | Yes
. Includes               | Yes
Front Matter / Meta Data | TOML
CSS Preprocessing        | -
HTML "Shortcodes"        | Markdown





# Part V. - Conclusions


# GitHub Stars

Site Builder  | GitHub Stars
------------- | -------------
Jekyll        | ★22 380 
Hugo          | ★6 701
Pelican       | ★5 231
Middleman     | ★4 756    
Metallsmith   | ★4 070
Wintersmith   | ★2 578 
Nicola        | ★874



# Building Blocks

**Configuration / Settings**

- 1) Use a data format: YAML • TOML • JSON
- 2) Use a (scripting) language: Ruby • Python • JavaScript 

**HTML Templates**

- 1) Use a "classic" template language: Liquid • Jinja • Handlebars (HBS) • Go Templates
- 2) Use a (scripting) language: Embedded Ruby (ERB) • Embedded JavaScript (EJS) 
- 3) Use a "compact" template language: Jade • Haml

**CSS Preprocessing**

- 1) No, thanks
- 2) Sass • LESS • Stylus

**HTML "Shortcodes"**

- 1) No, thanks
- 2) Markdown • ReStructuredText (ReST)  




# And the Winner is...

WordPress (Static Site) Exporter Plugin

![](i/staticgen-wp-jekyll-exporter.png)

by Ben Balter et al (★561)

WordPress: [`plugins/jekyll-exporter`](https://wordpress.org/plugins/jekyll-exporter),
GitHub: [`benbalter/wordpress-to-jekyll-exporter`](https://github.com/benbalter/wordpress-to-jekyll-exporter)



# Links, Links, Links

**Articles**


**News**


**Events**




# Bonus: Many More Static Site Builder / Generators

_What about PHP, Haskell, Lisp, Bash, Rust, C, [Your Language Here], etc.?_

See the staticgen or staticgen sites.



