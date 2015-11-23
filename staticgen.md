

# Ruby, Ruby, Ruby

# Middleman or Jekyll?  - HTML Templates

**Middleman**

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


**Jekyll**

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


**Middleman**

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



**Jekyll**

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

**Middleman**

~~~
set :css_dir, 'stylesheets'

set :js_dir, 'javascripts'

set :images_dir, 'images'

set :site_title, 'Adventure Time!'
set :banner, '/images/about.jpg'
set :description, 'Adventure Time is an American animated television series created by Pendleton Ward for Cartoon Network...'

activate :blog do |blog|
  blog.sources           = "posts/{year}-{month}-{day}-{title}.html"
  blog.summary_separator = "<!--more-->"
  blog.tag_template      = "tag.html"
  blog.calendar_template = "calendar.html"
end

page "/feed.xml", layout: false
~~~

(Source: [`config.rb`](https://github.com/remotesynth/Static-Site-Samples/blob/master/middlemansite/config.rb))


**Jekyll**

~~~
title: Adventure Time!
email: brian.rinaldi@example.com
banner: "/images/about.jpg"
description: > Adventure Time is an American animated television series created by Pendleton Ward for Cartoon Network.
  The series follows the adventures of Finn, a human boy, and his best friend and adoptive brother Jake,
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



# Middleman or Jekyll? - More Static Site Builders in Ruby

Sorted by GitHub Stars (+1s):

<!-- todo: use/add stars char -->

- [Nanoc](https://github.com/nanoc/nanoc) by Denis Defreyne et al (★1 225)
- [Ruhoh](https://github.com/ruhoh/ruhoh.rb) by Jade Dominguez et al (★611)
- [Bonsai](https://github.com/benschwarz/bonsai) by Ben Schwarz et al (★269)
- [Awestruct](https://github.com/awestruct/awestruct) by Bob McWhirter et al (★208) 
- [WebGen](https://github.com/gettalong/webgen) by Thomas Leitner et al (★77)
- [ZenWeb](https://github.com/seattlerb/zenweb) by Ryan Davis et al (★50)
- and many more


# Python, Python, Python



# Pelican or Nicola? 


<!-- todo: -->


# JavaScript, JavaScript, JavaScript

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



# Go, Clojure, Haskell, [Your Language Here], etc.

# Hugo or [Your Static Site Builder Here]? - HTML Templates

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



# HTML Templates - Summary

Site Builder  | Language   | HTML Templates
------------- | ---------- | ----------------------------------------
Middleman     | Ruby       | Embedded Ruby (ERB) Template Language
Jekyll        | Ruby       | Liquid Template Language 
Pelican       | Python     | Jinja2 Template Language
Nicola        | Python     | Jinja2 Template Language
Wintersmith   | JavaScript | Jade Template Language
Metallsmith   | JavaScript | Handlebars (HBS) Template Language
Hugo          | Go         | Go Template Language


