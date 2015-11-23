
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

