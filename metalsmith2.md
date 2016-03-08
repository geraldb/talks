title: Building Static (Web) Sites w/ Metalsmith (Node.js)


# Agenda

- What's Metalsmith?
- Everything is a Plugin
- File / Directory Structure
- Real World Showcase -  Node.js Project Site
- More Plugins



# What's Metalsmith?

A static site builder in JavaScript (Node.js).

![](i/staticgen-metalsmith.png)

See [`metalsmith.io`](http://www.metalsmith.io) »



# Metalsmith - Everything is a Plugin

Configure file processing build pipeline / chain using plugins


Example - `build.js`:

```
Metalsmith(_dirname)
  .source( 'src' )

  /*******************
    add plugins here
   ******************* /
 
  .destination( 'build' )
  .build( function(err) {
	        if (err) console.log(err);
  });
```

# Metalsmith - Everything is a Plugin (Cont.)

Example:

```
  .use( metadata({
          links:  './data/links.json' }))
  .use( inPlace({
          engine: 'nunjucks',
          pattern: '**/*.html' }))
  .use( markdown() )
  .use( layouts({
          engine: 'nunjucks',
          directory: './layouts' }))
```


# Metalsmith - File / Directory Structure

```
├── build.js                     # Metalsmith build script / configuration
├── layouts/
|   ├── default.html             # master layout template
|   ├── page.html                # - layout for pages
|   └── post.html                # - layout for blog posts
└─── src/
    ├── css/
    |   └── style.css           
    ├── data/
    |   └── links.json           # datafile (in json)
    ├── posts/
    |    ├── 2014-11-11-new-repo-maps.md
    |    ├── 2014-12-12-new-build-system.md
    |    └── 2015-08-25-new-season.md   
    ├── about.md                 # about page (in markdown e.g. md) 
    └── index.html               # front page (in hypertext e.g. html)
```


# Metalsmith - File / Directory Structure  - Output

Use:

```
$ node build.js
```

Resulting in:

```
└─── build/
     ├── css/
     |   └── style.css
     ├── posts/
     |    ├── 2014-11-11-new-repo-maps.html
     |    ├── 2014-12-12-new-build-system.html
     |    └── 2015-08-25-new-season.html
     ├── about.html
     └── index.html     
```


# Metalsmith - File / Directory Structure  - Output (Cont.)

![](i/staystatic-samplesite.png)

(Source: Stay Static Sample Site Showcase - `index.html`)


# Metalsmith - HTML Templates - Handlebars or Nunjucks?

Handlebars.js (web: [`handlebarsjs.com`](http://handlebarsjs.com)) - Minimalistic
Mustache-style `{{}}` "logic-less" templates
(e.g. no arguments for if conditionals or each loops possible,
no nested layouts, no content blocks, no inline macros, etc.)   

~~~
<!DOCTYPE html>
<html>
  {{{> head }}
  <body>
     {{{> header }}
    <div class="main">
      {{{{ contents }}}
    </div>   
     {{{> footer }}
  </body>
</html>
~~~

(Source: [staystatic/metalsmith-handlebars/layouts/default.html](https://github.com/staystatic/metalsmith-handlebars/blob/master/layouts/default.html))


# Metalsmith - HTML Templates - Handlebars or Nunjucks? (Cont.)

Nunjucks.js (web: [`mozilla.github.io/nunjucks`](https://mozilla.github.io/nunjucks)) - Rich & powerful
template language with block inheritance, autoescaping, macros, asynchronous control, and much more.
(Inspired by Jinja2 - a python template language inspired by Django's template language).


~~~
<!DOCTYPE html>
<html>
  {%% include 'partials/head.html' %}
  <body>
    {%% include 'partials/header.html' %}
    <div class="main">
      {%% block content %}
        {{{ contents | safe }}
      {%% endblock %}
    </div>   
    {%% include 'partials/footer.html' %}
  </body>
</html>
~~~



(Source: [staystatic/metalsmith-nunjucks/layouts/default.html](https://github.com/staystatic/metalsmith-nunjucks/blob/master/layouts/default.html))



# Metalsmith - HTML Templates - Handlebars or Nunjucks? (Cont.)

Metalsmith uses Consolidate.js (github: [tj/consolidate.js](https://github.com/tj/consolidate.js)) -
a template engine consolidation library for Node.js.

More template engines include:

- Eco (Embedded CoffeeScript)
- EJS (Embedded JavaScript)
- Jade
- Liquid
- Mustache
- And many more



# Real World Showcase -  Node.js Project Site

![](i/showcase-nodejs.png)

See [`nodejs.org`](https://nodejs.org)
[(Source)](https://github.com/nodejs/nodejs.org) » 


# Real World Showcase -  Node.js Project Site (Cont.)

Source - [`build.js`](https://github.com/nodejs/nodejs.org/blob/master/build.js):

```
const Metalsmith  = require('metalsmith')
const collections = require('metalsmith-collections')
const feed        = require('metalsmith-feed')
const layouts     = require('metalsmith-layouts')
const markdown    = require('metalsmith-markdown')
const prism       = require('metalsmith-prism')
const stylus      = require('metalsmith-stylus')
const permalinks  = require('metalsmith-permalinks')
const pagination  = require('metalsmith-yearly-pagination')

...

const metalsmith = Metalsmith(__dirname)
  metalsmith
    // Sets global metadata imported from the locale's respective site.json.
    .metadata({
      site: require(siteJSON),
      project: source.project,
      i18n: i18nJSON(locale)
    })
    // Sets the build source as the locale folder.
    .source(path.join(__dirname, 'locale', locale))
    // Defines the blog post/guide collections used to internally group them for
    // easier future handling and feed generation.
    .use(collections({
      blog: {
        pattern: 'blog/**/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false
      },
      blogAnnounce: {
        pattern: 'blog/announcements/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false
      },
      blogReleases: {
        pattern: 'blog/release/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false
      },
      blogVulnerability: {
        pattern: 'blog/vulnerability/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false
      },
      lastWeekly: {
        pattern: 'blog/weekly-updates/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false,
        limit: 1
      },
      tscMinutes: {
        pattern: 'foundation/tsc/minutes/*.md',
        sortBy: 'date',
        reverse: true,
        refer: false
      },
      knowledgeBase: {
        pattern: 'knowledge/**/*.md',
        refer: false
      },
      guides: {
        pattern: 'docs/guides/!(index).md',
        refer: false
      }
    }))
    .use(pagination({
      path: 'blog/year',
      iteratee: (post, idx) => ({
        post,
        displaySummary: idx < 10
      })
    }))
    .use(markdown(markedOptions))
    .use(githubLinks({ locale: locale }))
    .use(prism())
    // Deletes Stylus partials since they'll be included in the main CSS file
    // anyways.
    .use(filterStylusPartials())
    .use(stylus({
      compress: true,
      paths: [path.join(__dirname, 'layouts', 'css')],
      use: [autoprefixer()]
    }))
    // Set pretty permalinks, we don't want .html suffixes everywhere.
    .use(permalinks({
      relative: false
    }))
    // Generates the feed XML files from their respective collections which were
    // defined earlier on.
    .use(feed({
      collection: 'blog',
      destination: 'feed/blog.xml',
      title: 'Node.js Blog'
    }))
    .use(feed({
      collection: 'blogAnnounce',
      destination: 'feed/announce.xml',
      title: 'Node.js Announcements'
    }))
    .use(feed({
      collection: 'blogReleases',
      destination: 'feed/releases.xml',
      title: 'Node.js Blog: Releases'
    }))
    .use(feed({
      collection: 'blogVulnerability',
      destination: 'feed/vulnerability.xml',
      title: 'Node.js Blog: Vulnerability Reports'
    }))
    .use(feed({
      collection: 'tscMinutes',
      destination: 'feed/tsc-minutes.xml',
      title: 'Node.js Technical Steering Committee meetings'
    }))
    // Finally, this compiles the rest of the layouts present in ./layouts.
    // They're language-agnostic, but have to be regenerated for every locale
    // anyways.
    .use(layouts({
      engine: 'handlebars',
      pattern: '**/*.html',
      partials: 'layouts/partials',
      helpers: {
        copyright: require('./scripts/helpers/copyright-year.js'),
        equals: require('./scripts/helpers/equals.js'),
        startswith: require('./scripts/helpers/startswith.js'),
        i18n: require('./scripts/helpers/i18n.js'),
        changeloglink: require('./scripts/helpers/changeloglink.js'),
        strftime: require('./scripts/helpers/strftime.js'),
        apidocslink: require('./scripts/helpers/apidocslink.js'),
        majorapidocslink: require('./scripts/helpers/majorapidocslink.js'),
        summary: require('./scripts/helpers/summary.js')
      }
    }))
    // Pipes the generated files into their respective subdirectory in the build
    // directory.
    .destination(path.join(__dirname, 'build', locale))

  // This actually executes the build and stops the internal timer after
  // completion.
  metalsmith.build(function (err) {
    if (err) { throw err }
    console.timeEnd('[metalsmith] build/' + locale + ' finished')
  })
}
```


# Metalsmith - More Plugins

**metadata** - Load metadata from JSON or YAML files.

**inPlace** - In-place templating, render templates in your source files.

**markdown** - Convert Markdown files to HTML.

**layouts** - Apply layouts to your source files.

---

**drafts** - Hide any files marked as drafts.

**permalinks**  - Apply custom permalinks and rename files to be nested properly for
static sites, basically converting about.html into about/index.html.

**collections** - Group files together, like blog posts.
That way you can loop over them to generate an index, or add 'next' and 'previous'
links between them.

**feed** - Generate an RSS feed for a collection.

**excerpts** - Extract the first paragraph from the beginning of any HTML file.


And Many More



# Links, Links, Links

Interested in Static (Web) Site Builders / Generators?

Follow the [Static Times News Channel](https://twitter.com/statictimes).

or 

See the [Stay Static Showcase](http://staystatic.github.io).

Thanks. 


