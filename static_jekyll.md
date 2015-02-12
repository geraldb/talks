

%%% todo - move to planet jekyll
 move to quickrefs/versions.md  ???


## Milestones

### GitHub Pages Milestones

- 



## Jekyll Themes, Plugins, Sites Milestones

- Obama for President Campgain 2012 - switches from WordPress
   to static site w/ Jekyll for collecting donations



## Jekyll Milestones

> "Be the ~~change~~ pull request you wish to see in the world."
>   -- Mahatma Gandhi

- Upcoming in 2015 - **Jekyll v3.0.0** - headed by Parker Moore n friends
- Nov 2014 - [Jekyll's Mid-Life Crisis (Or, Jekyll turns 2.5.0) ](http://jekyllrb.com/news/2014/11/05/jekylls-midlife-crisis-jekyll-turns-2-5-0/)
- May 2014 - **Jekyll v2.0.0** - headed by Parker Moore n friends
    - [Jekyll turns 2.0.0 ](http://jekyllrb.com/news/2014/05/06/jekyll-turns-2-0-0/)
- May 2013 - **Jekyll v1.0.0** - headed by Parker Moore n friends
    - [Jekyll 1.0.0 Released](http://jekyllrb.com/news/2013/05/06/jekyll-1-0-0-released/)

- Dec 2012 - [The Immediate Future of Jekyll]() by Parker Moore
- Dec 2012 - [An Open Letter to Tom Preston-Werner](https://byparker.com/blog/2012/an-open-letter-to-tom-preston-werner/) by Parker Moore

> Jekyll is staying stagnant and plagued by the same issues
> it has been for the last **four years**.
> Solution: Add maintainers to Jekyll to help deal with issues,
> push new versions, and update the documentation when you don't have the time.

- May 2009  **v0.5.0** - headed by Tom Preston-Werner n friends
- Dec 2008 - **v0.3.0** 
- Nov 2008 - **v0.1.0** - First release by Tom Preston-Werner
    - [Nov 2008 - Blogging Like a Hacker](http://tom.preston-werner.com/2008/11/17/blogging-like-a-hacker.html) by by Tom Preston-Werner
      > Jekyll is a simple, blog aware, static site generator.
      > It takes a template directory (representing the raw form of a website),
      > runs it through Textile and Liquid converters,
      > and spits out a complete, static website suitable for serving with your favorite
      > web server. If you're reading this on the website (`tom.preston-werner.com`),
      > you’re seeing a Jekyll generated blog!
- Oct 2008 - **v0.0.0** - Birthday!


(Source: [Jekyll Contributors](https://github.com/jekyll/jekyll/graphs/contributors))



## Octopress Milestones

- Upcoming in 2015 - **Octopress v3.0.0** - headed by Brandon Mathis n friends
    - Jan 2015 - [Octopress 3.0 Is Coming](http://octopress.org/2015/01/15/octopress-3.0-is-coming/) by Brandon Mathis
      > What's coming? The Octopress 3.0 release is a full rewrite.
      > The fork and modify some guy's Jekyll blog distributed through Git 
      > will be replaced by a selection of gems, each with it's
      > own documentation and tests... 
      > **There will no longer be a division between Octopress and Jekyll.**
- Jul 2011 - **Octopress v2.0.0**
    - [Octopress 2.0 Surfaces](http://octopress.org/2011/07/23/octopress-20-surfaces/) by Brandon Mathis



## Jekyll Versions

- Preprocessing (Converter)
    - Converts posts written in Textile
    - Markdown support (was Textile only at first!)
    - Code highlighting
    - Add support for syntax highlighter w/ Rouge (was using system call w/ pygments)
    - Add support for css preprocessing w/ Sass
    - Add support for CoffeeScript
    - New markdown "standard" convertor, that is, kramdown  (was maruku)
    - Allow custom markdown convertor


- Permalinks & Config
    - Allow layouts to be in subfolders like includes 
    - Front matter (meta data e.g. title, date, tags, layout, etc.) defaults
    - Added ability to render drafts in _drafts folder via command line
    - Add ordinal date permalink style (/:categories/:year/:y_day/:title.html)
    - Allow placeholders in permalinks e.g.
    - Permalink templating system
    - Add ‘pretty’ permalink style for wordpress-like urls
    - Ability to set post categories via front matter
    - Ability to set prevent a post from publishing (draft) via front matter
    - Added post categories based on directories containing _posts
    - Add a real “related posts” implementation using Classifier

- Add "Collections" feature

- Add "Data" feature (e.g. support adding data as YAML files under a site’s _data directory)
    - Add support for JSON files in the _data directory
    - Add support for CSV files in the _data directory
    - Allow subdirectories in _data


- Commands
    - Refactored Jekyll commands into subcommands: build, serve, and migrate
    - Add jekyll new subcommand: generate a Jekyll scaffold
    - Add b and s aliases for build and serve commands


- Liquid (Templating)
    - Bump to the latest Liquid version, 3.x
    - Bump to the latest Liquid version, 2.6.1

    - Support a new relative_include tag   
    - Support passing parameters to templates in include tag
    - Add support for Liquid tags to post excerpts
    - Allow variables to be used with include tags

- Plugins
    - Allow using gems for plugin management
    - Add gem-based plugin whitelist to safe mode
    - Moved importers/migrators to jekyll-import gem


- Importers / Migrators




# Jekyll Versions (Milestones)

## 3.0.0  - Upcoming in 2015

- Speed (incremental regeneration e.g. build only pages that changed)
- Bump to the latest Liquid version, 3.x

## 2.5.0  - Nov 2014

- Allow placeholders in permalinks e.g.
- Add b and s aliases for build and serve commands

 
## 2.4.0 - Sep 2014

- Support a new relative_include tag
- Add support for CSV files in the _data directory

## 2.2.0 / 2014-07-29

## 2.1.0 / 2014-06-28

- Add support for JSON files in the _data directory
- Allow subdirectories in _data
- Bump to the latest Liquid version, 2.6.1
- Front matter defaults for documents in collections

## 2.0.0 / May 2014

- Add "Collections" feature
- Add support for syntax highlighter w/ Rouge (was using system call w/ pygments)
- Add support for css preprocessing w/ Sass
- Add support for CoffeeScript
- Front matter (meta data e.g. title, date, tags, layout, etc.) defaults

- New markdown "standard" convertor, that is, kramdown  (was maruku)
- Allow custom markdown convertor

- Add gem-based plugin whitelist to safe mode


## 1.3.0 / Nov 2013

- Add support for adding data as YAML files under a site’s _data directory
- Allow using gems for plugin management

- Allow layouts to be in subfolders like includes 
- Allow variables to be used with include tags

## 1.2.1 / 2013-09-14

## 1.1.0 / 2013-07-24

- Support passing parameters to templates in include tag
- Add support for Liquid tags to post excerpts


## 1.0.0 / May 2013

- Added ability to render drafts in _drafts folder via command line
- Add ordinal date permalink style (/:categories/:year/:y_day/:title.html)

- Add jekyll new subcommand: generate a Jekyll scaffold
- Refactored Jekyll commands into subcommands: build, serve, and migrate
- Removed importers/migrators from main project, migrated to jekyll-import sub-gem (#793)

## 0.5.0 / May 2009

- Permalink templating system
- Add ‘pretty’ permalink style for wordpress-like urls
- Ability to set post categories via front matter
- Ability to set prevent a post from publishing (draft) via front matter

## 0.3.0  / Dec 2008

- Added post categories based on directories containing _posts


## 0.1.3 / Dec 2008

- Markdown support
- Code hilighting

## 0.1.2 / Nov 2008

- Add a real “related posts” implementation using Classifier

## 0.1.0 / Nov 2008 (First release)

- Converts posts written in Textile
- Converts regular site pages
- Simple copy of binary files

## 0.0.0 / Oct 2008 (Birthday!)



## Jekyll Gems (Plugins)

Extract gist tag into a separate gem   (2.1.0)


### Standard / Core
-
-

### More

(Sources:
[History](http://jekyllrb.com/docs/history),
[Versions]())

