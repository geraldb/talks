% Talk Notes


- Static is the New Dynamic
- Jekyll is the New Ruby Killer App (Jekyll is the New Rails)


# Dynamic Site Generators

- The Biggies

**PHP**

- WordPress
- Drupal
- Joomla

On your live production site requires

- database (e.g. mysql)
- application server (e.g. mod_php)
- web server (e.g. apache)



# Static Site Generator

On your live prodcution site requires

- web server (e.g. apache) only

- NO database needed
- NO application server needed



# Why Static?

- Fast, Faster, Fastest

- Simple, Simpler, Simplest

Bonus: Super secure - invite all the hackers - basically unbreakable!

There are only static files on your server.
If an attacker hacks your sever, there's "just" some "temporary" data loss.
To recover
- Regenerate your site on your local machine.
- Upload it again to the server or shutdown the old "hacked" server and use a new server
   and you're back in action.

Note: You can host your site on Amazon S3.



# Static Site Generator Directories

- [staticgen.com](http://www.staticgen.com)
- [staticsitegenerators.net](http://staticsitegenerators.net)


# Static Site Generator / Static Site Server


- The Biggies

**Ruby**

- Jekyll         18_000+ Stars/3_700+ Forks
    - Octopress   8_600+ Stars/3_100+ Forks
    - Jekyll Now    800+ Stars/2_500+ Forks
    - Jekyll Incorporated    800+ Stars/200+ Forks
    - Poole (Jekyll Butler)  900+ Stars/300+ Forks
    - Hyde  (Jekyll Theme)   800+ Stars/400+ Forks
    - Lanyon (Jekyll Theme)  600+ Stars/300+ Forks
- Middleman       3_700+ Stars/  300+ Forks
- Nanoc           1_000+ Stars/  100+ Forks

**Python**

- Pelican
- Cactus
- Hyde

**JavaScript**

- Hexo
- Metalsmith
- Harp
- Docpad


**And Many More**

- Hugo (Go)
- Hakyll (Haskell)





# WordPress - Jekyll Goodies

- WordPress Jekyll Export Plugin  => One Click - gets you a zip w/ ready-to-use static Jekyll site

(best of both worlds - use all the WordPress tools plus get a fast and simple static site for live production)



# Jekyll Goodies

- Jekyll Now
- 


# GitHub Pages




# Octopress v1.0 vs. Octopress v2.0

- v1.0 (Released 20??)

  Basically a ready-to-use pre-configured Jekyll site w/ a theme, helpers, build scripts and more.
  "packaged" as git repo. To get started use
  
$ git clone octopress

and than start changing the configuration settings in ???.


- v2.0 (Released February 2015)

  Modul-mania. Now almost everything is a gem (more than 20+ gems e.g. octopress-videotag, octopress-unpublish,
    octopress-deplay)
      plus a new command line tool (that is, octopress)

To get started, use:

$ gem install octopress
$ octopress new


Why?

- Easier to update.
- Easier to extend and configure (use what you need; no big all-in-one git repo hairball -
    instead many small gems that work with "plain vanilla" jekyll)

