title: Quik - The Missing Project Scaffolder for Ruby - Quick Start Your Ruby Gems, Your Sinatra Apps, Your Jekyll Sites 'n' More




# Agenda




# Quick Starter Kits / Boilerplates / Project Scaffolder in the World

- Python uses [Cookiecutter](https://github.com/audreyr/cookiecutter)
- Node.js uses [Yeoman](http://yeoman.io) (`yo`) or (NEW!) [Yarn](https://yarnpkg.com/en/docs/cli/create) with (`create`)
- Rails uses [Rails Application Templates](http://guides.rubyonrails.org/rails_application_templates.html) ;-)

Q: What about Ye Old' Ruby?



# How do you get started with creating a new gem?

- [A] From scratch ;-)
- [B] Using bundler with `$ bundle gem`
- [C] Using quik with `$ quik new gem (or qk n gem)`
- [D] Using hoe with `$ soe` (incl. with hoe rake tasks gem)
- [E] Other (Please Tell).


# How do you get started with creating a new sinatra app?

- [A] From scratch ;-)
- [B] Using padriono with `$ padrino-gen project cool --orm activerecord`
- [C] Using quik with `$ quik new sinatra (or qk n sinatra)`
- [D] Other (Please Tell).


# How do you get started with creating a new jekyll theme?

- [A] From scratch ;-)
- [B] Using jekyll with `$ jekyll new-theme`
- [C] Using quik with `$ quik new jekyll (or qk n jekyll)`
- [D] Other (Please Tell).



# One Quik Starter to Rule Them All?

The Idea - Many starter templates / boilerplates
are read-to-fork GitHub repos.

Why not turn GitHub repos into quik starter templates?!

Let's do it in 1-2-3 steps.




# Step 1: Download Single-file Quik Starter Archive (.ZIP)

Did you know? You can download GitHub repos without git?
That is, download a single-file archive (.ZIP) -- gets (auto-)built by GitHub.

Example - `gem-starter.zip`:

```
lib/
  $filename$.rb
  $filename$/
    version.rb    
test/
  helper.rb
  test_version.rb
.gitignore
HISTORY.md
Manifest.txt
README.md
Rakefile
```


# Step 2: Parameterize Files - Use a Template Language

- [A] Use Embedded Ruby (ERB)
- [B] Use Liquid
- [C] Other (Please Tell).


Example - `lib/linz/version.rb`:

```
module Linz

  MAJOR = 0
  MINOR = 0
  PATCH = 1
  VERSION = [MAJOR,MINOR,PATCH].join('.')

  def self.version
    VERSION
  end

  def self.banner
    "linz/#{VERSION} on Ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) [#{RUBY_PLATFORM}]"
  end

end  # module Linz
```


# Step 2: Parameterize Files - Use a Template Language

Let's use a new (simpler) template language (e.g. `$name$`)!

Example - `lib/$filename$/version.rb`:

```
module $module$

  MAJOR = 0
  MINOR = 0
  PATCH = 1
  VERSION = [MAJOR,MINOR,PATCH].join('.')

  def self.version
    VERSION
  end

  def self.banner
    "$name$/#{VERSION} on Ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) [#{RUBY_PLATFORM}]"
  end

end  # module $module$
```


# Step 2: Parameterize Files - A New Meta Template Template Language

Why not ERB or Liquid?

- Simpler  -- works inside filenames too ;-) e.g. `lib/$filename$/version.rb`
- Shorter  -- less typing (plus: no worries with withespace)

**Most Important:** "Orthogonal" to ERB and Liquid.
Lets you parameterize ERB or Liquid templates too - no need for escaping or "raw" blocks etc.


```
module $module$     | module <%= module %>  | module {{ module }}  | module Linz
  ...               |  ...                  |   ...                |   ...
end                 | end                   | end                  | end
```


# Step 3: What's Missing? All together Now - Automate with a Script

Let's use Ruby ;-) with wizard mini language,
that is, a Domain-Specific Language (DSL).

Example - `scripts/gem.rb`:

```
say "Hello from the gem quick starter wizard script"

name  = ask "Name of the gem", "hola"

def make_module( name )
   ...
end

module_name = ask "Module name of the gem", make_module( name )


## use template repo e.g. github.com/rubyref/gem-starter-template

use "rubyref/gem-starter-template"     

config do |c|
  c.name     = name
  c.filename = name     ## for now assume name is 1:1 used as filename
  c.module   = module_name

  c.date     = Time.new.strftime("%Y-%m-%d")  ## e.g. use like $date$  => 2015-08-27
end
```



# That's it. Let's welcome the quik command line tool / gem.

Q: What's qk/quik?

Ruby quick starter template script wizard .:. the missing code generator or project scaffolder.

Trivia:

Q: Why is quik misspelled? A: Because it's shorter (saves on letter quik vs quick is quicker).
Helps with finding quik templates on Google!



# quik help  - Quik Starter Commands

```
$ quik --help      # or
$ qk -h
```

Resulting in:

```
NAME
    qk/quik - ruby quick starter template script wizard .:. the missing code generator

SYNOPSIS
    quik [global options] command [command options] [arguments...]

VERSION
    0.3.0

GLOBAL OPTIONS
    --help            - Show this message
    --test, --dry_run - (Debug) Dry run; run script in simulation for testing
    --verbose         - (Debug) Show debug messages
    --version         - Display the program version

COMMANDS
    list, ls, l - List ruby quick starter scripts
    new, n      - Run ruby quick starter script

    help        - Shows a list of commands or help for one command
    test        - (Debug) Test command suite
```




# quik ls - List Quik Starter Wizards

Use:

```
$ quik list    # or
$ quik ls      # or
$ quik l       # or
$ qk l
```

Resulting in:

```
  1..gem        .:.  Gem Quick Starter Template
  2..gem-hoe    .:.  Gem Quick Starter Template (Hoe Classic Edition)
  3..sinatra    .:.  Sinatra Quick Starter Template
...
```


# quik new - New Wizard Quik Start

To run a quick starter template wizard script
to download and install (unzip/unpack) a template archive and configure
the code ready-to-use. Try:

```
$ quik new gem    # or
$ quik n gem      # or
$ qk n gem
```


# quik new - New Wizard Quik Start (Cont.)


This will download the `gem.rb` wizard script
from the [Scripts](https://github.com/quikstart/scripts) repo
and run through all steps e.g.:

```
Welcome, to the gem quick starter script.

Q: What's your gem's name? [hola]:   hello
Q: What's your gem's module? [Hola]: Hello

Thanks! Ready-to-go. Stand back.

  Downloading Rubyref Gem Starter Template...
  Setting up Rubyref Starter Template...
  ...
Done.
```

That's it. Now the gem starter code is ready in the `hello`
folder.



# That's it. Thanks.

Questions? Comments?



# Bonus - Meet Mr Hyde

Q: What's Mr Hyde?

mrhyde is static website quick starter script wizard .:. the missing jekyll command line tool

Try:

```
$ mrhyde help
```



# Bonus:  Meet Mr Hyde - New Static Website Wizard Command

To run a static website quick starter wizard script
to download and install (unzip/unpack) a theme archive and configure
a static site ready-to-use. Try:

```
$ mrhyde new starter    # or
$ mrhyde n starter      # or
$ mrh n starter
```

This will download the `starter.rb` wizard script
from the [Mr. Hyde's Scripts](https://github.com/mrhydescripts/scripts) repo
and run through all steps e.g.:



# Bonus:  Meet Mr Hyde - New Static Website Wizard Command (Cont.)

```
Welcome, before setting up your site Mr. Hyde will ask you some questions.

Q: What's your site's title? [Your Site Title]:  Another Beautiful Static Site
Q: What's your name? [Henry Jekyll]: Edward Hyde
Q: Select your theme:
     1 - Starter
     2 - Bootstrap
     3 - Minimal
   Your choice (1-3)? [1]: 2

Thanks! Ready-to-go. Stand back.
```


# Bonus:  Meet Mr Hyde - New Static Website Wizard Command (Cont.)

```
  Downloading Henry's Bootstrap Theme...
  Setting up Henry's Bootstrap Theme..
  ...
  Updating settings in _config.yml...
    title: "Another Beautiful Static Site"
    author.name: "Edward Hyde"
  ...
Done.
```

That's it. Now use:

```
$ cd starter
$ jekyll serve
```

And open up your new static site in your browser.




# Bonus II: Quik Starter Thor Command Line Template

by Georg G. (aka nilsding) from Linz! Thanks!

```
config/
  environment.rb
  my_app.yml.example
exe/
  __name__
lib/
  __name__/
    application.rb
    cli.rb
    configuration.rb
  rake/
    rubocop_runner.rake
  __name__.rb
.rubocop.yml
.ruby-gemset
.ruby-version
Gemfile
LICENSE
README.md
Rakefile
```

(Source: [nilsding/ndcli-template](https://github.com/nilsding/ndcli-template))



# Bonus II: Quik Starter Thor Command Line Template (Cont.)

Use `$ quik new ndcli`

Quik Wizard Script - `ndcli.rb`:

```
# transform snake_case into CamelCase - borrowed from example quikstart scripts :-)
def camelize(str)
  str.gsub(/(?:^|_)([a-z])/i) { $1.upcase }
end

# generate a nice name, because naming is hard apparently
parts = %w(
  awesome epic great best word excel powerpoint office internet_explorer
  paint photoshop fox wolf dog coyote woof awoo bark bork yip yap yiff
  image video answer question retrospring sausage oachkatzlschwoaf bier
  pretzel emacs vim notepad slack twitter reddit ruby diamond gem humor
  lol internet melli17 blue green red black yellow orange magenta white
)
awesome_name = parts.shuffle.take(2).join("_")

say "*** nilsding's CLI template ***"

name = ask("How should I name your app? (snake_case)", awesome_name)

use "nilsding/ndcli-template"

config do |c|
  c.name         = name
  c.class        = camelize(name)
  c.current_year = Time.now.year.to_s
  c.user         = ENV.fetch("USER", "Anonymous Coward")
end
```

(Source: [nilsding/ndcli-template/ndcli.rb](https://github.com/nilsding/ndcli-template/blob/master/ndcli.rb))
