%%%%%%%
% Slide Show Headers

title: Tagging & Tag Clouds Made Easy

%%%%%%%%%%%%%
% Slides Start Here

# Tagging & Tag Clouds Made Easy

Agenda

* Tags, Tag Clouds, Folksonomy - Glossary
* Tag Clouds in the Wild
* Why? Share. Tag. Discover.
* Add Tags To Your Web App in 1-2-3 Steps
* Add Tag Clouds To Your Web App in 1-2-3 Steps
* Plugin Optimizations & Options
* Rails Tagging Plugin Choices


# Tags, Tag Clouds, Folksonomy - Glossary

Tags - Quick & Easy Free-Style Keywords

Tag Clouds - Listing of Most Popular Tags - More Popular Tags Usually Shown in Bigger Font Size or Deeper Color

Folksonomy - Self-organizing non-hierarchical cataloging/classification system using free-style keywords (also known as tags)

### Web 1.0/3.0 - Web 2.0

Category != Tag

Taxonomy != Folksonomy

Tag == Label, Keyword, Category (?)


# Tag Clouds in the Wild

* [Delicious Bookmark Tag Cloud](http://del.icio.us/tag)
* [Flickr Photo Tag Cloud](http://flickr.com/photos/tags)
* [LibraryThing Book Tag Cloud](http://www.librarything.com/tagcloud.php)
* [WordPress Blog Posting Tag Cloud](http://wordpress.com/tags)

# Why? Share. Tag. Discover.

Tagging Examples

* Location: burnaby gastown yaletown granville ubc stanleypark
* Events: northernvoice groovyvan vanrb barcampvancouver vanajax
* Ratings: `*` (1 Star) `**` (2 Star) `***` (3 Star) `****` (4 Star) `*****` (5 Star)

Discover Examples

* [Latest Bookmarks About Rails](http://del.icio.us/tag/rails)
* [Most Popular Bookmarks About Rails](http://del.icio.us/popular/rails)
* [Latest Blog Posts Tagged Vancouver](http://wordpress.com/tag/vancouver)
* [Latest Photos Tagged Vancouver](http://flickr.com/photos/tags/vancouver)
* Discover Related Tags and much more

# Wikipedia Tagging Related Terms Tag Cloud

[Tag (metadata)](http://en.wikipedia.org/wiki/Tag_(metadata))
[Tag Cloud](http://en.wikipedia.org/wiki/Tag_cloud)
[Social Bookmarking](http://en.wikipedia.org/wiki/Social_bookmarking)
[Folksonomy](http://en.wikipedia.org/wiki/Folksonomy)
[Taxonomy](http://en.wikipedia.org/wiki/Taxonomy)
[Web 2.0](http://en.wikipedia.org/wiki/Web_2.0)
[Web 3.0](http://en.wikipedia.org/wiki/Web_3.0)
[Semantic Web](http://en.wikipedia.org/wiki/Semantic_web)


# Add Tags To Your Web App

Step 0: Install Taggable Plug-In

Step 1: Add Database Tables (Tags, Taggings)

Step 2: Mark Your ActiveRecord Classes (Bookmark, Photo, Book etcetera) As Taggable

Step 3: That's it.


# Step 1 - Add Database Tables (Tags, Taggings)

The Tags Table

```
create_table :tags do |t|
  t.column :name, :string  # The tag name
end
```

The Taggings (Polymorphic Join) Table

```
create_table :taggings do |t|
   t.column :tag_id, :integer       # foreign key; id of the tag
   t.column :taggable_id, :integer  # foreign key; id of the taggable object
   t.column :taggable_type, :string # taggable object type (bookmark, photo, book, etcetera)
   t.column :created_at, :datetime  # timestamp
end
```


# Step 2 - Mark Your ActiveRecord Classes As Taggable

```
class Photo < ActiveRecord::Base
  acts_as_taggable
  ...
end
```

### Methods Added To Your ActiveRecord Model Classes

* tag_list
* find_tagged_with( tags, options = {} )
* tag_counts( options = {} )

Get Tags:

```
>> p = Photo.find(:first)
>> p.tag_list.to_s

=> "wedding vancouver summer"
```

Store Tags:

```
>> p = Photo.find(:first)
>> p.tag_list = "wedding vanouver summer"
>> p.save
```

Add or Remove Tags:

```
>> p.tag_list.add( "fun" )
>> p.tag_list.remove( "wedding" )
```

Find Photos Tagged With:

```
>> photos = Photo.find_tagged_with( 'summer' )
```

Note: You can use options such as `:order`, `:limit`, `:offset`, etcetera

Get Tag Counts for Tag Cloud:

```
>> tags = Photo.tag_counts
```


# Add Tag Clouds To Your Web App

Step 1: Add the Tag Cloud Helper to your App

Step 2: Write the Controllers and Views

Step 3: That's it.


# Step 1: Add the Tag Cloud Helper to Your App

`public/stylesheets/cloud.css`:

```
.cloud1 {font-size: 100%;}
.cloud2 {font-size: 120%;}
.cloud3 {font-size: 140%;}
.cloud4 {font-size: 160%;}
.cloud5 {font-size: 180%;}
.cloud6 {font-size: 200%;}
.cloud7 {font-size: 220%;}
```

`app/helpers/application_helper.rb`:

```
def tag_cloud( tags )
  classes = %w(cloud1 cloud2 cloud3 cloud4 cloud5 cloud6 cloud7)

  max, min = 0, 0
  tags.each { |t|
    max = t.count.to_i if t.count.to_i > max
    min = t.count.to_i if t.count.to_i < min
  }

  divisor = ((max - min) / classes.size) + 1 

  tags.each { |t|
     yield t.name, classes[(t.count.to_i - min) / divisor]
  }
end
```

# Step 2: Write the Tags Controller and View

`app/controllers/tags_controller.rb`:

```
class TagsController < ApplicationController
  def index
      @tags = Photo.tag_counts( :order => "name" )
  end
end
```

`app/views/tags/index.rhtml`:

```
<%% tag_cloud @tags do |name, css_class|  %>
      <%%= link_to name, {:action => :tag, :id => name},
            :class => css_class %>
<%% end %>
```


# Plugin Optimizations & Options

Create Database Index for Taggings Table

```
add_index :taggings, :tag_id
add_index :taggings, [:taggable_id, :taggable_type]
```

Add Cached Tag List Table Column

```
t.column :cached_tag_list, :string   # tells plugin to cache taglist
```

Tag Seperator/Delimiter

```
TagList.delimiter = " "  # use space or comma
```

# Rails Tagging Plugin Choices

* `acts_as_taggable` Gem by Dirk Elmendorf
* `acts_as_taggable` Plugin by David Heinemeier Hansson (Prototype - Not Production Ready)
* `acts_as_taggable_on_steroids` Plugin by Jonathan Viney
* `acts_as_taggable_redux` Plugin by Wesley Beary
* `scalable_acts_as_taggable` Plugin by Matthew Carr
* `has_many_polymorphs` Plugin


# Free Online Articles and Chapters About Tagging

* Oracle Tech Net Article: [Tagging with Oracle and Ruby on Rails](http://www.oracle.com/technology/pub/articles/kern-rails-tagging.html) by Matt Kern (June 2007)
* Practical Rails Social Networking Sites Book by Alan Bradburne (June 2007): Free Sample Chapter 10 - [Adding Tags to the Photo Gallery](http://www.apress.com/book/view/1590598415)

# That's it - Thanks!

