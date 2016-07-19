title: Facebook App Development w/ Ruby on Rails
gradient-colors: #0e1f5b #3b5998

# Q: How can I create a Friends Badge using the Facebook API?

![](i/rfacebook-friendsbadge.png)

* Step 1: Get your friends' ids (calling the `friends.get` Facebook API using rfacebook).
* Step 2: Get all your friends' square profile pic links (calling `users.getInfo` asking for the `pic_square` field).

### [Facebook API Test Console](http://developer.facebook.com/tools.php?api) - Demo


# Q: How can I create a Friends Badge using the Facebook API? Continued

* Step 1: Get your friends' ids (calling the `friends.get` Facebook API using rfacebook).
* Step 2: Get all your friends' square profile pic links (calling `users.getInfo` asking for the `pic_square` field).

```
def badge

  # get your friends' ids
  friendsIds = fbsession.friends_get.uid_list

  # use your friends' ids to get square profile pic links
  friendsInfo = fbsession.users_getInfo(
                   :uids => friendsIds,
                   :fields => ["pic_square"] )

  # store all your friends profile pic links
  @friendsPics = []
  friendsInfo.user_list.each do |user|
    @friendsPics << user.pic_square  unless user.pic_square.blank?
  end  

end
```

# Q: How can I create a Friends Badge using the Facebook API? Continued

And add a matching view (web page template):

```
<h3>Open Web Friends Badge Sample</h3>

<style type="text/css">
div.badge  { border: 4px solid #3b5998; }
img.badge  { border-bottom: thin solid black;
             border-right: thin solid black; }
</style>

<div class="badge">

<% @friendsPics.each do |pic| -%>
  <%= image_tag pic, :class => 'badge'-%>
<% end %>

</div>
```

That's all.

![](i/rfacebook-friendsbadge.png)


# 10-Minute Quick Start Guide for Facebooker

Create a Facebook app in 7 easy steps from scratch
displaying a Twitter-like "What are you doing?" status message.

1. Create a Rails application
2. Install the Facebooker Rails plugin
3. Log on to Facebook and set up a new application
4. Add your API key and secret to the `facebooker.yml` configuration file
5. Create Rails controller (and view scaffolds)
6. Configure default route and remove public/index.html page
7. Use Facebooker to get your name, profile pic and status


# Step 1: Create a Rails application

Note: Facebook applications require unique names. This Quick Start Guide uses `openweb`.
Replace `openweb` everywhere with your own unique name.

Use Rails to create your web application. Type on the command line:

```
$ rails openweb
```

# Step 2: Install the Facebooker Rails plugin

To turn your Rails application into a Facebook application
add the Facebooker Rails plugin. Change into your Rails folder and type on the command line:

```
$ cd railsadvance
$ script/plugin install http://facebooker.rubyforge.org/svn/trunk/facebooker/
```

# Step 3: Log on to Facebook and set up a new application

Log on to Facebook and set up your new application. Browse to the Facebook Developers
page (`facebook.com/developers`) and click "Set Up New Application".

Fill in the new application form:

* Use "Open Web on Facebook" for your Application Name.
* Check that you have read and agree to the terms of service.
* Set your Callback URL to http://localhost:3000/ (note: do not forget the trailing slash /)
* Set the Canvas Page URL to http://apps.facebook.com/openweb/
* We build a web application using inline frames (iframe). Check (X) Use iframe and (X) Website for Application Type.

![](i/rfacebook-config2.png)

That's it. Hit submit to get your API key and secret created.
Write down your API key and secret for the next step.

* API Key: `47013ae80a97cc151707961fc03bc9bf`
* Secret: `7f296d598f9c79a5f439289e1240dc69`


# Step 4: Add your API key and secret to the facebooker.yml configuration file

The Facebooker plugin creates the `facebooker.yml` configuration file during installation
in the config folder. Open the `config/facebooker.yml` file and add your own API key, secret
and canvas page name:

```
development:
    key: 47013ae80a97cc151707961fc03bc9bf
    secret: 7f296d598f9c79a5f439289e1240dc69
    canvas_page_name: /openweb/
```


# Step 5: Create Rails controller and (view scaffolds)

Lets create a Rails controller with an index action/view. Type on the command line:

```
$ script/generate controller workshop index
```

# Step 7: Use Facebooker to get your name, profile pic and status

Add some code to our workshop controller and views. Add a `ensure_authenticated_to_facebook`
to the top of your workshop controller to ask Facebookers using your application to login.

`app/controllers/workshop_controller.rb`:

```
class WorkshopController < ApplicationController

  ensure_authenticated_to_facebook

  def index
  end

end
```


# Step 7 Continued: Tap into Facebook using the API

Facebooker hides all the Facebook API requests.
To get your name, profile pic and status using the Facebook API
just store a referene to the Facebooker user from the Facebook session
in `@user` in your controller

`app/controllers/workshop_controller.rb`:

```
def index
  @user = session[:facebook_session].user
end
```

and use `@user.name`, `@user.pic_square` and `@user.status.message` in your view


`app/views/workshop/index.html.erb`:

```
<h1>What are you doing?</h1>

<%= image_tag @user.pic_square, :align => 'left' %>

<b><%= @user.name %></b>
<%= @user.status.message %>
```


# That's it - Log in to the Open Web Facebook application

Start up your web server (`script/server`)
and surf with your browser to `http://localhost:3000/`.
If all works you will get redirected to Facebook asking you to login to your Facebook application.

![](i/rfacebook-login.png) 

To have your inline frame Facebook application hosted inside Facebook
surf to `http://apps.facebook.com/openweb` Voila! 

![](i/rfaceboo-status.png)


# Facebook and Ruby

What's rfacebook? What's Facebooker?

Open-Source Ruby libraries/gems for using the Facebook API

### What's the difference between rfacebook and Facebooker?

In a nutshell rfacebook is more or less a straight Ruby port of the Facebook PHP client libraries. Facebooker wraps the Facebook API into classes and methods with the goal of designing an API that follows the Ruby Way and is, thus, more intuitive for a typical Ruby developer to pick up and work with than a straight port from PHP.

Chad Fowler - the Facebooker project lead - has a posting titled "Writing APIs to Wrap APIs" discussing the difference in design and goals of rfacebook vs. Facebooker.

Paul Prescod of (Are You Normal? fame) comments: I prefer the rfacebook design because:

* It makes it crystal clear exactly which methods are doing network transactions, and network transactions are really, really expensive (especially into Facebook!).
* It is easy to read Facebook's documentation and see exactly how that applies to rfacebook. It isn't as clear for Facebooker.

Source: [facebook for ruby // questions & answers / more site](http://rfacebook.wordpress.com)


# Facebook Application Types

* Web Applications
  * Running Inside Inline Frames (iframes) using "Classic" HTML/JavaScript/CSS
  * Running Inside Facebook Canvas using FBML (Facebook Markup Language) and FBJS (Facebook JavaScript)

<!-- -->

* Desktop Applications


# What's FBML?

FBML = Facebook Markup Language

```
<fb:dashboard>
   <fb:create-button href="<%= url_for(
     :controller => 'notes', :action => "new", :only_path => true) %>">
       Add a New Goal
    </fb:create-button>
</fb:dashboard>

<fb:tabs>
 <fb:tab_item
    href="<%= url_for(
      :controller => 'notes', :action => "index", :only_path => true) %>"
        title="All Goals" />
</fb:tabs>

<% if flash[:notice] %>
  <fb:success>
     <fb:message><%= flash[:notice] %></fb:message>
  </fb:success>
<% end %>
```

![](i/showgoal.png)


# What's FBJS? What's FQL?

FBJS = Facebook JavaScript (Sandboxed JavaScript)

FQL = Facebook Query Language (SQL-like Database Query Language)

```
SELECT name, pic FROM user WHERE uid=211031 OR uid=4801660
```


# Learn more about Facebook Development w/ Ruby

Online Tutorials

* Facebooker Tutorial on Facebook by David Clements
* More see `rfacebook.wordpress.com`

Books

* Pragmatic Programmer Book (Available in Beta - PDF Download) Developing Facebook Apps with Rails (Facebooker) by Michael J. Mangino

Getting Help

* [Facebook for Ruby Forum/Mailing List](http://groups.google.com/group/rfacebook)
* [Facebooker Mailing List on RubyForge](http://rubyforge.org/mailman/listinfo/facebooker-talk)
* [Official Facebook Forum](http://forum.developers.facebook.com)


# What's Bebo?

[Bebo](http://bebo.com) is (another) social network that has licensed the Facebook API.
Thus, Facebook widgets run on Bebo with little change.

What's Beboist? 

Open-Source Ruby library/gem for using the Facebook API w/ Bebo


# What's Lovd by Less? What's Insoshi?

What's Lovd by Less?

Open Source Social Network with Activity (News) Feed in Ruby on Rails.

What's Insoshi

Another Open Source Social Network based originally on the RailsSpace
book and code. Now a venture funded startup.


# That's it. Thanks.

Questions? Comments?  

See Facebook applications live in action at today's App Nite starting at 5ish
including British Columbia's #1 Facebook hit Scratch and Win.

The $uper Rewards folks invite you to free beer (1) at the 
Vancouver Facebook Developers Pub Nite @ Steamworks Brewery. 

1: Redeem your Hockey Pool Pro Puck Points or sign-up for Win a Free 
Wii Contest or install the Glitterati Toolbar to qualify. Just kidding ;-)   
