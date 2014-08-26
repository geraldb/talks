title: Building Your Own Tags / Elements / Web Components


# Who's this guy?

- `football.db`   - Open Football Data; see [`github.com/openfootball`]()
- `beer.db`       - Open Beer, Brewery n Brewpubs Data; see [`github.com/openbeer`]()
- `world.db`      - Open World Data (Countries, Regions, Cities); see [`github.com/openmundi`]()



# Building Your Own Tags / Elements / Web Components

Agenda

* What's a custom tag? What's a custom element? What's a web component?
* Web Components Machinery - New Web Standard Building Blocks
* What's X-Tag? What's Polymer?
* Example 1 - `<beer-of-the-day>`  
* Example 2 - `<football-js event='at.2013/14'>`




# `<beer-of-the-day>` - Use or Build Your Own Tag / Element / Web Component

- What's a custom tag?
- What's a custom element?
- What's a web component?

Tag == Element == Web Component

Lets you build your own tags that you can use as easily as plain old `<div>` or `<span>` tags.

Example:

- Step 1: Import element definition into your page using an HTML import (`<link rel="import">`)
- Step 2: Use the new element; once imported the new element is a first-class HTML element; use like any other

~~~
<!-- Step 1: Import element -->
<link rel="import" href="football-js.html">

<!-- Step 2: Use the new element -->
<football-js event="at.2014/15"></football-js>
~~~

![](i/football-js.png)

or

~~~
<!-- Step 1: Import element -->
<link rel="import" href="google-map.html">

<!-- Step 2: Use the new element -->
<google-map lat="37.790" long="-122.390"></google-map>
~~~


# `<beer-of-the-day>` - Use or Build Your Own Tag / Element / Web Component - More Examples


~~~
<super-button></super-button>

<you-tube-video imfeelinglucky='rock me amadeus'></yout-tube-video>

<pdf-js src='wien.pdf'></pdf-js>

<marked-js>
## Markdown Renderer

* code in `JavaScript`
* realtime formatting

_many_ more goodies.
</marked-js>
~~~



#  "Real World" Paper Elements (Material Design)

~~~
<paper-button>
<paper-checkbox>
<paper-dialog-transition>
<paper-dialog>
<paper-fab>

~~~

Usage:

~~~
<paper-toolbar theme='light'>
  <paper-button icon='menu'></paper-button>
  <paper-button icon='refresh'></paper-button>
  <div flex>Toolbar: light theme</div>
  <paper-button icon='add'></paper-button>
</paper-toolbar>
~~~




# Web Components Machinery - New Web Standard Building Blocks

### New Browser Machinery

#### HTML Templates

(`<template>`) - [W3C Spec](http://www.w3.org/TR/html-templates/)

#### Custom Elements 

(`<element>`) - [W3C Spec](http://www.w3.org/TR/custom-elements/)

#### Shadow DOM

(hide DOM subtrees under shadow roots - `createShadowRoot()`) - [W3C Spec](http://www.w3.org/TR/shadow-dom/)

#### HTML Imports

(include and reuse HTML documents) - [W3C Spec](http://www.w3.org/TR/html-imports/)


### Higher-Level Machinery (w/ JavaScript Libraries)

#### MDV (Model Driven Views)

(`repeat='{{{{ greetings }}}}'`) - JavaScript Library



# Web Components Machinery - New Web Standard Building Blocks - Can I Use?

tl;dr:

Chrome 36+ ships all the web components machinery (built into the browser and turn on by default)

Firefox ships "lite" web components machinery
(shipping HTML Templates and Custom Elements; NOT shipping Shadow DOM and HTML Imports)


![](i/polymer-compatibility.png)

([Source: ](http://www.polymer-project.org/resources/compatibility.html))



# Web Components Machinery - New Web Standard Building Blocks - Can I Use?

### HTML Templates (`<template>`)  - [`caniuse.com/#feat=template`](http://caniuse.com/#feat=template)

- Shipped by Chrome (and Opera) - Stable
- Shipped by Firefox - Stable
- Shipped by Safari - Stable (?)
- Under consideration by Internet Explorer - Maybe in 2020?

### Custom Elements (`<element>`) - [`caniuse.com/#feat=custom-elements`](http://caniuse.com/#feat=custom-elements)

- Shipped by Chrome (and Opera) - Stable
- Shipped by Firefox - Some Bugs (Flag Required - `dom.webcomponents.enabled`)
- Under consideration by Internet Explorer - Maybe in 2020?
- Not supported by Safari - Who knows when or what?

### Shadow DOM (`createShadowRoot()`, scoped CSS) - [`caniuse.com/#feat=shadowdom`](http://caniuse.com/#feat=shadowdom)

- Shipped by Chrome (and Opera) - Stable
- Not supported by Firefox
- Under consideration by Internet Explorer - Maybe in 2020?
- Not supported by Safari - Who knows when or what?

<!--
- Shipped by Firefox - Some Bugs (Flag Required - `dom.webcomponents.enabled`)
-->

### HTML Imports (`<link rel='import' href='x-tag.html'>`) - [caniuse.com/#feat=imports](http://caniuse.com/#feat=imports)

- Shipped by Chrome (and Opera) - Stable
- Not supported by Firefox
- Under consideration by Internet Explorer - Maybe in 2020?
- Not supported by Safari - Who knows when or what?

<!--
- Shipped by Firefox - Some Bugs (Flag Required)
-->




# Web Compontens Machinery - Industry (Browser)  News / Support

### The Good -  Google, Firefox

Google I/O 2014 -  Paper Elements n Material Design  =>  Google wants you to build web apps (running anywhere desktop, tablet, phone)

Firefox OS   => Firefox wants you to build mobile web apps for Firefox OS (everything in HTML/CSS/JS). 


### The Bad 'n' Ugly - Apple, Microsoft

No news. No suprise. Apple wants you to build iOS apps (Cocoa/ObjectiveC)
and Microsoft wants you to build - suprise, suprise - Windows apps (Win32/.NET/WinRT) first.




# What's X-Tag? What's Polymer?


### X-Tag

Small JavaScript library by Mozilla - lets you use and build custom tags
for all modern browsers
(in theory; in practice looks like an orphan - last news update (blog post) dated
June 15th, 2013, that is, more than a year ago; github repo about two commits per month (!))

More info @ [`x-tags.org`](http://x-tags.org)



### Polymer - Tag Line => Welcome to the future

Library by Google - lets you use and build custom tags;
uses web components machinery built into modern browsers; (ugly) pollyfills for older browsers.

More info @ [`www.polymer-project.org`](http://www.polymer-project.org)




# Example 1 - `<beer-of-the-day>` Tag Definition


`beer-of-the-day.html`

~~~
<polymer-element name='beer-of-the-day'>
  <template>
    <span>I'm a fan of <b>Ottakringer Helles</b>
      and this is my Shadow DOM.
    </span>
  </template>
</polymer-element>
~~~


# Example 1 - `<beer-of-the-day>` Tag Usage

~~~
<html>
  <head>

    <!-- 1. Shim/polyfill missing web components machinery -->
    <script src='js/libs/polymer-min.js'></script>

    <!-- 2. Load custom tag (e.g HTML Imports in action) -->
    <link rel='import' href='beer-of-the-day.html'>

  </head>
  <body>

    <!-- 3. Use custom tag -->
    <beer-of-the-day></beer-of-the-day>

  </body>
</html>
~~~


# Example 2 - `<football-js>` Bundesliga Matchday Widget

What's `football.js`?

Football widgets in JavaScript using the football.db HTTP JSON(P) API

The old way in JavaScript. Usage Example:

~~~
<script src='js/football.js'></script>

<div id='bl'></div>

<script>
  Widget.create( '#bl', { event: 'at.2013/14' } );
</script>
~~~

The new way:

~~~
<link rel='import' href='football-js.html'>   <!-- HTML Imports -->

<football-js event='at.2013/14'></football-js>  <!-- custom tag use -->
~~~


# Example 2 - `<football-js>` - MDV (Model Driven Views)

### Matchday Data as JavaScript Objects

~~~
{
  "event":{"key":"at.2013/14","title":"Österr. Bundesliga 2013/14"},
  "round":{"pos":2,"title":"2. Runde","start_at":"2013/07/27","end_at":"2013/07/28"},
  "games":[{"team1_title":"SK Rapid Wien","team1_code":"RAP","team2_title":"SC Wiener Neustadt","team2_code":"WRN","play_at":"2013/07/27","score1":4,"score2":0},
           {"team1_title":"FC Admira Wacker","team1_code":"ADM","team2_title":"FC Wacker Innsbruck","team2_code":"FCW","play_at":"2013/07/27","score1":1,"score2":2},
           {"team1_title":"SV Ried","team1_code":"RIE","team2_title":"Wolfsberger AC","team2_code":"WAC","play_at":"2013/07/27","score1":1,"score2":0},
           {"team1_title":"FC RB Salzburg","team1_code":"RBS","team2_title":"FK Austria Wien","team2_code":"AUS","play_at":"2013/07/27","score1":5,"score2":1},
           {"team1_title":"SK Sturm Graz","team1_code":"STU","team2_title":"SV Grödig","team2_code":"SVG","play_at":"2013/07/28","score1":0,"score2":2}]
}
~~~

~~~
<template>
  <div class='football-widget'>
    <h3>
      {{{{ data.event.title }}}  -  {{{{ data.round.title }}}
    </h3>
  </div>
</template>
~~~


# Example 2 - `<football-js>` Widget - Nested Templates

~~~
<template>
  <div class='football-widget'>
    <h3>
      {{{{ data.event.title }}}}  -  {{{{ data.round.title }}}}
    </h3>

   <table>
   <template repeat='{{{{data.games}}}}'>
   <tr>
    <td>
      {{{{ play_at }}}}
     </td>
     <td style='text-align: right;'>
       {{{{ team1_title }}}} ({{{{ team1_code }}}})
     </td>

     <td>
       {{{{ score1 }}}} - {{{{ score2 }}}}
     </td>
     <td>
      {{{{ team2_title }}}} ({{{{ team2_code }}}})
     </td>
   </tr>
   </template>
   </table>  
</template>
~~~



# Example 2 - `<football-js>` - All Together Now

~~~
<polymer-element name='football-js' attributes='event'>
 
  <template>
   <style>
     .football-widget {
        border: 1px solid green;
        padding: 4px;
        margin: 10px;
     }
    </style>

   <div class='football-widget'>
    <h3>
      {{{{ data.event.title }}}}  -  {{{{ data.round.title }}}}
    </h3>

    <table>
   <template repeat='{{{{data.games}}}}'>
   <tr>
    <td>
      {{{{ play_at }}}}
     </td>
     <td style='text-align: right;'>
       {{{{ team1_title }}}} ({{{{ team1_code }}}})
     </td>

     <td>
       {{{{ score1 }}}} - {{{{ score2 }}}}
     </td>
     <td>
      {{{{ team2_title }}}} ({{{{ team2_code }}}})
     </td>
   </tr>
   </template>
    </table>

 </div>
 </template>
  
  <script>
    Polymer('football-js', {
      // event: 'euro.2012',
      data: {
        "event":{"key":"at.2013/14","title":"\u00d6sterr. Bundesliga 2013/14"},
        "round":{"pos":2,"title":"2. Runde","start_at":"2013/07/27","end_at":"2013/07/28"},
        "games":[{"team1_key":"rapid","team1_title":"SK Rapid Wien","team1_code":"RAP","team2_key":"wrneustadt","team2_title":"SC Wiener Neustadt","team2_code":"WRN","play_at":"2013/07/27","score1":4,"score2":0,"score1ot":null,"score2ot":null,"score1p":null,"score2p":null},
                 {"team1_key":"admira","team1_title":"FC Admira Wacker","team1_code":"ADM","team2_key":"innsbruck","team2_title":"FC Wacker Innsbruck","team2_code":"FCW","play_at":"2013/07/27","score1":1,"score2":2,"score1ot":null,"score2ot":null,"score1p":null,"score2p":null},
                 {"team1_key":"ried","team1_title":"SV Ried","team1_code":"RIE","team2_key":"wac","team2_title":"Wolfsberger AC","team2_code":"WAC","play_at":"2013/07/27","score1":1,"score2":0,"score1ot":null,"score2ot":null,"score1p":null,"score2p":null},
                 {"team1_key":"salzburg","team1_title":"FC RB Salzburg","team1_code":"RBS","team2_key":"austria","team2_title":"FK Austria Wien","team2_code":"AUS","play_at":"2013/07/27","score1":5,"score2":1,"score1ot":null,"score2ot":null,"score1p":null,"score2p":null},
                 {"team1_key":"sturm","team1_title":"SK Sturm Graz","team1_code":"STU","team2_key":"groedig","team2_title":"SV Gr\u00f6dig","team2_code":"SVG","play_at":"2013/07/28","score1":0,"score2":2,"score1ot":null,"score2ot":null,"score1p":null,"score2p":null}]
       } 
    });
  </script>
</polymer-element>
~~~


# Example 2 - `<football-js>` - Usage

~~~
<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>football.js</title>
    <script src='js/libs/polymer.min.js'></script>
    <link rel='import' href='football-js.html'>
  </head>
  <body>
    <football-js event='at.2013/14'></football-js>
  </body>
</html>
~~~

![](i/football-js.png)

That's it.



# Thanks - Questions? Comments?

## Learn More

- [webcomponents.org]()
- []




# Appendix: Web Components Machinery - New Web Standard Building Blocks - Sources

### Chrome

- [`chromium.org/blink/web-components`](http://www.chromium.org/blink/web-components)

### Internet Explorer

- [`status.modern.ie`](http://status.modern.ie)

Site source on github (creative commons licensed) build w/ Node.js;
modern IE ?! - is this possible? - IE and modern?

### Apple

- Secrets, Secrets, Secrets - Who knows when or what? 



