title: Web Components (w/ React) - Past, Present, Future


# Agenda

- Intro
  - `<HelloWorld/>`
  - Components in Components in Components
  - It's "Just" JavaScript - For Each Loops in Your Component Templates 
  - "Generic" Placeholder / Containers - Add Any Components / Children
- Inside React Web Components
- Classes, Classes, Classes 
  - Extends `React.Component`
  - State & Lifecycle - Seconds Elapsed: 47 
- High Speed Batched Updates - 60 Frames/Second (60 FPS)
  - Diffing Element Trees 
- React @ Facebook - "Enterprise" React
  - GraphQL, Relay, Flow 'n' Friends
- What's Next? 
  - I) Built-In Web Components Browser APIs
  - II) JavaScript w/ Types
  - III) More Fun - Fun(ctional) Languages
  - IV) Going Mobile - React Native for Android, iOS Apps
- Community - Meetups & Conferences



# Intro `<HelloWorld/>`

Let's make a `<HelloWorld/>` web component / element / tag.


The simplest (React) web component - 
a function returning a web snippet (HTML block)

```
function HelloWorld() {
  return <h1>Hello, World!</h1>;
}
```

or in "fun(k)y" JavaScript 6+ arrow-style:

```
const HelloWorld = ()=> <h1>Hello, World!</h1>;
```

That's it.



# Components in Components in Components

Nesting of Web Components => Web Component Trees

Example:

```
function Message( {name} ) {
  return <span>{ `Hello, ${name}!` }</span>;
}

function HelloWorld() {
  return <h1><Message name="World"/></h1>;
}
```

and (again) use it like:

```
<HelloWorld/>
```


# It's "Just" JavaScript - For Each Loops in Your Component Templates 

In Pseudo Code:

```
for link in links
  <li><a href={link.url}>{link.title}</a></li> 
end
```

No New Template Language. Use JavaScript 6+:

```
links.map( 
  link => <li><a href={link.url}>{link.title}</a></li> 
)
```

Example:

```
function LinkList( {links} ) {
  return( 
      <ul>
        {links.map( link => <li><a href={link.url}>{link.title}</a></li> )}
      </ul>);   
}
```

and use it like

```
const links =
[
 { title: "football.db - Open Football Data",              url: "https://github.com/openfootball" },
 { title: "beer.db - Open Beer, Brewery 'n' Brewpub Data", url: "https://github.com/openbeer"     },
 { title: "world.db - Open World Data",                    url: "https://github.com/openmundi"    }
];
...

<LinkList links={links}/>
```


# "Generic" Placeholder / Containers - Add Any Components / Children

```
function Page( {children} ) {
  return( 
     <div>
        <Header/>
        <div>
          {children}
        </div>
        <Footer/>
      </div>);
}
```

and use it like:

```
<Page>
  <div>
    <b>News 'n' Updates</b>
    <PostList posts={posts}/>
  </div>
  <div>
    <b>Links 'n' Bookmarks</b>
    <LinkList links={links}/>
  </div>  
</Page>
```


# Inside React Web Components


```
function HelloWorld() {
  return <h1>Hello, World!</h1>;
}
```

gets converted to "plain vanilla" JavaScript:

```
function HelloWorld() {
  return React.createElement( "h1", null, "Hello, World!" );
}
```

Try Babel online => [`babeljs.io/repl`](http://babeljs.io/repl) 



# Inside React Web Components (Continued)

``` jsx
function Message( {name} ) {
  return <span>{ `Hello, ${name}!` }</span>;
}

function HelloWorld() {
  return <h1><Message name="World"/></h1>;
}
```

gets converted to "plain vanilla" JavaScript:

``` js
function Message(_ref) {
  var name = _ref.name;
  return React.createElement( "span", null, "Hello, " + name + "!" );
}

function HelloWorld() {
  return React.createElement("h1", null,
           React.createElement(Message, { name: "World" })
  );
}
```


# Inside React Web Components (Continued)

``` jsx
function LinkList( {links} ) {
  return (
      <ul>
        {links.map( link => <li><a href={link.url}>{link.title}</a></li> )}
      </ul>);   
}
```

gets converted to "plain vanilla" JavaScript:

``` js
function LinkList(_ref) {
  var links = _ref.links;

  return React.createElement( "ul", null,
    links.map( function(link) {
      return React.createElement( "li", null,
               React.createElement( "a", { href: link.url }, link.title ));
    }));
}
```


# Classes, Classes, Classes - Extends React.Component


Use Classes for more "advanced" components. Example:

``` jsx
function HelloWorld() {
  return <h1>Hello, World!</h1>;
}
```

is the same as:


``` jsx
class HelloWorld extends React.Componet {

  render() {
    return <h1>Hello, World!</h1>;
  }
}
```


# Classes, Classes, Classes (Continued) - Seconds Elapsed: 47 

- Lifecyle e.g. `mount` - component turned on - and `unmount` - component turned off
- State e.g. `secondsElapsed` - a variable holding a number / counter, for example


``` jsx
class Timer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {secondsElapsed: 0};
  }

  tick() {
    this.setState((prevState) => ({
      secondsElapsed: prevState.secondsElapsed + 1
    }));
  }

  componentDidMount() {
    this.interval = setInterval(() => this.tick(), 1000);
  }

  componentWillUnmount() {
    clearInterval(this.interval);
  }

  render() {
    return (
      <div>Seconds Elapsed: {this.state.secondsElapsed}</div>
    );
  }
}
```


# High Speed Batched Updates - 60 Frames/Second (60 FPS)

React keeps a copy of the DOM (Document Object Model) in memory
like a "frame buffer";
uses a diffing algorithm 
to calculate changes (in the two trees) and batches
updates together in 60 frames per second.



# React @ Facebook - "Enterprise" React

- GraphQL & Relay  - Data Queries & Binding
- Flow             - Built-Time (Static) Data Flow Analysis & Type Checking
- ReasonML         - "Better" Objective Functional Language (Compiles to JavaScript)

and more.





# I) What's Next? Built-In  Web Components Browser APIs

New (Upcoming) Web Standard Building Blocks


### Custom Elements 

(`<element>`) - [W3C Spec](http://www.w3.org/TR/custom-elements)

### Shadow DOM

(hide DOM subtrees under shadow roots - `createShadowRoot()`) - [W3C Spec](http://www.w3.org/TR/shadow-dom)

### HTML Templates

(`<template>`) - [W3C Spec](http://www.w3.org/TR/html-templates)

### HTML Imports

(include and reuse HTML documents) - [W3C Spec](http://www.w3.org/TR/html-imports)




# II) What's Next? JavaScript w/ Types

Biggies

- TypeScript ★20098 (by Microsoft) -  JavaScript Extended w/ Types 
- Flow ★10693 (by Facebook)        -  Add Annotations for Type Checking
- Dart ★1038 (by Google)           -  New Language w/ (Optional) Types 

And Others.


# III) What's Next? More Fun - Fun(ctional) Languages

Biggies

- Elm ★3706 (by Evan Czaplicki 'n' friends) - Small (Pure) Functional Language for the Web
  - Based on Haskell (Simplified) 
- ReasonML ★2467 (by Facebook) - Larger (Pragmatic) Functional Language
  - Based on OCaml (New ReasonML Syntax Closer to JavaScript)

And Others.

Why Fun(ctional)?

- "Stronger" Types
  - No Null and No Undefined Possible
    - e.g. Elm Uses Maybe types with Just a and Nothing and Tuple Units e.g. `()`)
  - Lists must always be of the same type
  - If expressions must always return the same type (in if and else branch)
  - Case expressions must always cover all possible branches / values
  - And so on
- Immutability 
  - Cannot change/overwrite variables (change will always create a new variable)
  - Great for "high-speed" diffing web component/element trees ("just" compare node references)
  - Great for "time travel" debugging e.g. save or restore any state in time


# IV) What's Next? Going Mobile - React Native for Android, iOS Apps

Use React Native ★45909 (and JavaScript) to build native phone apps for Android an iOS.
Example:

```
import React, { Component } from 'react';
import { AppRegistry, Text } from 'react-native';

class HelloWorldApp extends Component {
  render() {
    return (
      <Text>Hello world!</Text>
    );
  }
}

AppRegistry.registerComponent('HelloWorldApp', () => HelloWorldApp);
```


# Community - Meetups & Conferences


Meetups In Vienna, Austria

- [React Vienna Meetup](https://meetup.com/ReactVienna) (FREE) - Monthly @ sektor5
- [Reason Vienna Meetup](https://meetup.com/Reason-Vienna) (FREE)  - Starting Up
- [Elm Vienna Meetup](https://www.meetup.com/Vienna-Elm-Meetup) (FREE) - Starting Up


Conferences

- [React Conf](http://conf.reactjs.org) (by Facebook)
   - 2017 - March 13 & 14 - in Santa Clara, CA
   - 2016 - February 22 & 23 - in San Francisco, CA
   - 2015 - January 28 & 29 - Facebook HQ, CA

In Europe

- [React Europe](https://www.react-europe.org)
  - 2017 - May 18 & 19 in Paris, France
  - 2016 - June 2 & 3 in Paris, France
  - 2015 - July 2 & 3 in Paris, France

Regional in 2017

- [React London 2017](https://react.london) - March 28
- [React Amsterdam 2017](https://react.amsterdam) - April 21
- [React Native Europe 2017](https://react-native.eu) - September 6 & 7 in Wroclaw, Poland
- [React.js Day 2017](http://2017.reactjsday.it) - October 6 in Verona, Italy
- [State.js React Conference 2017](https://statejs.com) - October 13 in Stockholm, Sweden
- [Reactive Conf](https://reactiveconf.com) - October 25t-27 in Bratislava, Slovakia


More [Upcoming Conferences »](https://facebook.github.io/react/community/conferences.html)



