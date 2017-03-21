title: Web Components (w/ React) - Past, Present, Future





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


# It's "Just" JavaScript - For Each Loops in your Component Template 

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
 { title: "football.db - Open Football Data",
   url:   "https://github.com/openfootball" },
 { title: "beer.db - Open Beer, Brewery 'n' Brewpub Data",
   url:   "https://github.com/openbeer" },
 { title: "world.db - Open World Data",
   url:   "https://github.com/openmundi" }
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
function Message(props) {
  return React.createElement( "span", null, "Hello, " + props.name + "!" );
}

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
    links.map(function (link) {
      return React.createElement( "li", null,
               React.createElement( "a",{ href: link.url }, link.title ));
    }));
}```

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


# Classes, Classes, Classes - Seconds Elapsed: 47 (Continued)


Lifecyle (mount e.g. component turned on, unmount e.g. component turned off) 

and

State e.g. secondsElapsed - a variable holding a number / counter,
for example.


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







