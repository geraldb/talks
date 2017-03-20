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
function Message( props ) {
  return <span>{"Hello, "+props.name+"!"}</span>;
}

function HelloWorld() {
  return <h1><Message name="World"/></h1>;
}
```


# For Each Loops in your Component Template - It's "Just" JavaScript 

No New Template Language - Use JavaScript 6+:

```
links.map( link => 
 <li><a href={link.url}>{link.title}</a></li> 
)
```

Example:

```
function LinkList( props ) {
  const {links} = this.props;
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
function Page( props ) {
  return( 
     <div>
        <Header/>
        <div>
          {props.children}
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
function Message( props ) {
  return <span>{"Hello, "+props.name+"!"}</span>;
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

function HelloWorld() {
  return React.createElement(
    "h1",
    null,
    React.createElement(Message, { name: "World" })
  );
}
```


# Inside React Web Components (Continued)

``` jsx
function LinkList( props ) {
  const {links} = props;
  return (
      <ul>
        {links.map( link => <li><a href={link.url}>{link.title}</a></li> )}
      </ul>);   
}
```

gets converted to "plain vanilla" JavaScript:

``` js
function LinkList(props) {
  var links = props.links;

  return React.createElement(
    "ul",
    null,
    links.map(function (link) {
      return React.createElement(
        "li",
        null,
        React.createElement(
          "a",
          { href: link.url },
          link.title
        )
      );
    })
  );
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
class HelloMessage extends React.Componet {

  render() {
    return <h1>Hello, World!</h1>;
  }
}
```


# Classes, Classes, Classes - Seconds Elapsed: 47 (Continued)


Lifecyle (mount e.g. component turned on, unmount e.g. component turned off) 
and State e.g. secondsElapsed a variable holding a number / counter,
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







