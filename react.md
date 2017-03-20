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

No New Template Language - Use JavaScript 6+

Example:

```
```







