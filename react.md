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
  return 
      <ul>
        {links.map( link => <li><a href={link.url}>{link.title}</a></li> )}
      </ul>;   
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
  return 
     <div>
        <Header/>
        <div>
          {props.children}
        </div>
        <Footer/>
      </div>
};
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








