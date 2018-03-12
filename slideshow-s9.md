title: Build Presentations / Talks with (Structured) Text


# Build (Online) Presentations / Talks

(and Handout Notes)

with (Structured) Text with Formatting in Markdown Markup Conventions and

(Off-the-Shelf Open Free) Slide Design Themes (Reveal.js, Bespoke.js, Impress.js, Shower.js, S6, ...) and

the Jekyll (Static) Website Compiler



# The Biggies

- Microsoft PowerPoint
- Apple Keynote
- ...

What's wrong?


# What's the World Wide Web (WWW)?

Publish your presentations / slide decks on the internets as:

- binary blobs or
- PDF (Portable Document Format) or
- Flash Movie
- every slide as a raster (pixel) image or
- ...

Export to HTML (Hypertext Markup Language? (Bonus: With beautiful (structured) markup?)


# Presentations / Slide Decks in HTML

``` html
<article>
  <section>
    ...
  </section>
  <section>
    ...
  </section>
  ...
</article>
```

# Presentations / Slide Decks in HTML

``` html
<article>
  <section>
    <h1>Build Presentations / Talks with (Structured) Text</h1>
    <p>With (Off-the-Shelf Open Free) Slide Design Themes
       (Bespoke.js, Reveal.js, Impress.js, Shower.js, S6, ...) and
       the Jekyll (Static) Website Compiler
    </p>
  </section>
  <section>
    <h1>The Biggies</h1>
    <ul>
     <li>Microsoft PowerPoint</li>
     <li>Apple Keynote</li>
     <li>...</li>
    </ul>
    <p>What's wrong?</p>
  </section>
  ...
</article>
```


# Write in (Structured) Text

```
# Build Presentations / Talks with (Structured) Text

With (Off-the-Shelf Open Free) Slide Design Themes
(Bespoke.js, Reveal.js, Impress.js, Shower.js, S6, ...) and
the Jekyll (Static) Website Compiler


# The Biggies

- Microsoft PowerPoint
- Apple Keynote
- ...

What's wrong?
```


# Best of Both World?  Easy to Write and Beautiful Design / Layout

Write in Text and (auto-)generate beautiful hypertext markup language (HTML).

How to turn your notes into presentation / slide deck?

Use (off-the-shelf open free) design theme.

Q: Who are the Biggies on the Web?



# The Biggies (Web Edition)

- **Reveal.js** ★39 277 - github: [hakimel/reveal.js](https://github.com/hakimel/reveal.js) by Hakim El Hattab et al
  - Official Tagline - "The HTML Presentation Framework"
- **Bespoke.js** ★4 331 - github: [bespokejs/bespoke](https://github.com/bespokejs/bespoke) by Mark Dalgleish et al
  - Official Tagline - "DIY Presentation Micro-Framework"
- **Shower.js** ★3 925  - github: [shower/shower](https://github.com/shower/shower) by Vadim Makeev et al
  - Official Tagline - "Shower HTML Presentation Engine"
- **Impress.js** ★33 207 - github: [impress/impress.js](https://github.com/impress/impress.js) by Bartek Szopka et al
  - Official Tagline - "Presentation framework based on the power of CSS3 transforms and transitions in modern browsers and inspired by the idea behind prezi.com"
- ...

And **S6** ★96 - github: [slidekit/s6](https://github.com/slidekit/s6) - Made in Austria :-) by Gerald Bauer et al - Bespoke-compatible since V2.0



## Best of All Worlds :-) - Slideshow (S9) Design Themes

What's Slideshow (S9)?

**Slideshow (S9)** ★143 - github: [slideshow-s9/slideshow](https://github.com/slideshow-s9/slideshow) by Gerald Bauer et al

command line tool that lets you build presentations / talks from (structured) text
with jekyll-compatible design themes



## Best of All Worlds :-) - Slideshow (S9) Design Themes (Cont.)

Slideshow (S9) theme / template packs include:

- **S6**  - github: [slideshow-templates/slideshow-s6-blank](https://github.com/slideshow-templates/slideshow-s6-blank)
- **Reveal.js** - github: [slideshow-templates/slideshow-reveal.js](https://github.com/slideshow-templates/slideshow-reveal.js)
- **Shower.js**  - github: [slideshow-templates/slideshow-shower](https://github.com/slideshow-templates/slideshow-shower)
- **Bespoke.js** - github: [slideshow-templates/slideshow-bespoke.js](https://github.com/slideshow-templates/slideshow-bespoke.js)
- **Impress.js** - github:  [slideshow-templates/slideshow-impress.js](https://github.com/slideshow-templates/slideshow-impress.js)
- ...


## Getting Started w/ Slideshow (S9)

Slideshow (S9) Quick Starter Kit - github: [slideshow-s9/slideshow-starter](https://github.com/slideshow-s9/slideshow-starter)

What's Included?

- A first sample talk (see `sample1.text`)
- A second sample talk (see `sample2.text`) incl. some macros/helpers e.g. `left`, `right`, `step`, etc.


## Getting Started w/ Slideshow (S9) (Cont.)

Use the Slideshow (S9) command line tool to build
a web page (e.g. `sample1.html` or `sample2.html`)
that is an all-in-one-page handout and a live slide show all at once.

```
$ slideshow build sample1.text

=> Preparing slideshow 'sample1.html'...
=> Done.
```

And one more time.

```
$ slideshow build sample2.text

=> Preparing slideshow 'sample2.html'...
=> Done.
```

That's it. Open up your slide show `sample1.html` or `sample2.html` in your browser
(Firefox, Chrome, Safari, Edge and others)
and hit F11 to switch into full screen projection
and hit the space bar or the right arrow, down arrow
or page down key to flip through your slides.
