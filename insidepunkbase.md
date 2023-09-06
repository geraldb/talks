# Vienna.js  Talk Notes  - Inside Punkbase - SQLite (SQL.js) In Action


## What is SQLite?

<https://sqlite.org>

The world's most popular "server-less" database.

"Server-less?!"  -  "Old School" "Server-less" - it's a library - no client/server database -
that is,  sql queries done via "local" function calls!

Source code in C.  Public domain (that is, no rights reserved).




## What is SQL.js?

<https://sql.js.org>

SQLite compiled to JavaScript that runs - yes - in your browser (web page) "server-less"!

Source code in C compiled via Emscripten (to WebAssembly, really ;-).

Two Parts:

- "Booter/Init/Loader" Script in "Plain" Javascript  e.g <https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.6.1/sql-wasm.js> about ~100k.
- WebAssembly "Blob"  e.g. <https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.6.1/sql-wasm.wasm> about ~1MB.


## What are punk (pixel) heads?

For some background see [**Inside the Billion Dollar $$$ (Crypto) Punk (Pixel) Heads »**](https://github.com/cryptopunksnotdead/cryptopunks/tree/master/insidepunks)



## What is punkbase.db?

Single-File SQLite Database, that is, punkbase.db - about ~5MB

with 10 000 records of the punk pixel art collection
with all metadata (e.g. base type, skin tone, gender, head, eyes, etc.)


## What is punkbase?

Query punkbase.db with SQL online using "server-less" web page
thanks to SQL.js

Demo <https://cryptopunksnotdead.github.io/punkbase/>



## What is punkbase.db?  Continued

The "artbase.db" database schema:

```sql
CREATE TABLE metadata (
    -- general
    id         INTEGER      PRIMARY KEY AUTOINCREMENT
                            NOT NULL,
    base       VARCHAR      NOT NULL,
    gender     VARCHAR      NOT NULL,
    skin_tone  VARCHAR,
    count      INTEGER      NOT NULL,
    -- attributes
    blemishes  VARCHAR,
    head       VARCHAR,
    beard      VARCHAR,
    eyes       VARCHAR,
    nose       VARCHAR,
    mouth      VARCHAR,
    mouth_prop VARCHAR,
    ears       VARCHAR,
    neck       VARCHAR,

    -- image
    image      VARCHAR      NOT NULL
);
```

Yes, it's just a single table.


What about the pixel artwork / images?

The images are stored "inline"
in the image column in 24×24 px in the PNG (Portable Network Graphics)
format with transparent background and base64-encoded
and starting with `image:data`. Example - punk no. 0:


```
data:image/png;base64,
iVBORw0KGgoAAAANSUhEUgAAABgAAAAYBAMAAAASWSDLAAAAFVBMVEUAAAAA
AABQfDNdi0NfHQmui2H/9o4qIUCBAAAAB3RSTlMA////////pX+m+wAAAHhJ
REFUeJxjYkACTORy2NLSEmAcNiCLDUVZApQDoj+wwWQ+MPz/D1f2/8MHmGlA
Bf8RRv//e4HhLEKZAMNHGIeRRYHBCdU5CXDOAZgBCQwCDA5gQ4GcWQyM/98z
/poFUTZrPaPg7wVwAw4w7EeYZrPnA4LD8h/T2wA0qh2r1DURDgAAAABJRU5E
rkJggg==
```



## What is SQL.js? Continued  - Usage - Inside Punkbase


artbase.js:

``` js
class Artbase {

  async init( database = "artbase.db" ) {

    console.log( "fetching sql.js..." );
    await loadScript( 'https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.6.1/sql-wasm.js' );
    console.log( "done fetching sql.js" );

    const sqlPromise = initSqlJs({
      locateFile: file => "https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.6.1/sql-wasm.wasm"
    });

    const dataPromise = fetch( database ).then(res => res.arrayBuffer());
    const [SQL, buf] = await Promise.all([sqlPromise, dataPromise])
    this.db = new SQL.Database(new Uint8Array(buf));
  }

  // and "server-less" function call for sql queries via exec(ute)
   query( sql ) {
      return this.db.exec( sql );
   }
}
```

That's it.


Usage

```js
const artbase = new Artbase();
await artbase.init( "punkbase.db" );

const result = artbase.query( `select *
                               from   metadata
                               where  gender    = 'f' AND
                               head      = 'Wild Blonde' AND
                               skin_tone = 'Light' AND
                               blemishes = 'Mole'` );
//=>  3 punkettes ("marilyns")
```

resulting in:

```json
[
  {
    "columns": [
      "id",
      "base",
      "gender",
      "skin_tone",
      "count",
      "blemishes",
      "head",
      "beard",
      "eyes",
      "nose",
      "mouth",
      "mouth_prop",
      "ears",
      "neck",
      "image",
      "created_at",
      "updated_at"
    ],
    "values": [
      [
        3725,
        "Human",
        "f",
        "Light",
        3,
        "Mole",
        "Wild Blonde",
        null,
        "Green Eye Shadow",
        null,
        null,
        null,
        null,
        null,
        "data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAABgAAAAYBAMAAAASWSDLAAAAGFBMVEUAAAAAAABchTlumE1xEBCRdlbbsYD/9o6wz3knAAAACHRSTlMA/////////9XKVDIAAACnSURBVHichZAxDsIwDEV/IxWvPQMn4B4sSExsrN2y5QxsuQHdGcgBGDgJV0gYTREmTqS2EgPenv1s2TZYhPkDBDeBY5e5goMF+goET2ByVsF6qxn2GUilHF61nEGM2lk0xA4CtkUDUpMG0h7G+X2XmxqlIh2edQPG2G6w13FaOS5G80oQcOjrOq8GOwxUwHt8HsCpAEsw6zFNxwVc50u3lzRDK78P+QL2JDO9McBF3gAAAABJRU5ErkJggg==",
        "2022-08-29 16:44:23.877084",
        "2022-08-29 16:44:23.877084"
      ],
      [
        7645,
        "Human",
        "f",
        "Light",
        4,
        "Mole",
        "Wild Blonde",
        null,
        "Clown Eyes Blue",
        null,
        "Purple Lipstick",
        null,
        null,
        null,
        "data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAABgAAAAYBAMAAAASWSDLAAAAG1BMVEUAAAAAAAAoWLEpPmQsUZWRdlbNAMvbsYD/9o7EIpG6AAAACXRSTlMA//////////83ApvUAAAAoElEQVR4nIWQIRICMQxFfzsDRMIMlrtwBDQKiUSujKxjNYbeCG7SyrA7UJqW3WUGQdxLXtufWnyV/QMEN4ITl7mCAwNNBQIThBwrMLN2RIFUysWq5Q5C0JNFQ1giDRr6aHxLCoKzvaeNGmXyjLjVBILObLEeJqcZsPtcLYsXPI5NjfOwOKCl+g4jhZI0gyRvVl0cl/O4TpvuL3GCefr9kDdLKjPPD7rOeQAAAABJRU5ErkJggg==",
        "2022-08-29 16:47:06.686801",
        "2022-08-29 16:47:06.686801"
      ],
      [
        9712,
        "Human",
        "f",
        "Light",
        4,
        "Mole",
        "Wild Blonde",
        null,
        null,
        null,
        null,
        "Vape",
        "Earring",
        null,
        "data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAABgAAAAYBAMAAAASWSDLAAAAIVBMVEUAAAAAAAAAQP9ZWVlxEBCRdlambizSnWDbsYD/2Sb/9o5w16fTAAAAC3RSTlMA/////////////312fdUAAACtSURBVHichZA9DsIwDIWfClKFpxwBehmukZUFNSMXQOqWqXPmbIxcJvQG7WQGoCVO+KnEgLfP79l6doFZFX+A4D/g2UfO4OGANgNBE5i8E3DaSYd1BBJTLC222EEIMplsCGuMYJdsQFd0NckMYz92YxBHUm4Kp5yAcSgVjKwTRc1W8+oBi2Ob41wXqBtjlIDWuJ+rapMUnuxyG8LlfZzF1PfDC3bN8D27nH4f8gRxzTfh5odm/AAAAABJRU5ErkJggg==",
        "2022-08-29 16:48:12.730838",
        "2022-08-29 16:48:12.730838"
      ]
    ]
  }
]
```





## What is SQL.js? Continued  - Usage - Inside Punkbase  - "No-Code" SQL Queries

<https://github.com/pixelartexchange/artbase.js/blob/master/artbase.js>



## More Examples

[**8-Bitbase**](https://pixelartexchange.github.io/artbase.js/8bitbase) (8888 max.) - query 8bit metadata & images (24×24px) via sql & more - download all-in-one single-file sqlite database  (~4Mb)

[**Goblinbase**](https://pixelartexchange.github.io/artbase.js/goblinbase) (4444 max.)  - query goblin town metadata & images (48×48px) via sql & more - download all-in-one single-file sqlite database (~4MB)


[**Moonbirdbase**](https://pixelartexchange.github.io/artbase.js/moonbirdbase) (10000 max.)  - query moonbird (pixel owl) metadata & images (42×42px) via sql & more - download  all-in-one single-file sqlite database (~9MB)


[**Pudgybase**](https://pixelartexchange.github.io/artbase.js/pudgybase) (5000 max.) - query pudgy (penguin) metadata & images (28×28px) via sql & more - download all-in-one single-file sqlite database (~3MB)

[**Pudgypunkbase**](https://pixelartexchange.github.io/artbase.js/pudgypunkbase) (8888 max.) - query pudgy (penguin) punk metadata & images (24×24px) via sql & more - download all-in-one single-file sqlite database (~5MB)

[**Unemployablebase**](https://pixelartexchange.github.io/artbase.js/unemployablebase) (5000 max.) - query unemployable metadata & images (24×24px) via sql & more - download all-in-one single-file sqlite database  (~2MB)


and more



## Questions? Comments?



## Credits - Thank Yous

Original idea and source code by Skøgard (of Factoria Fame).

See <https://github.com/mixtape-network/moonbirdbase>,
<https://github.com/mixtape-network/mferbase>,
<https://github.com/mixtape-network/creyziebase>. and others.




## Breaking News - SQLite Gets Offical "First-Class" WebAssembly & JavaScript Support

see <https://sqlite.org/wasm>

Announced in October 2022 the about page reads:

> About the sqlite3 WASM/JS Subproject
> WebAssembly, a.k.a. WASM, is a standard defining a low-level programming language suitable
> (A) as a target for cross-compilation from many other languages and 
> (B) for running via a virtual machine in a browser.
> Designed with scriptability via JavaScript in mind, it provides a way to compile C code (among others) to WASM and script 
> it via JavaScript with relatively little friction despite the vast differences between JavaScript and C.
>
> Folks have been building sqlite3 for the web since as far back as 2012
> but this subproject is the first effort "officially" associated with the SQLite project, 
> created with the goal of making WASM builds of the library first-class members of the family of supported SQLite deliverables.
