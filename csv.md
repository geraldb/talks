title: Mining for Gold Using the World's #1 and Most Popular Data Format




# Trivia Quiz - The World's Most Popular Data Format?

- [ ]  XML
- [ ]  YAML
- [ ]  JSON
- [ ]  CSV
- [ ]  CSV ♥ JSON
- [ ]  TAB
- [ ]  PDF (!?)
- [ ]  HTML
- [ ]  Other, Please Tell




# Data Format Timeline / History - Past, Present, Future


1970s - Pre Personal Computer (PC) Age

- [CSV = Comma-Separated Values](https://en.wikipedia.org/wiki/Comma-separated_values)
- [TAB = Tab(ulator)](https://en.wikipedia.org/wiki/Tab_key)


1990s - Internet / Web Age

- [PDF = Portable Document Format](https://adobe.com/pdf) - 1993¹-  (25 years ago)
- [HTML = Hypertext Markup Language](https://www.w3.org/html/) - 1993-
- [XML = Extensible Markup Language](https://www.w3.org/XML/) - 1996-

2000s  

- [YAML = YAML Ain't Markup Language (was: Yet Another Markup Language)](http://yaml.org) - 2001-  (17 year ago)
- [JSON = JavaScript Object Notation](https://json.org)  - 2001-

Now

- [CSV ♥ JSON](https://github.com/csv11/csv-json) - 2018-  (3 days ago)
- [CSV ♥ YAML](https://github.com/csv11/csv-yaml) - 2018-  (upcoming :-))


Notes:

¹: First release of PDF as an open standard on July 1, 2008.





# Data Formats @ Statistics Austria


![](i/opendata-statistik-austria.png)

(Source: [`data.statistik.gv.at`](http://data.statistik.gv.at/web/catalog.jsp))

It's all CSV :-) with meta data in JSON. Example:

```
JAHR;GCD;GEM_NAME;BEV_ABSOLUT;BEV_UNTER15;BEV_UEBER65;AUSL_STAATSB;...
2011;10101;Eisenstadt;13.101;13,8;18,2;9,4;...
2011;10201;Rust;1.896;13,8;22,2;4,7;...
2011;10301;Breitenbrunn am Neusiedler See;1.902;12,1;22,4;5,6;...
2011;10302;Donnerskirchen;1.742;12,3;20,2;3,8;...
2011;10303;Großhöflein;1.929;12,9;17;3,4;...
...
```

Note: The semicolon `;` is the value separator and for numbers
the dot `.` is the thousands separator (!) e.g. `13.101`
and the comma `,` is the decimal separator/point e.g. `13,8` :-).


# Triva Quiz: Who Invented the Space Character in Writing (and When)?

- [ ] The Greeks in the Golden Age
- [ ] The Romans in the Age of Empire (What have the Romans ever done for us?)
- [ ] The Irish Monks in the Middle Ages 
- [ ] The Frankish Alchemists in the Charlemagne's Age (800-)



# Triva Quiz: Who Invented the Space Character in CSV (and When)?

- [ ] [CSV v1.1](https://csv11.github.io) in 2018 :-)

Still waiting :-). Yes, you can. Let's make it happen.
Pretty printed version with space (CSV v1.1). Example:

```
JAHR; GCD;   GEM_NAME;                 BEV_ABSOLUT; BEV_UNTER15; BEV_UEBER65; AUSL_STAATSB; ...
2011; 10101; Eisenstadt;                    13.101;        13,8;        18,2;          9,4; ...
2011; 10201; Rust;                           1.896;        13,8;        22,2;          4,7; ...
2011; 10301; Breitenbrunn am Neusiedler See; 1.902;        12,1;        22,4;          5,6; ...
2011; 10302; Donnerskirchen;                 1.742;        12,3;        20,2;          3,8; ...
2011; 10303; Großhöflein;                    1.929;        12,9;          17;          3,4; ...
...
```







# Data Formats @ Open Gov Data Austria

![](i/opendata-austria-gov.png)

(Source: [`data.gv.at`](https://www.data.gv.at/suche/))


- How many datasets? 20_383 datasets
- How many in CSV? 19_201 (!) => ~95%

Note: [Offenerhaushalt.at](https://www.offenerhaushalt.at) (= Open Spending)
17_424 (!) datasets (that, is ~85% of all datasets)
for budgets (money, money, money) - all in CSV.

2nd largest open data publisher - the city of Vienna
- with 440 datasets (that is, ~2%).

Open (structured) data in JPEG (195), PNG (182), GIF (170) - really?!





# Data Formats @ Open Gov Data United States of America (U.S.A.)

![](i/opendata-us-gov.png)

(Source: [`data.gov`](https://catalog.data.gov/dataset))


- How many datasets? 303_079 datasets
- How many in CSV? 18_862 =>  ~6%

Trivia Quiz: [What's MrSID?](https://en.wikipedia.org/wiki/MrSID)
[What's XYZ?](https://en.wikipedia.org/wiki/XYZ_file_format)

- MrSID = acronym for MultiResolution Seamless Image Database (MrSID) format
- XYZ = chemical format for molecule geometry incl. the number of atoms with cartesian and atomic coordinates





# Data Formats @ DataHub v1

![](i/opendata-datahub.png)

(Source: [`old.datahub.io`](https://old.datahub.io/dataset))


- How many datasets? 11_344 datasets
- How many in CSV (+ text/csv + csv)? 1_231 + 400 + 317 (1_948) => ~17%

Trivia Quiz: [What's meta/void?](https://en.wikipedia.org/wiki/VoID)

- VoID = acronym for Vocabulary of Interlinked Datasets (VoID) -
 an RDF schema vocabulary for metadata






# Data Formats @ DataHub v2  - (Tabular) Data Packages

![](i/opendata-datahub-v2.png)

(Source: [`datahub.io`](https://datahub.io/core/gold-prices))





# And the Winner is ... Lies, Damned Lies and Statistics

>  Statistics can be used to support anything - especially statisticians¹. 
>
> -- Franklin P. Jones

¹: Or (big) data gold mining scientists, of course :-).





# What's Comma-Separated Values (CSV) - One Format? Many Formats?

Text format - all (string) values separated by comma.

```
a,b,c
1,2,3
```

in ruby:

``` ruby
[["a","b","c"],
 ["1","2","3"]]
```



# CSV Basics - What about commas in values?

Wrap the value in double quotes (`"`):

```
a,b,c,"d,e"
1,2,3,"4,5"
```

in ruby:

``` ruby
[["a","b","c","d,e"],
 ["1","2","3","4,5"]]
```



# CSV Basics - What about quotes in quotes?

Variant 1: Double the double quote (`""`):

```
"Hamlet says, ""Seems,"" madam! Nay it is; I know not ""seems."""
```


Variant 2: Use backslash escape (`\"`):

```
"Hamlet says, \"Seems,\" madam! Nay it is; I know not \"seems.\""
```




# CSV Basics - Many Formats / Dialects / Variants?

That's it :-).
Where's the controversy?
What's the heated discussion all about?


# CSV Basics - Edge Cases

What about leading and/or trailing whitespaces?

```
1 , 2 , 3
"a", "b", "c"
```

What about blank lines?

```
1,2,3

4,5,6
```

What "stray" quotes?

```
Paris, 48°51'24"N,2°21'03"E
```


What about blank vs empty string?

```
,,"",""
```

What about nulls, not a number (nan), infinity, newlines,
and more?

What about treating un-quoted and quoted values different?

What about treating un-quoted and quoted leading and trailing spaces different?


# CSV Basics - Type Inference and Data Converters

How to convert strings to:

- numbers
  - integers
  - floats
- booleans (true, false, no, yes, on, off, etc.)
- nulls (null, nil, etc.)
- dates
- arrays (?)
- structs (?)
- etc.


![](i/xkcd-date.png)



# CSV Formats / Variants

CSV Formats supported by the (new)
csvreader library:

- CSV "The Right Way"  
- CSV "Strict"
- CSV <3 Numeric
- CSV <3 JSON
- CSV <3 YAML

Database Export:
  - PostgreSQL CSV
  - PostgreSQL Text
  - MySQL

Or configure (or build) your own :-).

![](i/xkcd-standards.png)



