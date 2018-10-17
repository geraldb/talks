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
the dot `.` is the thousand separator (!) e.g. `13.101`
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



# Data Formats @ Open Gov Data United States of America (U.S.A.)

![](i/opendata-us-gov.png)

(Source: [`data.gov`](https://catalog.data.gov/dataset))



# Data Formats @ DataHub v1

![](i/opendata-datahub.png)

(Source: [`old.datahub.io`](https://old.datahub.io/dataset))



# Data Formats @ DataHub v2  - (Tabular) Data Packages

![](i/opendata-datahub-v2.png)

(Source: [`datahub.io`](https://datahub.io/core/gold-prices))







# And the Winner is ... Lies, Damned Lies and Statistics

>  Statistics can be used to support anything - especially statisticians¹. 
>
> -- Franklin P. Jones

¹: Or (big) data gold mining scientists, of course :-).
