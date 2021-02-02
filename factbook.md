title:  factbook.json - Turn the World Factbook into Open (Structured) Data


# Agenda

- What's the World Factbook?
- Country Profile Example - Austria (au) :: Europe
- Country Trivia (Taxonomy)
- Countries - Politics Trivia
- Divide n Conquer - Continents n Regions Trivia (Taxonomy)
- Europe Trivia (Taxonomy)
- Facts - Did You Know?
- History - Milestones 
- History - Let's welcome factbook.json!
- factbook.json - Example
- Country Profiles - What's Included?
- Inside factbook.json - How it works 
  - Step 1: Beautiful HTML 
  - Step 2: Convert HTML to JSON
- factbook.json - Philosophy
- Case Study - How to use?
  - World Alamanc 
  - factbook.sql 


#  What's the World Factbook?

The World Factbook [1][2] published by the Central Intelligence Agency (CIA)
offers free country profiles in the public domain
(that is, no copyright(s), no rights reserved).

- [1] [The World Factbook](https://www.cia.gov/the-world-factbook/)
- [2] [Wikipedia Article: The World Factbook](http://en.wikipedia.org/wiki/The_World_Factbook)


# World Factbook - Country Profile Example - Austria (au) :: Europe

> Note: All country profiles use the "official" two-letter GEC (formerly FIPS) codes and NOT the ISO codes (you
> might be used to) e.g. `au` for Austria (and not `at`), `gm` for Germany (and not `de`), and so on.

![](i/factbook-au.png)

(Source: [www.cia.gov/the-world-factbook/countries/austria](https://www.cia.gov/the-world-factbook/countries/austria/))


# World Factbook - Country Trivia (Taxonomy)

Q: How many "countries" in the world?

A: 261

- 195 sovereign countries
- 52 territories n dependencies
- 6 miscellaneous
- 2 others
- 5 oceans
- 1 world


# World Factbook - Countries - Politics Trivia

Palestine NOT included; includes two entries: Gaza Strip (gz), West Bank (we) -
NOT recognized as a sovereign country by the US.

Tawain (tw) categorized in others - US acknowledges Beijing's One-China policy e.g. Tawain is part of China (and NOT a sovereign country).

Burma (bm) not renamed to Myanmar - new name NOT recognized by the US.

European Union (ee) included in others.

And much more.


# World Factbook - Divide n Conquer - Continents n Regions Trivia (Taxonomy)

Q: How many "continents/regions" in the world?

A: 11

- North America (7)
- Central America & Caribbean (33)
- South America (14)
- Europe (55)
- Africa (56)
- Middle East (19)
- South Asia (9)
- Central Asia (6)
- East & Southeast Asia (22)
- Australia/Oceania (30)
- Antartica (4)


![](i/factbook-regions.png)



# World Factbook - Europe Trivia (Taxonomy)

Q: How many "countries" in Europe?

A: 55

- Greenland - Part of Europe? (no, in North America)
- Cyprus - Part of Europe? (yes)
- Turkey - Part of Europe? (no, in Middle East)
- Russia - Part of Europe? (no, in Central Asia)
- Uzbekistan - Part of Europe? (no, in Central Asia)



# World Factbook - Facts - Did You Know?

The World Factbook is one of the US Government's most accessed publications.

> The World Factbook, produced for US policymakers and coordinated throughout the US Intelligence Community,
> presents the basic realities about the world in which we live. We share these facts with the people of
> all nations in the belief that knowledge of the truth underpins the functioning of free societies.

Who uses The World Factbook?

> A wide variety of folks including US Government officials, researchers, news organizations, corporations,
> geographers, teachers, professors, librarians, and students. In short, anyone looking for an expansive
> body of international data on a recently updated Web site.


The World Factbook is a unique reference in that it is updated continuously - on average, every week.

> Information in The Factbook is collected from - and coordinated with - a wide variety of US Government
> agencies, as well as from hundreds of published sources.

(Source: [the-world-factbook/about/did-you-know](https://www.cia.gov/the-world-factbook/about/did-you-know/))



# World Factbook - History - Milestones I/II

The United States has carried on foreign intelligence activities since the days of George Washington but only since World War II have they been coordinated on a government-wide basis. Three programs have highlighted the development of coordinated basic intelligence since that time: (1) the Joint Army Navy Intelligence Studies (JANIS), (2) the National Intelligence Survey (NIS), and (3) The World Factbook.

During World War II, intelligence consumers realized that the production of basic intelligence by different components of the US Government resulted in a great duplication of effort and conflicting information. The Japanese attack on Pearl Harbor in 1941 brought home to leaders in Congress and the executive branch the need for integrating departmental reports to national policymakers. Detailed and coordinated information was needed not only on such major powers as Germany and Japan, but also on places of little previous interest. In the Pacific Theater, for example, the Navy and Marines had to launch amphibious operations against many islands about which information was unconfirmed or nonexistent. Intelligence authorities resolved that the United States should never again be caught unprepared.

In 1943, Gen. George B. Strong (G-2), Adm. H. C. Train (Office of Naval Intelligence - ONI), and Gen. William J. Donovan (Director of the Office of Strategic Services - OSS) decided that a joint effort should be initiated. A steering committee was appointed on 27 April 1943 that recommended the formation of a Joint Intelligence Study Publishing Board to assemble, edit, coordinate, and publish the Joint Army Navy Intelligence Studies (JANIS). JANIS was the first interdepartmental basic intelligence program to fulfill the needs of the US Government for an authoritative and coordinated appraisal of strategic basic intelligence. Between April 1943 and July 1947, the board published 34 JANIS studies. JANIS performed well in the war effort, and numerous letters of commendation were received, including a statement from Adm. Forrest Sherman, Chief of Staff, Pacific Ocean Areas, which said, "JANIS has become the indispensable reference work for the shore-based planners."

The need for more comprehensive basic intelligence in the postwar world was well expressed in 1946 by George S. Pettee, a noted author on national security. He wrote in The Future of American Secret Intelligence (Infantry Journal Press, 1946, page 46) that world leadership in peace requires even more elaborate intelligence than in war. "The conduct of peace involves all countries, all human activities - not just the enemy and his war production."

(Source: [the-world-factbook/about/history](https://www.cia.gov/the-world-factbook/about/history/))


# World Factbook - History - Milestones II/II

**1962**
The first classified Factbook was published in August 1962.

**1971**
The first unclassified version was published in June 1971.

**1981**
Publication becomes an annual product and is renamed The World Factbook. A total of 165 nations are
covered on 225 pages.

**1992**
Twenty new successor state entries replace those of the Soviet Union and Yugoslavia. New countries are
respectively: Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Kyrgyzstan, Latvia, Lithuania,
Moldova, Russia, Tajikistan, Turkmenistan, Ukraine, Uzbekistan; and Bosnia and Hercegovina, Croatia,
Macedonia, Serbia and Montenegro, Slovenia. Number of nations in the Factbook rises to 188.

**1994**
The World Factbook is first produced on CD-ROM.

**1997**
The World Factbook introduced onto the Internet. A special printed edition prepared for the CIA's 50th anniversary.

**1998**
Last year for the production of CD-ROM versions of the Factbook.

**2012**
Size of the printed Factbook's 50th anniversary edition reaches 847 pages.

(Source: [the-world-factbook/about/history](https://www.cia.gov/the-world-factbook/about/history/))




# World Factbook - History - Let's welcome factbook.json!

**2015**

A nobody in Austria (au) introduces the World Factbook
to the free world and open structured data. Let's welcome factbook.json
(and the factbook page parser)!

See [`/factbook/factbook`](https://github.com/factbook/factbook)
and [`/factbook/factbook.json`](https://github.com/factbook/factbook.json) on GitHub.



# World Factbook - factbook.json - Example

Example - Austria - au.json:

```
{
  "Introduction": {
    "Background": {
      "text": "Once the center of power for the large Austro-Hungarian Empire,
               Austria was reduced to a small republic after its defeat in World War..."
    }
  },
  "Geography": {
    "Location": {
      "text": "Central Europe, north of Italy and Slovenia"
    },
    "Geographic coordinates": {
      "text": "47 20 N, 13 20 E"
    },
    "Map references": {
      "text": "Europe"
    },
    "Area": {
      "total": {
        "text": "83,871 sq km"
      },
      "land": {
        "text": "82,445 sq km"
      },
      "water": {
        "text": "1,426 sq km"
      }
    },
    "Area - comparative": {
      "text": "about the size of South Carolina...",
    },
    "Land boundaries": {
      "total": {
        "text": "2,524 km"
      },
      "border countries": {
        "text": "Czech Republic 402 km, Germany 801 km, Hungary 321 km, Italy 404 km,
                 Liechtenstein 34 km, Slovakia 105 km, Slovenia 299 km, Switzerland 158 km"
      }
    },
    "Climate": {
      "text": "temperate; continental, cloudy; cold winters with frequent rain and
               some snow in lowlands and snow in mountains;
               moderate summers with occasional showers"
    },
    "Elevation extremes": {
      "lowest point": {
        "text": "Neusiedler See 115 m"
      },
      "highest point": {
        "text": "Grossglockner 3,798 m"
      }
    },
    "Natural resources": {
      "text": "oil, coal, lignite, timber, iron ore, copper, zinc, antimony,
               magnesite, tungsten, graphite, salt, hydropower"
    },
    ...
```


# World Factbook - Country Profiles - What's Included?

Example Sections and Subsections for Austria (au):

1. Introduction
   1. Background
2. Geography
   1. Location
   2. Geographic coordinates
   3. Map references
   4. Area
   5. Area - comparative
   6. Land boundaries
   7. Coastline  
   8. Maritime claims
   9. Climate
   10. Terrain
   11. Elevation
   12. Natural resources
   13. Land use
   14. Irrigated land
   15. Population - distribution
   16. Natural hazards
   17. Environment - current issues
   18. Environment - international agreements
   19. Geography - note
3. People and Society
   1. Population
   2. Nationality
   3. Ethnic groups
   4. Languages
   5. Religions
   6. Age structure
   7. Dependency ratios
   8. Median age
   9. Population growth rate
   10. Birth rate
   11. Death rate
   12. Net migration rate
   13. Population distribution
   14. Urbanization
   15. Major urban areas - population
   16. Sex ratio
   17. Mother's mean age at first birth
   18. Maternal mortality rate
   19. Infant mortality rate
   20. Life expectancy at birth
   21. Total fertility rate
   22. Contraceptive prevalence rate
   23. Health expenditures
   24. Physicians density
   25. Hospital bed density
   26. Drinking water source
   27. Sanitation facility access
   28. HIV/AIDS - adult prevalence rate
   29. HIV/AIDS - people living with HIV/AIDS
   30. HIV/AIDS - deaths
   31. Obesity - adult prevalence rate
   32. Education expenditures
   33. School life expectancy (primary to tertiary education)
   34. Unemployment, youth ages 15-24
4. Government
   1. Country name
   2. Government type
   3. Capital
   4. Administrative divisions
   5. Independence
   6. National holiday
   7. Constitution
   8. Legal system
   9. International law organization participation
   10. Citizenship
   11. Suffrage
   12. Executive branch
   13. Legislative branch
   14. Judicial branch
   15. Political parties and leaders
   16. Political pressure groups and leaders
   17. International organization participation
   18. Diplomatic representation in the US
   19. Diplomatic representation from the US
   20. Flag description
   21. National symbol(s)
   22. National anthem
5. Economy
   1. Economy - overview
   2. GDP (purchasing power parity)
   3. GDP (official exchange rate)
   4. GDP - real growth rate
   5. GDP - per capita (PPP)
   6. Gross national saving
   7. GDP - composition, by end use
   8. GDP - composition, by sector of origin
   9. Agriculture - products
   10. Industries
   11. Industrial production growth rate
   12. Labor force
   13. Labor force - by occupation
   14. Unemployment rate
   15. Population below poverty line
   16. Household income or consumption by percentage share
   17. Distribution of family income - Gini index
   18. Budget
   19. Taxes and other revenues
   20. Budget surplus (+) or deficit (-)
   21. Public debt
   22. Fiscal year
   23. Inflation rate (consumer prices)
   24. Commercial bank prime lending rate
   25. Stock of narrow money
   26. Stock of broad money
   27. Stock of domestic credit
   28. Market value of publicly traded shares
   29. Current account balance
   30. Exports
   31. Exports - commodities
   32. Exports - partners
   33. Imports
   34. Imports - commodities
   35. Imports - partners
   36. Reserves of foreign exchange and gold
   37. Debt - external
   38. Stock of direct foreign investment - at home
   39. Stock of direct foreign investment - abroad
   40. Exchange rates
6. Energy
   1. Electricity access
   2. Electricity - production
   3. Electricity - consumption
   4. Electricity - exports
   5. Electricity - imports
   6. Electricity - installed generating capacity
   7. Electricity - from fossil fuels
   8. Electricity - from nuclear fuels
   9. Electricity - from hydroelectric plants
   10. Electricity - from other renewable sources
   11. Crude oil - production
   12. Crude oil - exports
   13. Crude oil - imports
   14. Crude oil - proved reserves
   15. Refined petroleum products - production
   16. Refined petroleum products - consumption
   17. Refined petroleum products - exports
   18. Refined petroleum products - imports
   19. Natural gas - production
   20. Natural gas - consumption
   21. Natural gas - exports
   22. Natural gas - imports
   23. Natural gas - proved reserves
   24. Carbon dioxide emissions from consumption of energy
7. Communications
   1. Telephones - fixed lines
   2. Telephones - mobile cellular
   3. Telephone system
   4. Broadcast media
   5. Internet country code
   6. Internet users
8. Transportation
   1. National air transport system
   2. Civil aircraft registration country code prefix
   3. Airports
   4. Airports - with paved runways
   5. Airports - with unpaved runways
   6. Heliports
   7. Pipelines
   8. Railways
   9. Roadways
   10. Waterways
   11. Merchant marine
   12. Ports and terminals
9. Military and Security
   1. Military branches
   2. Military service age and obligation
   3. Military expenditures
10. Transnational Issues
    1. Disputes - international
    2. Refugees and internally displaced persons
    3. Illicit drugs



# Inside factbook.json - How it works - Step 1: Beautiful HTML I/II

Step 1: Fetch Country Profile Page (in HTML)

Step 2: Remove "Chrome" - all headers, footers, scripts, etc. - keep the "raw" profile data (in HTML)

Step 3: Cleanup the profile data (in HTML) e.g.

All (style) classes get stripped except `category`, `category_data`.
All country comparison links get stripped. All field reference links
and images get stripped. The audio player (for the national anthem) gets stripped.
All gallery photos gets stripped and so on and so forth.



# Inside factbook.json - How it works - Step 1: Beautiful HTML II/II

Resulting in:

```
<h2>Introduction</h2>

<h3>Background:</h3>
<div class=category_data>Once the center of power for the large Austro-Hungarian Empire,
Austria was reduced to a small republic after its defeat ...
A prosperous, democratic country, Austria entered the EU Economic and Monetary Union in 1999.</div>

<h2>Geography</h2>

<h3>Location:</h3>
<div class=category_data>Central Europe, north of Italy and Slovenia</div>

<h3>Geographic coordinates:</h3>
<div class=category_data>47 20 N, 13 20 E</div>

<h3>Map references:</h3>
<div class=category_data>Europe</div>

<h3>Area:</h3>
<div><span class=category>total: </span><span class=category_data>83,871 sq km</span></div>
<div><span class=category>land: </span><span class=category_data>82,445 sq km</span></div>
<div><span class=category>water: </span><span class=category_data>1,426 sq km</span></div>
...
```


# Inside factbook.json - How it works - Step 2:  Convert HTML to JSON

- Heading 2 (`h2`) gets converted to a section.
- Heading 3 (`h3`) gets converted to a subsection.
- Here be dragons. Now the unknown - everythig is possible part ;-)
  - Data entry might be a simple entry or an entry with a note or nested entry
    or nested entry with a note or a sequence of simple entries and so on.


# factbook.json - Philosophy

Don't be "smarter" than the CIA - is it possible? ;-)

- Use the same country code as the CIA.
- Use the same region for countries as the CIA.
- Use the same section names as the CIA.
- Use the same subsection names as the CIA (including "inconsistencies").
- Use the same data as the CIA (including "known" typos /errors).



# Case Study - How to use? - World Alamanc I/II

Build yourself your very own World Alamanc in 20 Lines of Script

```
require 'factbook'

TEMPLATE = <<EOS

### <%= names %>

<%= page.name_long=='none' ? '\-' : page.name_long %> › <%= page.map %> -- <%= page.location %> <br>
<%= page.capital %> • <%= page.area %> • pop. <%= page.population %>

**Languages:** <%= page.languages %>
**Major cities:** <%= page.major_cities %>
**Ethnic groups:** <%= page.ethnic_groups %>
**Religions:** <%= page.religions %>
**Independence:** <%= page.independence %>

**Internet:** `<%= page.internet %>` • <%= page.internet_users %> • <%= page.internet_users_rate %>
**Telephones - mobile:** <%= page.telephones_mobile %> • <%= page.telephones_mobile_subscriptions %> subs./100

EOS


## step 1: read all countries; using local json (dump) files

json_dir = '../../factbook/factbook.json'

codes  = Factbook.codes.countries
pages  = Factbook::JsonPageReader.new( json_dir ).read_pages( codes )

almanac = Factbook::Almanac.new( pages )

## step 2: save to disk

File.open( './ALMANAC.md', 'w:utf-8' ) do |f|
  f.write almanac.render( TEMPLATE )
end

puts "Done."
```

(Source: [factbook/script/almanac.rb](https://github.com/factbook/factbook/blob/master/script/almanac.rb))


# Case Study - How to use? - World Alamanc II/II

### Afghanistan

Islamic Republic of Afghanistan › Asia -- Southern Asia, north and west of Pakistan, east of Iran <br>
Kabul • 652,230 sq km • pop. 32,564,342 (July 2015 est.)

**Languages** Afghan Persian or Dari (official) 50%, Pashto (official) 35%, Turkic languages (primarily Uzbek and Turkmen) 11%, 30 minor languages (primarily Balochi and Pashai) 4%, much bilingualism, but Dari functions as the lingua franca
**Major cities** KABUL (capital) 4.635 million (2015)
**Ethnic groups** Pashtun, Tajik, Hazara, Uzbek, other (includes smaller numbers of Baloch, Turkmen, Nuristani, Pamiri, Arab, Gujar, Brahui, Qizilbash, Aimaq, Pashai, and Kyrghyz)
**Religions** Sunni Muslim 80%, Shia Muslim 19%, other 1%
**Independence** 19 August 1919 (from UK control over Afghan foreign affairs)

**Internet** `.af` • 1.9 million • 5.9% (2014 est.)
**Telephones - mobile** 23.4 million • 74 (2014 est.) subs./100

### Albania • Shqiperia

Republic of Albania › Europe -- Southeastern Europe, bordering the Adriatic Sea and Ionian Sea, between Greece to the south and Montenegro and Kosovo to the north <br>
Tirana (Tirane) • 28,748 sq km • pop. 3,029,278 (July 2015 est.)

**Languages** Albanian 98.8% (official - derived from Tosk dialect), Greek 0.5%, other 0.6% (including Macedonian, Roma, Vlach, Turkish, Italian, and Serbo-Croatian), unspecified 0.1% (2011 est.)
**Major cities** TIRANA (capital) 454,000 (2015)
**Ethnic groups** Albanian 82.6%, Greek 0.9%, other 1% (including Vlach, Roma (Gypsy), Macedonian, Montenegrin, and Egyptian), unspecified 15.5% (2011 est.)
**Religions** Muslim 56.7%, Roman Catholic 10%, Orthodox 6.8%, atheist 2.5%, Bektashi (a Sufi order) 2.1%, other 5.7%, unspecified 16.2%
**Independence** 28 November 1912 (from the Ottoman Empire)

**Internet** `.al` • 1.7 million • 56.5% (2014 est.)
**Telephones - mobile** 3.4 million • 111 (2014 est.) subs./100

### Algeria • Al Jaza'ir

People's Democratic Republic of Algeria › Africa -- Northern Africa, bordering the Mediterranean Sea, between Morocco and Tunisia <br>
Algiers • 2,381,741 sq km • pop. 39,542,166 (July 2015 est.)

**Languages** Arabic (official), French (lingua franca), Berber dialects: Kabylie Berber (Tamazight), Chaouia Berber (Tachawit), Mzab Berber, Tuareg Berber (Tamahaq)
**Major cities** ALGIERS (capital) 2.594 million; Oran 858,000 (2015)
**Ethnic groups** Arab-Berber 99%, European less than 1%
**Religions** Muslim (official; predominantly Sunni) 99%, other (includes Christian and Jewish)


**Independence** 5 July 1962 (from France)

**Internet** `.dz` • 6.5 million • 16.7% (2014 est.)
**Telephones - mobile** 37.3 million • 96 (2014 est.) subs./100

...

(Source: [factbook/factbook.json/ALMANAC.md](https://github.com/factbook/factbook.json/blob/master/ALMANAC.md))


# Case Study - How to use? - factbook.sql - I/II

How about a single-file SQLite database for all factbook data?

Step 1: Create a database schema (tables and columns). Example:

```
CREATE TABLE "facts" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "code" varchar(255) NOT NULL,
  "name" varchar(255) NOT NULL,

  ---------------------------------------
  -- Geography
  "area"       integer,
  "area_land"  integer,
  "area_water" integer,

  ----------------------------------------
  -- People and Society
  "population"        integer,
  "population_growth" float,
  "birth_rate"        float,
  "death_rate"        float,
  "migration_rate"    float
  ...
)
```


Step 2: Import the data

```
require 'factbook'

DB_CONFIG = {
  adapter:  'sqlite3',
  database: './factbook.db'
}

ActiveRecord::Base.logger = Logger.new( STDOUT )
ActiveRecord::Base.establish_connection( DB_CONFIG )

Factbook::CreateDb.new.up    ## create tables

importer = Factbook::Importer.new

Factbook.codes.each do |code|
  puts "Fetching #{code.code}- #{code.name}..."
  page = Factbook::Page.new( code.code )

  puts "Adding #{code.code}- #{code.name}..."
  importer.import( page )
end

puts "Done."
```


# Case Study - How to use? - factbook.sql - II/II

Find the ten largest countries by area:

```
  SELECT name, area FROM facts ORDER BY area DESC LIMIT 10;
```

Resulting in:

```
Russia         | 17_098_242
Canada         |  9_984_670
United States  |  9_826_675
China          |  9_596_960
Brazil         |  8_515_770
Australia      |  7_741_220
European Union |  4_324_782
India          |  3_287_263
Argentina      |  2_780_400
Kazakhstan     |  2_724_900
```

Find the ten largest countries by population:

```
SELECT name, population FROM facts ORDER BY population DESC LIMIT 10;
```

Resulting in:

```
World          | 7_256_490_011
China          | 1_367_485_388
India          | 1_251_695_584
European Union |   513_949_445
United States  |   321_368_864
Indonesia      |   255_993_674
Brazil         |   204_259_812
Pakistan       |   199_085_847
Nigeria        |   181_562_056
Bangladesh     |   168_957_745
```

And so on.
