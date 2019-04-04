# The State of Bool


## Agenda

- Trivia Quiz - The States of Bool
- Bool Basics
  - Classes, Classes, Classes
  - Truthy and Falsy - (Almost) Everything is True
  - The state of built-in bool conversions -  `to_s`, `to_i`, `to_b`, `to_bool`
- safebool Library / Gem
  - Prior Art
    - Some Older Bool / Boolean Libraries
    - `ActiveModel::Type::Boolean < Value`
  - Bool Conversion
    - Empty String
    - Number Two
    - Choose Your Error Handling on Invalid Bool Values / States
- Bool Share Nothing - True or False?
  - Bool in the Real World? What for?
- Bool in Other Programming Languages
  - Classic Programming Languages - From Integer to...
  - Modern (Functional) Programming Languages - ... Algebraic Data (Sum) Types



## What's a Bool?

In computer science, the Boolean data type is a data type that has one of two possible values (usually denoted _true_ and _false_),
intended to represent the two truth values of logic and Boolean algebra. 
It is named after George Boole, who first defined an algebraic system of logic in the mid 19th century.

(Source: [Boolean data type @ Wikipedia](https://en.wikipedia.org/wiki/Boolean_data_type))


## Trivia Quiz - The States of Bool

Q: How many states has a boolean type in a programming language?

- [ A ] 1 - One State
- [ B ] 2 - Two States
- [ C ] 3 - Three States
- [ D ] Other. Please tell


## Trivia Quiz - The States of Bool

A: In practice three really :-), that is, `true`, `false` and
undefined (e.g. `nil`).



## Bool Basics  -  Classes, Classes, Classes

``` ruby
false.class           #=> FalseClass
true.class            #=> TrueClass
false.is_a?(Bool)     #=> NameError: uninitialized constant Bool
true.is_a?(Bool)      #=> NameError: uninitialized constant Bool
false.class.ancestors #=> [FalseClass, Object, Kernel, BasicObject]
true.class.ancestors  #=> [TrueClass, Object, Kernel, BasicObject]
```

Did you known? Ruby has no builtin common bool class / type.



## Bool Basics - Classes, Classes, Classes

``` ruby
module Bool; end

class FalseClass
  include Bool
end

class TrueClass
  include Bool
end
```

Now try:

``` ruby
false.is_a?(Bool)     #=> true
true.is_a?(Bool)      #=> true
false.class.ancestors #=> [FalseClass, Bool, Object, Kernel, BasicObject]
true.class.ancestors  #=> [TrueClass, Bool, Object, Kernel, BasicObject]
```

(Source: [`s6/safebool`](https://github.com/s6ruby/safebool))




## Bool Basics  -  Truthy and Falsy - (Almost) Everything is True

**Everything is `true` except `false` and `nil`.**

``` ruby
!! false   #=> false
!! nil     #=> false

!! true    #=> true
!! "false" #=> true
!! ""      #=> true
!! 0       #=> true
!! 1       #=> true
!! []      #=> true
!! {}      #=> true
!! 0.0     #=> true
!! :false  #=> true
# ...
```

PS: What's bang bang (`!!`)?  The bang `!` character
is the (logical) boolean not operator that converts the expression into a bool AND toggles (inverts) the boolean value (that is, `false` to `true` and `true` to `false`). Thus, if you double up `!!`, that's a "quick hack" for turning anything into a bool (same as adding `def to_b() self ? true : false; end` to `Object / Kernel`, for example).



## Bool Basics  -  to_s, to_i, to_b, to_bool

The state of built-in bool conversions:

``` ruby
false.to_s            #=> "false"
true.to_s             #=> "true"
false.to_i            #=> NoMethodError: undefined method `to_i' for false:FalseClass
true.to_i             #=> NoMethodError: undefined method `to_i' for true:TrueClass
Integer(false)        #=> TypeError: can't convert false into Integer
Integer(true)         #=> TypeError: can't convert true into Integer

# -or-

"false".to_b          #=> NoMethodError: undefined method `to_b' for String
0.to_b                #=> NoMethodError: undefined method `to_b' for Integer
Bool("false")         #=> NoMethodError: undefined method `Bool' for Kernel
Bool(0)               #=> NoMethodError: undefined method `Bool' for Kernel

"true".to_b           #=> NoMethodError: undefined method `to_b' for String
1.to_b                #=> NoMethodError: undefined method `to_b' for Integer
Bool("true")          #=> NoMethodError: undefined method `Bool' for Kernel
Bool(1)               #=> NoMethodError: undefined method `Bool' for Kernel
```

Let's add `to_i`, `to_b`, `parse_bool / to_bool`, `Bool()`.
Why? Why not? Discuss.



## safebool Library / Gem  Adds `Bool()`, `to_b`, `to_bool`, and More

``` ruby
false.to_i            #=> 0
true.to_i             #=> 1

# -or-

"false".to_b          #=> false
0.to_b                #=> false
Bool("false")         #=> false
Bool(0)               #=> false

"true".to_b           #=> true
1.to_b                #=> true
Bool("true")          #=> true
Bool(1)               #=> true
```

(Source: [`s6/safebool`](https://github.com/s6ruby/safebool))




## Prior Art - Some Older Bool / Boolean Libraries

- gem [`to_bool`](https://rubygems.org/gems/to_bool), source <https://github.com/bricker/to_bool>
- gem [`wannabe_bool`](https://rubygems.org/gems/wannabe_bool), source <https://github.com/prodis/wannabe_bool>
- gem [`boolean`](https://rubygems.org/gems/boolean), source <https://github.com/RISCfuture/boolean> - archived (read-only)
- gem [`boolean_class`](https://rubygems.org/gems/boolean_class), source <https://github.com/elgalu/boolean_class>
- gem [`to_boolean`](https://rubygems.org/gems/to_boolean), source <https://github.com/JaniJegoroff/to_boolean>
- gem [`extends_bool`](https://rubygems.org/gems/extends_bool), source <https://github.com/andreleoni/extends_bool>
- gem [`be_boolean`](https://rubygems.org/gems/be_boolean), source <https://github.com/sporto/be_boolean> - archived (read-only)
- gem [`to_b`](https://rubygems.org/gems/to_b), source <https://github.com/benSlaughter/utilise>
- gem [`boolean_conversions`](https://rubygems.org/gems/boolean_conversions), source <https://github.com/thebadmonkeydev/boolean_conversions>
- gem [`trybool`](https://rubygems.org/gems/trybool), source <https://github.com/hashrocket/trybool>
- and many more.

Try a search onb rubygems with [`bool`](https://rubygems.org/search?&query=bool) /
[`boolean`](https://rubygems.org/search?&query=boolean) :-).




## Prior Art - ActiveModel::Type::Boolean < Value

A class that behaves like a boolean type, including rules for conversion of user input.

Conversion

Values set from user input will first be converted into the appropriate ruby type. Conversion behavior is roughly mapped to Ruby's boolean semantics:

- `"false"`, `"f"`, `"0"`, `0` or any other value in `FALSE_VALUES`¹ will be converted to `false`.
- Empty strings are converted to `nil`.
- All other values will be converted to `true`.

¹: `FALSE_VALUES	=	[false, 0, "0", "f", "F", "false", "FALSE", "off", "OFF"]`

(Source: [`api.rubyonrails.org/classes/ActiveModel/Type/Boolean.html`](https://api.rubyonrails.org/classes/ActiveModel/Type/Boolean.html))




## Bool Conversion - Empty String

``` ruby
!! ""       #=> true
ActiveModel::Type::Boolean.new.cast( "" )  #=> nil
"".to_b     #=> false
"".to_bool  #=> nil
Bool("")    #=> ArgumentError: invalid value "":String for Bool(); parse_bool failed (returns nil)
```


## Bool Conversion - Number Two

``` ruby
!! 2        #=> true
ActiveModel::Type::Boolean.new.cast( 2 )  #=> true
2.to_b      #=> true
2.to_bool   #=> nil
Bool(2)     #=> ArgumentError: invalid value 2:Integer for Bool(); parse_bool failed (returns nil)
```


## Bool Conversion - Choose Your Error Handling on Invalid Bool Values / States

1. `to_b` always returns a bool even if the conversion / parsing fails e.g. `true` (for numbers) and `false` (for strings) on error
2. `parse_bool / to_bool` always returns `nil` if the conversion / parsing fails
3. `Bool()` always raises an `ArgumentError` if the conversion / parsing fails
   and a `TypeError` if the conversion is unsupported (e.g. expected required `parse_bool` method missing / undefined)


``` ruby
"2".to_b                #=> false
"2".to_bool             #=> nil
"2".to_bool.bool?       #=> false
"2".to_bool.is_a?(Bool) #=> false
Bool("2")               #=> ArgumentError: invalid value "2":String for Bool(); parse_bool failed (returns nil)

2.to_b                  #=> true
2.to_bool               #=> nil
2.to_bool.bool?         #=> false
2.to_bool.is_a?(Bool)   #=> false
Bool(2)                 #=> ArgumentError: invalid value 2:Integer for Bool(); parse_bool failed (returns nil)
...
```

(Source: [`s6/safebool`](https://github.com/s6ruby/safebool))



## Bool Share Nothing - True or False?

Why no shared common `Bool` class for `FalseClass` and `TrueClass`?

Pro argument / answer: There's NOTHING `true` and `false` share in the code. Keep it simple, stupid.


## Bool Share Nothing - True or False?

Contra argument / answer. How about?

``` ruby
module Bool

  TRUE_VALUES =  ['true', 'yes', 'on', 't', 'y', '1']
  FALSE_VALUES = ['false', 'no', 'off', 'f', 'n', '0']

  def self.parse( o )
    if o.is_a? String
      str = o
    else  ## try "coerce" to string
      str = o.to_str
    end

    case str.downcase.strip
    when *TRUE_VALUES
      true
    when *FALSE_VALUES
      false
    else
      nil   ## note: returns nil if cannot convert to true or false
    end
  end


  def self.convert( o )   ## used by "global" Bool( o ) kernel conversion method
    if o.respond_to?( :parse_bool )
      value = o.parse_bool()  # note: returns true/false OR nil
      if value.nil?
        raise ArgumentError.new( "invalid value >#{o.inspect}< of type >#{o.class.name}< for Bool(); method parse_bool failed (returns nil)")
      end
      value
    else
      raise TypeError.new( "can't convert >#{o.inspect}< of type >#{o.class.name}< to Bool; method parse_bool expected / missing")
    end
  end

  def self.zero() false; end
end # module Bool
```

(Source: [`s6/safebool/lib/safebool.rb`](https://github.com/s6ruby/safebool/blob/master/lib/safebool.rb))



## Bool in the Real World? What for?

Use `Bool` for type annotation e.g.:

``` ruby
sig Bool => Bool
# instead of something like
sig Union[TrueClass,FalseClass] => Union[TrueClass,FalseClass]
```

Use `Bool.zero` for fixing the billion dollar nil mistake with zero, that is,
always initialize your values with (default) zero values.

``` ruby
true.class.zero   #=> false
false.class.zero  #=> false
```

and some more.




## Bool in Classic Programming Languages

**C**

1972:  Use the integer number `0` and `1`. No boolean type.

C99:   Adds boolean type (`bool`) with `false` and `true`
but keeps "tradition" with boolean values are just integer numbers.


**Python**

2.x series:  Use the integer number `0` and `1`. No boolean type.

2.3+ series:  Adds boolean type (`Bool`) - as a derived integer type -
with `False` and `True`.  

Try:

```
1 + True  #=> 4
```


## Bool in Modern (Functional) Programming Languages

Bool is just an enum with two variants, that is, `False` and `True`.

And an enum is "just" an algebraic union "sum" data type with variants e.g.

```
data Bool = False | True
```

Try:

```
1 + True   #=> TypeError
```

And in Ruby:

``` ruby
1 + true   #=> TypeError: true can't be coerced into Integer
```


## Thanks - Questions? Comments?
