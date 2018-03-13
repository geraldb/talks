title: Blockchain vs (Hyper) Ledger -- Inside (Hyper) Ledger Lite - Add Transactions One Block at a Time and Balance the Accounts (Books)



# Blockchain! Blockchain! Blockchain! Decentralize Payments. Decentralize Transactions. Decentralize Blockchains.

What's Blockchain?

- Distributed Database?
- Hyper Ledger Book?
- Consensus with Proof-of-Work or Proof-of-Stake?
- Digital Fingerprints? Cryptographic Hashes?
- Lottery? Central Bank?
- Byzantine-Generals Solution?

Yes. Yes. Yes. Blockchain! Blockchain! Blockchain!

(Source: [openblockchains/whatsblockchain](https://github.com/openblockchains/whatsblockchain))



# Inside Blockchains - Inside (Hyper) Ledgers

Q: What's the best way to understand and learn about blockchains
and (hyper) ledgers?

A: Do it yourself (DIY)! Build your own blockchains and (hyper) ledgers
from scratch (zero)!


# Let's Build Your Own (Hyper) Ledger (From Zero / Scratch)

Crypto God? Python Cowboy? Ruby Ninja? JavaScript Rockstar?

Yes, you can.

![](i/fake-dilbert-blockchain.png)



# Awesome Blockchains

What's Awesome Blockchains?

A public page that collects articles on building your own
blockchains and (hyper) ledgers from scratch (zero) -
github: [openblockchains/awesome-blockchains](https://github.com/openblockchains/awesome-blockchains).




# What's (Hyper) Ledger Lite?

ledger-lite library (github: [openblockchains/ledger.lite.rb](https://github.com/openblockchains/ledger.lite.rb)) - (free, open source) hyper ledger book for the distributed blockchain internet era;
add your transactions one block at a time; transfer crypto currencies or collectibles
and balance the accounts.


# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Q: What's the Difference?



# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Blockchain - a list (chain) of blocks of transactions.

Blockchain Lite Example:

``` ruby
b0 = Block.first(
        { from: "Dutchgrown", to: "Vincent", what: "Tulip Bloemendaal Sunset", qty: 10 },
        { from: "Keukenhof",  to: "Anne",    what: "Tulip Semper Augustus",    qty: 7  } )

b1 = Block.next( b0,
        { from: "Flowers", to: "Ruben", what: "Tulip Admiral van Eijck",  qty: 5 },
        { from: "Vicent",  to: "Anne",  what: "Tulip Bloemendaal Sunset", qty: 3 },
        { from: "Anne",    to: "Julia", what: "Tulip Semper Augustus",    qty: 1 },
        { from: "Julia",   to: "Luuk",  what: "Tulip Semper Augustus",    qty: 1 } )

b2 = Block.next( b1,
        { from: "Bloom & Blossom", to: "Daisy",   what: "Tulip Admiral of Admirals", qty: 8 },
        { from: "Vincent",         to: "Max",     what: "Tulip Bloemendaal Sunset",  qty: 2 },
        { from: "Anne",            to: "Martijn", what: "Tulip Semper Augustus",     qty: 2 },
        { from: "Ruben",           to: "Julia",   what: "Tulip Admiral van Eijck",   qty: 2 } )
...
```


# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Ledger Book = Accounting Book = Book of Accounts

What's an Account?

Q: An account records .... ?

A: Transactions, transaction, transactions.

![](i/ledger.jpg)



# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Ledger Lite Example:

``` ruby
ledger = Ledger.new

ledger.write( Tx.new( from: "Keukenhof†",  to: "Vincent", qty: 11, name: "Tulip Admiral van Eijck" ))
ledger.write( Tx.new( from: "Vincent",     to: "Anne",    qty:  3, name: "Tulip Admiral van Eijck" ))
ledger.write( Tx.new( from: "Anne",        to: "Julia",   qty:  2, name: "Tulip Admiral van Eijck" ))
ledger.write( Tx.new( from: "Julia",       to: "Luuk",    qty:  1, name: "Tulip Admiral van Eijck" ))

ledger.write(
   Tx.new( from: "Dutchgrown†", to: "Ruben",   qty: 11, name: "Tulip Semper Augustus" ),
   Tx.new( from: "Vincent",     to: "Max",     qty:  3, name: "Tulip Admiral van Eijck" ),
   Tx.new( from: "Ruben",       to: "Julia",   qty:  2, name: "Tulip Semper Augustus" ),
   Tx.new( from: "Anne",        to: "Martijn", qty:  1, name: "Tulip Admiral van Eijck" ))
...
```


# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Ledger Lite Example:

``` ruby
ledger = Ledger.new

ledger.write(
   Block.new( Tx.new( from: "Keukenhof†",  to: "Vincent", qty: 11, name: "Tulip Admiral van Eijck" ),
              Tx.new( from: "Vincent",     to: "Anne",    qty:  3, name: "Tulip Admiral van Eijck" ),
              Tx.new( from: "Anne",        to: "Julia",   qty:  2, name: "Tulip Admiral van Eijck" ),
              Tx.new( from: "Julia",       to: "Luuk",    qty:  1, name: "Tulip Admiral van Eijck" )),

   Block.new( Tx.new( from: "Dutchgrown†", to: "Ruben",   qty: 11, name: "Tulip Semper Augustus" ),
              Tx.new( from: "Vincent",     to: "Max",     qty:  3, name: "Tulip Admiral van Eijck" ),
              Tx.new( from: "Ruben",       to: "Julia",   qty:  2, name: "Tulip Semper Augustus" ),
              Tx.new( from: "Anne",        to: "Martijn", qty:  1, name: "Tulip Admiral van Eijck" )))
...
```


# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Q: What's the Difference?


# Blockchain vs (Hyper) Ledger (Book) - What's the Difference?

Blockchain:

- bundles transactions into blocks
- transactions get added to the chain on block at a time

=> Is the transaction valid and the "consensus" / accepted "time-stamped" truth to be trusted?

Ledger:

- add one transaction at a time
- balances your accounts!
- double entry bookkeeping (or triple entry bookkeeping!)

=> Who owns what?


# Double Entry Bookkeeping (Accounting)

> Double-entry bookkeeping, in accounting,
> is a system of bookkeeping so named because every entry to an account
> requires a corresponding and opposite entry to a different account.
> The double entry has two equal and corresponding sides known as debit and credit.
> The left-hand side is debit and right-hand side is credit.
>
> -- [Wikipedia](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system)


``` ruby
## apply/do single transaction - send payment - do transfer
def send( from, to, amount )

  if sufficient?( from, amount )
     @addr[ from ] -= amount
     @addr[ to   ] += amount
  end

end  # method send

## note: transfer is an alias for send (payment)
alias :transfer :send
```


# Ledger vs Hyper Ledger - What's the difference?

Hyper - Ledger (book) for the distributed blockchain internet era

Coinbase! Create money out of ... nothing one block at a time! (†)

``` ruby
Ledger.configure do |config|
  config.coinbase = ['Keukenhof†']
end

# ...

def send( from, to, amount )
  if sufficient?( from, amount )
    if Ledger.config.coinbase?( from )
      # note: coinbase has unlimited funds!! ("virtual" built-in money printing address)
    else
      @addr[ from ] -= amount
    end
    @addr[ to ] += amount
  end
end  # method send
```

(†): Yes, I know. Creating money out of nothing is not new :-). One block at a time every ten minutes with a lottery might.



# Inside Ledger Lite - Currencies, Commodities, Collectibles, Assets and More

Ledger Lite lets you design / create your own transactions.

For example, let's use
`from`, `to`, `qty` (quantity) and `name` (of commodity, collectible or asset).
Override the `Ledger#unpack` method for "unpacking" arguments from transactions
and the `Ledger#send` method for "committing" transactions:

``` ruby
def unpack( tx )
  ## "unpack" from, to, qty, name values
  if tx.is_a?( Hash )   ## support hashes
    from   = tx[:from]
    to     = tx[:to]
    qty    = tx[:qty]
    name   = tx[:name]
  else   ## assume it's a transaction (tx) struct/class
    from   = tx.from
    to     = tx.to
    qty    = tx.qty
    name   = tx.name
  end
  [from,to,qty,name]
end
```


# Inside Ledger Lite - Currencies, Commodities, Collectibles, Assets and More (Cont.)

``` ruby
def send( from, to, qty, name )
  if sufficient?( from, qty, name )
    if Ledger.config.coinbase?( from )
      # note: coinbase has unlimited supply!! magic happens here
    else
      @addr[ from ][ name ] -= qty
    end
    @addr[ to ][ name ] += qty
  end
end
```

# Inside Ledger Lite - Currencies, Commodities, Collectibles, Assets and More (Cont.)

Now use the ledger with the new transaction format like:

``` ruby
ledger = Ledger.new

ledger.send( "Keukenhof†",  "Vincent", 11, "Tulip Admiral van Eijck" )
ledger.send( "Vincent",     "Anne",     3, "Tulip Admiral van Eijck" )
ledger.send( "Anne",        "Julia",    2, "Tulip Admiral van Eijck" )
ledger.send( "Julia",       "Luuk",     1, "Tulip Admiral van Eijck" )

ledger.send( "Dutchgrown†", "Ruben",   11, "Tulip Semper Augustus"  )
ledger.send( "Vincent",     "Max",      3, "Tulip Admiral van Eijck" )
ledger.send( "Ruben",       "Julia",    2, "Tulip Semper Augustus" )
ledger.send( "Anne",        "Martijn",  1, "Tulip Admiral van Eijck"
```



# Do-it-yourself (DIY) - Build Your Own Real-World Case Studies

Open source, free case studies using blockchain-lite, merkletree, p2p,
and ledger-lite include:

- Central Bank - github: [openblockchains/centralbank](https://github.com/openblockchains/centralbank)
- Tulipmania - github: [openblockchains/tulipmania](https://github.com/openblockchains/tulipmania)

And last but not least:

- Shilling (or Schilling) also known as Bitshilling - github: [bitshilling](https://github.com/bitshilling)




# Do-it-yourself (DIY) - Build Your Own Real-World Case Studies - Central Bank

Central Bank - centralbank command line tool (and core library) -
print your own money / cryptocurrency;
run your own federated central bank nodes on the blockchain peer-to-peer over HTTP;
revolutionize the world one block at a time

Command Line. Use the `centralbank` command line tool. Try:

```
$ centralbank -h     
```

resulting in:

```
Usage: centralbank [options]

  Wallet options:
    -n, --name=NAME                  Address name (default: Alice)

  Server (node) options:
    -o, --host HOST                  listen on HOST (default: 0.0.0.0)
    -p, --port PORT                  use PORT (default: 4567)
    -h, --help                       Prints this help
```


# Do-it-yourself (DIY) - Build Your Own Real-World Case Studies - Central Bank (Cont.)

To start a new (network) node using the default wallet
address (that is, Alice) and the default server host and port settings
use:

```
$ centralbank
```

Stand back ten feets :-) while starting up the machinery.
Ready to print (mine) money on the blockchain?
In your browser open up the page e.g. `http://localhost:4567`. Voila!

![](i/centralbank.png)



# Do-it-yourself (DIY) - Build Your Own Real-World Case Studies - Shilling (or Schilling)

Shilling (or Schilling) on the Blockchain! -
Rock-Solid Alpine Dollar from Austria -
BTS is the new ATS - Bitcoin Sidechain

Find out more @ github: [bitshilling](https://github.com/bitshilling).
