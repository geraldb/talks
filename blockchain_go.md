title: Blockchain! Blockchain! Blockchain! - Build Your Own Blockchains in Go from Zero (Scratch)


# Alchemy - How-To Mine Digital Schilling? How-To Turn Digital Bits Into $$$ or €€€?

**Transactions (Hyper) Ledger Book**

| From                | To               | Amount  |
|---------------------|------------------|--------:|
| Grossklockner (†)   | Sepp             |    3798 |
| Grossvenediger (†)  | Franz            |    3666 |
| Dachstein (†)       | Sissi            |    2995 |
| Wildspitze (†)      | Maria            |    3768 |
|                     |                  |         |
| Sissi               | Eva              |     20  |
| Sepp                | Maria            |     17  |
| Franz               | Sissi            |      3  |
| Sepp                | Ferdl            |    100  |
|                     |                  |         |
| Franz               | Max              |     17  |
| Maria               | Adam             |      4  |

(†): Miner Transaction - New Schilling on the Market!

(Source: [bitshilling/bitshilling](https://github.com/bitshilling/bitshilling))



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


# The Proof of the Pudding is ...  The Bitcoin (BTC) Blockchain(s)

A Success Story on the Blockchain.

May 22, 2010 - World's 1st Bitcoin Payment

A developer bought two pizzas using 10 000 Bitcoin (BTC) - a then-little-known digital (crypto)currency.
Estimated worth about $40.

Triva Q: How much is one Bitcoin worth today? Q: How much are 10 000 Bitcoin worth today?



# $20 Million Dollar Pizza Day - Celebrating the Birth of Bitcoin Alchemy - $$$

!! update chart !!!

![](i/bitcoinmarket.png)

(Source: [coinmarketcap.com/currencies/bitcoin](https://coinmarketcap.com/currencies/bitcoin))



# Let's Build Your Own Blockchain in Go (From Zero / Scratch)

Crypto God? Go Ninja / Rockstar?

Yes, you can.

![](i/fake-dilbert-blockchain.png)




# Code, Code, Code - A Blockchain in Go in 20 Lines! A Blockchain is a Data Structure

What's Blockchain?

It's a list (chain) of blocks linked and secured by digital fingerprints (also known as
crypto hashes).


``` go
package main

import "..."

type Block struct {
  Time int64       // seconds since (unix) epoch (1970-01-01)
  Data string
  Prev string
  Hash string  
}

func calcHash( data string ) string {
  hashed := sha256.Sum256( []byte(data) )
  return binToStr( hashed[:] )   // note: [:] converts [32]byte to []byte
}


func NewBlock(data string, prev string) Block {
  t    := time.Now().Unix()
  hash := calcHash( intToStr(t) + prev + data )

  return Block { t, data, prev, hash }
}
```

(Source: [openblockchains/awesome-blockchains/blockchain.go](https://github.com/openblockchains/awesome-blockchains/blob/master/blockchain.go/blockchain.go))

Yes, that's it.


# Code, Code, Code - A Blockchain in Go in 20 Lines! A Blockchain is a Data Structure (Cont.)


Let's get started -  build a blockchain a block at a time!

``` go
func main() {

  b0 := NewBlock( "Hello, Cryptos!", "0000000000000000000000000000000000000000000000000000000000000000" )
  b1 := NewBlock( "Hello, Cryptos! - Hello, Cryptos!", b0.Hash )

  blockchain := []Block {b0, b1}
}
```


# Code, Code, Code - A Blockchain in Go in 20 Lines! A Blockchain is a Data Structure (Cont.)

> Wait, so a blockchain is just a linked list?
>
> No. A linked list is only required to have a reference to the previous element,
> a block must have an identifier depending on the previous block's identifier,
> meaning that you cannot replace a block without recomputing every single block that comes after.
> In this implementation that happens as the previous digest is input in the calc_hash method.


will log something like:

``` go
fmt.Println( b0 )
// {1522687834 Hello, Cryptos!
//    0000000000000000000000000000000000000000000000000000000000000000
//    d85da0f449ff9ddc2c5ba638b23b9524381811227eb463b8c9e0be40dc1b1a8a}
fmt.Println( len( b0.Hash ))
// => 64
fmt.Println( len( b0.Prev ))
// => 64

fmt.Println( b1 )
// {1522687834 Hello, Cryptos! - Hello, Cryptos!
//     d85da0f449ff9ddc2c5ba638b23b9524381811227eb463b8c9e0be40dc1b1a8a
//     e48ba730165d88e15435483fc3a60714be526096a0c9a71ad10623340e33c7e3}
fmt.Println( len( b1.Hash ))
// => 64
fmt.Println( len( b1.Prev ))
// => 64

fmt.Println( blockchain )
// => [{1522687834 Hello, Cryptos!
//           0000000000000000000000000000000000000000000000000000000000000000
//           d85da0f449ff9ddc2c5ba638b23b9524381811227eb463b8c9e0be40dc1b1a8a}
//     {1522687834 Hello, Cryptos! - Hello, Cryptos!
//           d85da0f449ff9ddc2c5ba638b23b9524381811227eb463b8c9e0be40dc1b1a8a
//           e48ba730165d88e15435483fc3a60714be526096a0c9a71ad10623340e33c7e3}]
```


# What about Proof-of-Work? What about Consensus?

Making (Hash) Mining a Lottery - Find the Lucky Number

``` go
t    := time.Now().Unix()
hash := calcHash( intToStr(t) + prev + data )
```

The computer (node) in the blockchain network that computes the
next block with a valid hash wins the lottery.

For adding a block to the chain you get a reward! You get ~25~ 12.5 Bitcoin! (†)

Bitcoin adds a block every ten minutes.

(†) The reward gets halfed about every two years. In Sep'17 you'll get 12.5 Bitcoin.



# What about Proof-of-Work? What about Consensus? (Cont.)

Random SHA256 hash #1: `c396de4c03ddb5275661982adc75ce5fc5905d2a2457d1266c74436c1f3c50f1`

Random SHA256 hash #2: `493131e09c069645c82795c96e4715cea0f5558be514b5096d853a5b9899154a`

Triva Q: What's SHA256?

- (A) Still Hacking Anyway
- (B) Secure Hash Algorithm
- (C) Sweet Home Austria
- (D) Super High Aperture


# What about Proof-of-Work? What about Consensus? (Cont.)

A: SHA256 == Secure Hash Algorithms 256 Bits

Trivia: Designed by the National Security Agency (NSA) of the United States of America (USA).

Secure == Random  e.g. Change one Letter and the Hash will Change Completely

Making (Hash) Mining a Lottery - Find the Lucky Number

Find a hash that starts with ten leading zeros e.g.

`0000000000069645c82795c96e4715cea0f5558be514b5096d853a5b9899154a`

Hard to compute! Easy to check / validate.



# What about Proof-of-Work? What about Consensus? (Cont.)

Making (Hash) Mining a Lottery - Find the Lucky Number (Nonce)

Nonce == Number used once

``` go
func calcHash( data string ) string {
  hashed := sha256.Sum256( []byte(data) )
  return binToStr( hashed[:] )   // note: [:] converts [32]byte to []byte
}

func computeHashWithProofOfWork( data string ) (int64,string) {
  nonce := int64( 0 )
  for {
    hash := calcHash( intToStr(nonce) + data )
    if strings.HasPrefix( hash, "0000" ) {
        return nonce,hash    // bingo! proof of work if hash starts with leading zeros (0000)
    } else {
        nonce += 1           // keep trying (and trying and trying)
    }
  }
}
```

(Source: [awesome-blockchains/blockchain_with_proof_of_work.go](https://github.com/openblockchains/awesome-blockchains/blob/master/blockchain.go/blockchain_with_proof_of_work.go))



# What about Proof-of-Work? What about Consensus? (Cont.)

Making (Hash) Mining a Lottery - Find the Lucky Number (Nonce)

Nonce == Number used once

``` go
type Block struct {
  Time  int64       // seconds since (unix) epoch (1970-01-01)
  Data  string
  Prev  string
  Hash  string
  Nonce int64       // number used once - lucky (mining) lottery number
}

func NewBlock( data string, prev string ) Block {
  t           := time.Now().Unix()
  nonce, hash := computeHashWithProofOfWork( intToStr(t) + prev + data )

  return Block { t, data, prev, hash, nonce }
}
```



# What about Proof-of-Work? What about Consensus? (Cont.)

Let's rerun the sample with the proof of work machinery added.
Now the sample will print something like:

``` go
b0 := NewBlock( "Hello, Cryptos!", "0000000000000000000000000000000000000000000000000000000000000000" )
b1 := NewBlock( "Hello, Cryptos! - Hello, Cryptos!", b0.Hash )

fmt.Println( b0 )
// {1522691756 Hello, Cryptos!
//    0000000000000000000000000000000000000000000000000000000000000000
//    00009f597a8e28fc42a450c0ed2eff1b6507f76f6a7d1e112686700ce37e3676
//    42278}
fmt.Println( len( b0.Hash ))
// => 64
fmt.Println( len( b0.Prev ))
// => 64

fmt.Println( b1 )
// {1522691756 Hello, Cryptos! - Hello, Cryptos!
//     00009f597a8e28fc42a450c0ed2eff1b6507f76f6a7d1e112686700ce37e3676
//     00009ef5ea432f840c3fb23dbedb5cce4c72e2951a140c1289dda1fedbcd6e99
//     105106}
```

See the difference?
All hashes now start with leading zeros (`0000`)
and the nonces (e.g. `42278` or `105106`)
are the random "lucky numbers" that makes it happen.
That's the magic behind the proof of work.



# What's Your Hash Rate?

!! add code benchmark !!




# Triva Quiz: What's the Bitcoin Hash Rate?

!! add chart !!




# The World's Worst Database - Bitcoin Blockchain Mining

- Uses approximately the same amount of electricity as could power an average American household
  for a day per transaction
- Supports 3 transactions / second across a global network with millions of CPUs/purpose-built ASICs
- Takes over 10 minutes to "commit" a transaction
- Doesn't acknowledge accepted writes: requires you read your writes,
  but at any given time you may be on a blockchain fork, meaning your write might not actually
  make it into the "winning" fork of the blockchain (and no, just making it into the mempool doesn't count).
  In other words: "blockchain technology" cannot by definition tell you if a given write is ever
  accepted/committed except by reading it out of the blockchain itself (and even then)
- Can only be used as a transaction ledger denominated in a single currency,
  or to store/timestamp a maximum of 80 bytes per transaction

(Source: [Tony Arcieri - On the dangers of a blockchain monoculture](https://tonyarcieri.com/on-the-dangers-of-a-blockchain-monoculture))


# Tulip Mania Quiz - Win 100 Schilling on the Blockchain!

Q: Tulip Mania Bubble in Holland - What Year?

- (A) 1561
- (B) 1637
- (C) 1753
- (D) 1817

Q: What's the Name of the Most Expensive Tulip?

- (A) Admiral van Eijck
- (B) Admiral of Admirals
- (C) Semper Augustus
- (C) Semper Cesarus


# Tulips on the Blockchain! Adding Transactions

Learn by Example from the Real World (Anno 1637) - Buy! Sell! Hold! Enjoy the Beauty of Admiral of Admirals, Semper Augustus and More.

**Transactions (Hyper) Ledger Book**

| From                | To           | What                      | Qty |
|---------------------|--------------|---------------------------|----:|
| Dutchgrown (†)      | Vincent      | Tulip Bloemendaal Sunset  |  10 |
| Keukenhof (†)       | Anne         | Tulip Semper Augustus     |   7 |
|                     |              |                           |     |
| Flowers (†)         | Ruben        | Tulip Admiral van Eijck   |   5 |
| Vicent              | Anne         | Tulip Bloemendaal Sunset  |   3 |
| Anne                | Julia        | Tulip Semper Augustus     |   1 |
| Julia               | Luuk         | Tulip Semper Augustus     |   1 |
|                     |              |                           |     |
| Bloom & Blossom (†) | Daisy        | Tulip Admiral of Admirals |   8 |
| Vincent             | Max          | Tulip Bloemendaal Sunset  |   2 |
| Anne                | Martijn      | Tulip Semper Augustus     |   2 |
| Ruben               | Julia        | Tulip Admiral van Eijck   |   2 |
|                     |              |                           |     |
| Teleflora (†)       | Max          | Tulip Red Impression      |  11 |
| Anne                | Naomi        | Tulip Bloemendaal Sunset  |   1 |
| Daisy               | Vincent      | Tulip Admiral of Admirals |   3 |
| Julia               | Mina         | Tulip Admiral van Eijck   |   1 |
|                     |              |                           |     |
| Max                 | Isabel       | Tulip Red Impression      |   2 |

(†): Grower Transaction - New Tulips on the Market!

(Source: [openblockchains/tulips](https://github.com/openblockchains/tulips))



# Tulips on the Blockchain! Adding Transactions (Cont.)

**Quotes - Blockchains are the next Internets / Tulips**

> People who compare digital tokens to tulips are essentially saying digital tokens are a bubble backed
> by nothing but pure hype and speculation.
>
> What they fail to understand is that tulips come from dirt, not a blockchain.
>
> And as we all know, blockchain is possibly the best technological innovation since the internet.
> It will have a tremendous impact on global business and society in general.
> -- [TulipToken](http://tuliptoken.com)



# Tulips on the Blockchain! Adding Transactions (Cont.)

``` go

  to be done

```



# Tulips on the Blockchain! Adding Transactions (Cont.)

resulting in:

``` go
 to be done
```


# What's Blockchain Lite - Go Edition?  (Upcoming)

blockchain-lite library (github: [`openblockchains/blockchain.lite.go`](https://github.com/openblockchains/blockchain.lite.go) -
build your own blockchain with crypto hashes -
revolutionize the world with blockchains, blockchains, blockchains one block at a time

**Usage**

Let's get started.  Build your own blockchain one block at a time.

``` go
import(
   blk "github.com/openblockchains/blockchain.lite.go"
)

b0 := blk.NewBlock( "Hello, Cryptos!", "0000000000000000000000000000000000000000000000000000000000000000" )
b1 := blk.NewBlock( "Hello, Cryptos! - Hello, Cryptos!", b0.Hash )

blockchain := []Block {b0, b1}

fmt.Println( blockchain )
```



# Case Study -  Dutch Gulden on the Blockchain!

!! update chart !!


![](i/guldenmarket.png)

(Source: [coinmarketcap.com/currencies/gulden](https://coinmarketcap.com/currencies/gulden))


# Schilling! Schilling! on the Blockchain! Rock-Solid Alpine Dollar from Austria

Who's in? Invest now!

Crypto #Schilling on the #Blockchain in 197 Days 7 Hours 30 Minutes!

Join the Rock-Solid Alpine Dollar Movement!

Learn more @ [bitshilling/bitshilling](https://github.com/bitshilling)



# What's Next? Beyond Currencies / Money

Crypto Collectibles - Non Fungible Tokens (NFTs) - Unique Bits on the Blockchain

- Fun, Fun, Fun - Blockchain Gambling Casinos
  - Kitties, Puppies, Dragons, Lambos, ...
- Assets, Deeds, Titles ,...
  - Land, Houses, Appartments, ...
  - Tickets, ...


# CryptoKitties (Yes, Cute Little Cartoon Cats) on the Blockchain!

Collectible. Breedable. Adorable.

Collect and breed digital cats. Start meow. Buy! Sell! Hold!

Learn more @ [cryptokitties.co](https://cryptokitties.co)

![](i/cryptokitties.png)


# CryptoKittes & CryptoCollectibles

- Awesome CryptoKitties                  @ CryptoCopycats
- Awesome CryptoCollectibles (& Assets)  @ CryptoCopycats  


# Awesome Blockchains

A collection about awesome blockchains - open distributed public databases w/ crypto hashes incl. git ;-). Blockchains are the new tulips. Distributed is the new centralized.

More @ [openblockchains/awesome-blockchains](https://github.com/openblockchains/awesome-blockchains)


# Articles - Build Your Own Blockchains from Scratch (Zero) in Go

Series by


Series by



# Austrian  Blockchain Token Whitepapers - Free Easy Money - Scam Alert

Thanks for your money! Thanks for holding the bag!



# Case Study - Hero (PLAY) by Byte Heroes



# Case Study - Crowd (CRWD) by Conda Austria



# Case Study - Pandos by Bitpanda









# Bonus: Blockchain Books

[**Attack of the 50 Foot Blockchain: Bitcoin, Blockchain, Ethereum & Smart Contracts**](https://davidgerard.co.uk/blockchain/table-of-contents/) by David Gerard, London, 2017 --
_What is a bitcoin? ++
The Bitcoin ideology ++
The incredible promises of Bitcoin! ++
Early Bitcoin: the rise to the first bubble ++
How Bitcoin mining centralised ++
Who is Satoshi Nakamoto? ++
Spending bitcoins in 2017 ++
Trading bitcoins in 2017: the second crypto bubble ++
Altcoins ++
Smart contracts, stupid humans ++
Business bafflegab, but on the Blockchain ++
Case study: Why you can’t put the music industry on a blockchain_

[**Mastering Bitcoin - Programming the Open Blockchain**](https://github.com/bitcoinbook/bitcoinbook/blob/second_edition/ch09.asciidoc) 2nd Edition,
by Andreas M. Antonopoulos, 2017 - FREE (Online Source Version) --
_What Is Bitcoin? ++
How Bitcoin Works ++
Bitcoin Core: The Reference Implementation ++
Keys, Addresses ++
Wallets ++
Transactions ++
Advanced Transactions and Scripting ++
The Bitcoin Network ++
The Blockchain ++
Mining and Consensus ++
Bitcoin Security ++
Blockchain Applications_

[**Blockchain for Dummies, IBM Limited Edition**](https://www.ibm.com/blockchain/what-is-blockchain.html) by Manav Gupta, 2017 - FREE (Digital Download w/ Email) --
_Grasping Blockchain Fundamentals ++
Taking a Look at How Blockchain Works ++
Propelling Business with Blockchains ++
Blockchain in Action: Use Cases ++
Hyperledger, a Linux Foundation Project ++
Ten Steps to Your First Blockchain application_


!! add the truth machine !!!



# Bonus: Git, Git, Git - The Stupid Content Tracker with Crypto Hashes

_Everything is local. Distributed is the new centralized._

> Yep, that's the joke. Nobody has been able to explain to me how the "blockchain" buzzword is significantly different to "git repo".
> -- [Yaakov](https://twitter.com/yaakov_h/status/902659507255312384)
>
> But if you said "let's build a currency where all transactions are stored in a git repo"
> you wouldn't be taken seriously for even 24 hrs.
> -- [Yaakov](https://twitter.com/yaakov_h/status/902659847224664064)

> Soon explaining git like "a git repo is like a blockchain with commits instead of blocks".
> -- [Nicolás Berger](https://twitter.com/nicoberger/status/901776907418697729)
>
> "A local branch is like a state channel. It can be pushed and merged into the master blockchain at any moment."
> -- [Nicolás Berger](https://twitter.com/nicoberger/status/901777571456614400)

> The #Blockchain has changed the world. Here I make the argument that the #Blockchain is just like #git.
> -- [Jackson Kelley](https://twitter.com/sjkelleyjr/status/901464041163341824)

> `git merge [-m REF] [-g BLOB] --push`
>  Merge and push all signed commits to the blockchain.
> -- [Git Commands](https://twitter.com/git_commands/status/935574015015612416)


!! add xkcd !!
