title: Blockchain! Blockchain! Blockchain! - Build Your Own Blockchains in JavaScript from Zero (Scratch)


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

(Source: [openblockchains/schilling](https://github.com/openblockchains/schilling))



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



# $20 Million Dolloar Pizza Day - Celebrating the Birth of Bitcoin Alchemy - $$$

![](i/bitcoinmarket.png)

(Source: [coinmarketcap.com/currencies/bitcoin](https://coinmarketcap.com/currencies/bitcoin))




# Let's Build Your Own Blockchain in JavaScript (From Zero / Scratch)

Crypto God? JavaScript Ninja / Rockstar?

Yes, you can.

![](i/fake-dilbert-blockchain.png)




# Aside/Excursion - Land der Berge! Land der Blockchain! Blockchain! Blockchain! - Austria

> Diese Technologie hat unglaubliches Potenzial. Wir müssen schnell sein. Wer zögert, hat verloren!
>
> -- Harald Mahrer, Bundesminster Bundesminister für Wissenschaft, Forschung und Wirtschaft (bmwfw)


**Blockchain wird unsere Welt verändern.**
Von einer Idee hin zu einer Revolution. Blockchain ist aus unserer digitalen Welt nicht mehr wegzudenken...

**Ein dezentrales Netz für Österreich.**
Österreich ist am Sprung zur europäischen Innovationsspitze...

Das World Economic Forum prognostizierte, dass schon im Jahr 2025
insgesamt 10 Prozent des weltweiten Bruttoinlandsprodukts
mit Hilfe der Blockchain-Technologie abgewickelt werden.
Wenn wir uns heute nicht mit derartigen Technologien beschäftigen, tun es andere.

> Es gibt kein sichereres oder transparenteres System, um Daten zu verwalten.
>
> -- Harald Mahrer, Bundesminster Bundesminister für Wissenschaft, Forschung und Wirtschaft (bmwfw)


![](i/blockchainaustria.png)

(Source: [blockchain-austria.gv.at](https://www.blockchain-austria.gv.at))



# Aside/Excursion - Land der Berge! Land der Blockchain! Blockchain! Blockchain! - Austria

Keep Calm and Learn Git First.

> Wo ist die #Git Austria Initiative um (sicher & transparent)
> öffentliche  Daten, Dokumente und Kode zu verwalten? #digital #austria #bmwfw
>
> -- [Gerald Bauer @ Vienna.html](https://twitter.com/viennahtml/status/910776683552264195)

![](i/xkcd1597.png)



# Code, Code, Code - A Blockchain in JavaScript in 20 Lines! A Blockchain is a Data Structure

What's Blockchain?

It's a list (chain) of blocks linked and secured by digital fingerprints (also known as
crypto hashes).


```
const SHA256 = require( "js-sha256" )     // for hash checksum digest function SHA256

class Block {

  constructor(index, data, previousHash) {
    this.index        = index
    this.timestamp    = new Date()
    this.data         = data
    this.previousHash = previousHash
    this.hash         = this.calcHash()
  }

  calcHash() {
    var sha = SHA256.create()
    sha.update( this.index.toString() +
                this.timestamp.toString() +
                this.data +
                this.previousHash )
    return sha.hex()
  }
```

(Source: [openblockchains/awesome-blockchains/blockchain.js](https://github.com/openblockchains/awesome-blockchains/blob/master/blockchain.js/blockchain.js))

Yes, that's it.



# Code, Code, Code - A Blockchain in JavaScript in 20 Lines! A Blockchain is a Data Structure (Cont.)

Let's add two helpers (`first`, `next`) for building ("mining") blocks.

```
class Block {
  ...
  static first( data="Genesis" ) {    // create genesis (big bang! first) block
    // uses index zero (0) and arbitrary previousHash ("0")
    return new Block( 0, data, "0" )
  }

  static next( previous, data="Transaction Data..." ) {
    return new Block( previous.index+1, data, previous.hash )
  }
}
```

(Source: [openblockchains/awesome-blockchains/blockchain.js](https://github.com/openblockchains/awesome-blockchains/blob/master/blockchain.js/blockchain.js))

Let's get started -  build a blockchain a block at a time!

```
b0 = Block.first( "Genesis" )
b1 = Block.next( b0, "Transaction Data..." )
b2 = Block.next( b1, "Transaction Data......" )
b3 = Block.next( b2, "More Transaction Data..." )

blockchain = [b0, b1, b2, b3]

console.log( blockchain )
```


# Code, Code, Code - A Blockchain in JavaScript in 20 Lines! A Blockchain is a Data Structure (Cont.)

> Wait, so a blockchain is just a linked list?
>
> No. A linked list is only required to have a reference to the previous element,
> a block must have an identifier depending on the previous block's identifier,
> meaning that you cannot replace a block without recomputing every single block that comes after.
> In this implementation that happens as the previous digest is input in the calc_hash method.


will log something like:

```
[ Block {
      index: 0,
      timestamp: 2017-09-18T08:25:54,
      data: 'Genesis',
      previousHash: '0',
      hash:         'c396de4c03ddb5275661982adc75ce5fc5905d2a2457d1266c74436c1f3c50f1' },
    Block {
      index: 1,
      timestamp: 2017-09-18T08:25:54,
      data: 'Transaction Data...',
      previousHash: 'c396de4c03ddb5275661982adc75ce5fc5905d2a2457d1266c74436c1f3c50f1',
      hash:         '493131e09c069645c82795c96e4715cea0f5558be514b5096d853a5b9899154a' },
    Block {
      index: 2,
      timestamp: 2017-09-18T08:25:54,
      data: 'Transaction Data......',
      previousHash: '493131e09c069645c82795c96e4715cea0f5558be514b5096d853a5b9899154a',
      hash:         '97aa3cb5052615d60ff8e6b41bef606562588c4874f011970ac2f218e2f0f4a8' },
    Block {
      index: 3,
      timestamp: 2017-09-18T08:25:54,
      data: 'More Transaction Data...',
      previousHash: '97aa3cb5052615d60ff8e6b41bef606562588c4874f011970ac2f218e2f0f4a8',
      hash:         'e10e020f832e46c2b60e1c3c0412bd370b2fde5f0f782c16eb87d0313ea0d3a3' } ]
```


# What about Proof-of-Work? What about Consensus?

Making (Hash) Mining a Lottery - Find the Lucky Number

```
calcHash() {
  var sha = SHA256.create()
  sha.update( this.index.toString() +
              this.timestamp.toString() +
              this.data +
              this.previousHash )
  return sha.hex()
}
```

The computer (node) in the blockchain network that computes the
next block with a valid hash wins the lottery?

For adding a block to the chain you get a reward! You get 25 Bitcoin! (†)

Bitcoin adds a block every twenty minutes.

(†) The reward gets halfed. In Sep'17 you'll get 12.5 Bitcoin.



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

```
computeHashWithProofOfWork( difficulty="00" ) {
    var nonce = 0
    while( true ) {
      var hash = this.calcHashWithNonce( nonce )
      if( hash.startsWith( difficulty ))
        return nonce, hash    // bingo! proof of work if hash starts with leading zeros (00)
      else
        nonce += 1            // keep trying (and trying and trying)
    }
}

calcHashWithNonce( nonce=0 ) {
    var sha = SHA256.create()
    sha.update( nonce.toString() +
                this.index.toString() +
                this.timestamp.toString() +
                this.data +
                this.previousHash )
    return sha.hex()
}
```

(Source: [awesome-blockchains/blockchain_with_proof_of_work.js](https://github.com/openblockchains/awesome-blockchains/blob/master/blockchain.js/blockchain_with_proof_of_work.js))




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
- (C) Red Augustus


# More

- Tulips on the Blockchain!

- What's Blockchain Lite?  


- Add Blockchain Technology Made In Austria - Free! Gratis!
- Dutch Gulden
- Schilling! Schilling! on the Blockchain! Rock Solid


# Bonus:

- Blockchain Articles
- Blockchain Books
- Awesome Blockchains
