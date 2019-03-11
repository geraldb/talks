

title: Contracts, Contracts, Contracts - Code Your Own (Crypto Blockchain) Contracts w/ Ruby (sruby), Universum & Co



# What's a (Smart) Contract?

It's code that runs "on chain".

What's "on chain"?

It's the opposite of "off chain" :-).

"On chain" is code that runs on
every (computer) node the same way (in a deterministic way)
and secured by crypto(graphy) on the blockchain (peer-to-peer) network
with every transaction and transaction receipt recorded tamper-proof
"forever".



# Contract-Oriented Programming Languages - Genesis - Bitcoin Script

Bitcoin - the mother of all (crypto) ponzies and bubbles
has a (very limited low-level) scripting language
called Bitcoin Script.

Very limited?

- "Stateless", that is, no storage.
- No loops.
- Can check signatures & time locks and not much more.

Example - Freezing funds until a time in the future:

```
scriptPubKey: <expiry time> OP_CHECKLOCKTIMEVERIFY OP_DROP OP_DUP OP_HASH160 <pubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
scriptSig: <sig> <pubKey>
```
(Source: [Script @ Bitcoin Wiki](https://en.bitcoin.it/wiki/Script))

Why?


# Contract-Oriented Programming Languages - Turing Complete and the Halting Problem

If you have a Turing complete programming language
than you can code anything.

What's the Halting Problem?

If you can code anything - how can you proof that the code will halt (stop)
and not run forever (in an endless loop, for example)? Spoiler: You can't.

> Gödel's incompleteness theorems were the first of several closely related theorems
> on the limitations of formal systems.
> They were followed by Tarski's undefinability theorem on the formal undefinability
> of truth, Church's proof that Hilbert's Entscheidungsproblem is unsolvable, and
> Turing's theorem that there is no algorithm to solve the halting problem
>
> (Source: [Gödel's incompleteness theorems @ Wikipedia](https://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems))


# Contract-Oriented Programming Languages - Fees, Fees, Fees - $$$ - There's No Free Lunch

Solving the Halting Problem for Contract Code

You have to pay money for every "on chain" CPU cycle / bytecode instruction.

You have to pay money for every "on chain" storage byte.

Will the code halt (stop)? Yes, because you will run out-of-money :-)
and for extra safety there's a maximum limit of CPU cycle / bytecode instructions per block.



# Contract-Oriented Programming Languages - What's the World's Most Popular Programming Language?

Python? JavaScript?

Solidity - JavaScript-like (Static)
Contract-Oriented Programming Languages with Types

```
contract GavToken
{
  mapping(address=>uint) balances;
  uint constant totalTokens = 100000000000;

  /// Endows creator of contract with 1m GAV.
  function GavToken(){
      balances[msg.sender] = totalTokens;
  }

  /// Send $((valueInmGAV / 1000).fixed(0,3)) GAV from the account of
  ///   $(message.caller.address()), to an account accessible only by $(to.address()).
  function send(address to, uint256 valueInmGAV) {
    if (balances[msg.sender] >= valueInmGAV) {
      balances[to] += valueInmGAV;
      balances[msg.sender] -= valueInmGAV;
    }
  }

  /// getter function for the balance
  function balance(address who) constant returns (uint256 balanceInmGAV) {
    balanceInmGAV = balances[who];
  }
}
```

Compiles to Ethereum Virtual Machine (EVM) Bytecode



# Contract-Oriented Programming Languages - What's the World's Most Popular Programming Language?

Python? JavaScript?

~~Serepent~~  Vyper - Python-like (Static)
Contract-Oriented Programming Languages with Types (Python 3-Compatible Syntax)

```
Transfer: event({_from: indexed(address), _to: indexed(address), _value: uint256})
Approval: event({_owner: indexed(address), _spender: indexed(address), _value: uint256})


# Variables of the token.
name: public(bytes32)
symbol: public(bytes32)
totalSupply: public(uint256)
decimals: public(uint256)
balances: int128[address]
allowed: int128[address][address]

@public
def __init__(_name: bytes32, _symbol: bytes32, _decimals: uint256, _initialSupply: uint256):

    self.name = _name
    self.symbol = _symbol
    self.decimals = _decimals
    self.totalSupply = uint256_mul(_initialSupply, uint256_exp(convert(10, 'uint256'), _decimals))
    self.balances[msg.sender] = convert(self.totalSupply, 'int128')


# What is the balance of a particular account?
@public
@constant
def balanceOf(_owner: address) -> uint256:

    return convert(self.balances[_owner], 'uint256')


# Send `_value` tokens to `_to` from your account
@public
def transfer(_to: address, _value: int128(uint256)) -> bool:

    assert self.balances[msg.sender] >= _value
    assert self.balances[_to] + _value >= self.balances[_to]

    self.balances[msg.sender] -= _value  # Subtract from the sender
    self.balances[_to] += _value  # Add the same to the recipient
    log.Transfer(msg.sender, _to, convert(_value, 'uint256'))  # log transfer event.

    return True

# ...
```

Compiles to Ethereum Virtual Machine (EVM) Bytecode





# Contract-Oriented Programming Languages - Why Not Just Ruby?

Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby

The Ruby Programming Language for Contract / Transaction Scripts
on the Blockchain World Computer - Yes, It's Just Ruby


``` ruby
######################
# Token Contract

event :Transfer, :from, :to, :value
event :Approval, :owner, :spender, :value

def setup( name, symbol, decimals, initial_supply )
  @name     = name
  @symbol   = symbol
  @decimals = decimals
  @total_supply =  initial_supply * (10 ** decimals)
  @balances     =  Mapping.of( Address => Money )
  @balances[msg.sender] = @total_supply
  @allowed      =  Mapping.of( Adress => Mapping.of( Address => Money ))
end


# What is the balance of a particular account?
def balance_of( owner )
  @balances[owner]
end

# Send `_value` tokens to `_to` from your account
def transfer( to, value )
  assert @balances[msg.sender] >= value
  assert @balances[to] + value >= @balances[to]

  @balances[msg.sender] -= value  # Subtract from the sender
  @balances[to]         += value  # Add the same to the recipient

  log Transfer.new( msg.sender, to, value )   # log transfer event.

  true
end

## ...
```

Runs on "plain vanilla" ruby (*).

(*) with the universum (runtime) library




# 10 Contracts - Contracts by Example

- Hello, World! - Greeter
- Piggy Bank
- Simple Storage
- Simple Ponzi - King of Ether
- Ballot - Liquid / Delegative Democracy - Let's Vote
- Roll the (Satoshi) Dice - Win x65 000 (Casino Gambling)
- Tic-Tac-Toe
- Kick Start Your Project with a Crowd Funder
- PonzICO - Blockchain Performance Art
- Powerball Mega Millions Lottery


# Contracts by Example - Hello, World! - Greeter


# Networks, Networks, Networks - Running the Contract Code

- Main Network
- Test Network
- Private (Local) Test Network


# Contracts by Example - Hello, World! - Greeter - Running on the Private (Local) Test Network



# Contracts by Example - Piggy Bank

# Contracts by Example - Simple Storage

# Contracts by Example - Simple Ponzi - King of Ether

# Contracts by Example - Ballot - Liquid / Delegative Democracy - Let's Vote

# Contracts by Example - Roll the (Satoshi) Dice - Win x65 000 (Casino Gambling)

# Contracts by Example - Tic-Tac-Toe

# Contracts by Example - Kick Start Your Project with a Crowd Funder

# Contracts by Example - PonzICO - Blockchain Performance Art

# Contracts by Example - Powerball Mega Millions Lottery




# New to (Secure) Ruby Contract Programming / Scripting?
