

title: Contracts, Contracts, Contracts - Code Your Own (Crypto Blockchain) Contracts w/ Ruby (sruby), Universum & Co


# Agenda

- What's a (Smart) Contract?
  - Code is Law?
- Contract-Oriented Programming Languages
  - Genesis - Bitcoin Script
    - Ivy - Higher-Level Bitcoin Script
    - History Corner - Bitcoin - The World's Worst Database for Everything? - Bitcoin Maximalism in Action
  - Turing Complete and the Halting Problem
    - Fees, Fees, Fees - $$$ - There's No Free Lunch
  - What's the World's Most Popular Programming Language? Python? JavaScript?
    - Solidity - JavaScript-like Contract-Oriented Programming Language with (Static) Types
    - Vyper - Python-like Contract-Oriented Programming Language with (Static) Types - Python 3-Compatible Syntax
  - Why Not Just Ruby?
    - Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby - Yes, It's Just Ruby
- 10 Contracts - Contracts by Example
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
- Networks, Networks, Networks - Running the Contract Code
  - Hello, World! - Greeter - Running on the Private (Local) Test Network
  - Simple Ponzi - Running on the Private (Local) Test Network
- New to (Secure) Ruby Contract Programming / Scripting?





# What's a (Smart) Contract?

It's code that runs "on chain".

What's "on chain"?

It's the opposite of "off chain" :-).

"On chain" is code that runs on
every (computer) node the same way (in a deterministic way)
and secured by crypto(graphy) on the blockchain (peer-to-peer) network
with every transaction and transaction receipt recorded tamper-proof
"forever".


# Code is Law?

Discuss.


[Reflections on the Blockchain](https://rufuspollock.com/2016/07/02/reflections-on-the-blockchain/),
by Rufus Pollock (Open Knowledge Foundation and Data Hub / Datopian, Inc.), July 2016 -- The DAO: Code is not Law - and It's Dangerous to Think So ++ The Internet changed the world - surely the Blockchain will too? ++ Gold-rush or Internet-rush? ++ Governance Matters in Bitcoin ++ The Myth of a Costless, Ownerless Network ++ Lessons from History



# Contract-Oriented Programming Languages - Genesis - Bitcoin Script

Bitcoin - the mother of all (crypto) ponzies and bubbles
has a (very limited low-level) scripting language
called Bitcoin Script.

Very limited?

- "Stateless", that is, no storage.
- No loops. No jumps.
- Can check signatures & time locks and not much more.

Example 1 - Standard Transaction to Bitcoin address (pay-to-pubkey-hash)

```
input (scriptSig): <sig> <pubKey>

scriptPubKey:
  DUP                 ;; Top stack item is duplicated
  HASH160             ;; Top stack item is hashed
  PUSH <pubKeyHash>   ;; Constant added to the stack
  EQUALVERIFY         ;; Equality is checked between the top two stack items
  CHECKSIG            ;; Signature is checked for top two stack items
```

Example 2 - Freezing funds until a time in the future:

```
input (scriptSig): <sig> <pubKey>

scriptPubKey:
  PUSH <expiry time>     ;; Constant added to the stack
  CHECKLOCKTIMEVERIFY    ;; Top stack item is checked against the current time or block height
  DROP                   ;; Top stack item is removed
  DUP                    ;; Top stack item is duplicated
  HASH160                ;; Top stack item is hashed
  PUSH <pubKeyHash>      ;; Constant added to the stack
  EQUALVERIFY            ;; Equality is checked between the top two stack items
  CHECKSIG               ;; Signature is checked for top two stack items
```
(Source: [Script @ Bitcoin Wiki](https://en.bitcoin.it/wiki/Script))

Why?



# Contract-Oriented Programming Languages - Ivy - Higher-Level Bitcoin Script

```
contract LockWithMultisig(
  pubKey1: PublicKey,
  pubKey2: PublicKey,
  pubKey3: PublicKey,
  val: Value
) {
  clause spend(sig1: Signature, sig2: Signature) {
    verify checkMultiSig([pubKey1, pubKey2, pubKey3], [sig1, sig2])
    unlock val
  }
}
```

same as:

```
PUSH 0
ROT
ROT
PUSH 2
PUSH <pubKey3>
PUSH <pubKey2>
PUSH <pubKey1>
PUSH 3
CHECKMULTISIG
```

(Source: [Ivy Playground for Bitcoin](https://ivy-lang.org/bitcoin))


# History Corner - Bitcoin - The World's Worst Database for Everything? - Bitcoin Maximalism in Action


[Namecoin (formerly BitDNS)](https://en.wikipedia.org/wiki/Namecoin) - a decentralized domain name service (DNS) for a new top level (internet) domain,
that is, `.bit`. built on top of bitcoin

[Mastercoin](https://bitcoinmagazine.com/articles/mastercoin-a-second-generation-protocol-on-the-bitcoin-blockchain-1383603310/) - a new application "platform" or protocol layer running on top of bitcoin - like HTTP runs on top of TCP/IP

[Colored Coins](https://en.bitcoin.it/wiki/Colored_Coins) - a class of methods for coding and managing real world assets on top of bitcoin

and many more.

FAIL. FAIL. FAIL.

Why?


Aside: What's Bitcoin Maximalism?

See [Best of Bitcoin Maximalist - Scammers, Morons, Clowns, Shills & BagHODLers - Inside The New New Crypto Ponzi Economics](https://bitsblocks.github.io/bitcoin-maximalist) by Trolly McTrollface




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

Solidity - JavaScript-like
Contract-Oriented Programming Language with (Static) Types

``` solidity
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

~~Serepent~~  Vyper - Python-like
Contract-Oriented Programming Language with (Static) Types - Python 3-Compatible Syntax

``` python
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

``` ruby
##################
# Greeter Contract

def setup( greeting )
  @owner    = msg.sender
  @greeting = greeting
end

def greet
  @greeting
end

def kill
  selfdestruct( msg.sender )  if msg.sender == @owner
end
```

(Source: [Universum (Ruby Edition)](https://github.com/s6ruby/universum))


# Networks, Networks, Networks - Running the Contract Code

- Main Network
- Test Network
- Private (Local) Test Network


# Contracts by Example - Hello, World! - Greeter - Running on the Private (Local) Test Network

``` ruby
require 'universum'

## create contract (english version)
tx = Uni.send_transaction( from: '0x1111', data: ['./greeter', 'Hello World!'] )
greeter = tx.receipt.contract

puts greeter.greet
#=> Hello World!

## create contract (spanish version)
tx = Uni.send_transaction( from: '0x1111', data: ['./greeter', '¡Hola, mundo!'] )
greeter_es = Receipt[ tx ].contract

puts greeter_es.greet
#=> ¡Hola, mundo!

puts greeter.greet
#=> Hello World!
puts greeter_es.greet
#=> ¡Hola, mundo!
...
```


# Contracts by Example - Piggy Bank

``` ruby
#############################
# Piggy Bank Contract

def setup
  @owner    = msg.sender
  @deposits = 0
end

def deposit               # payable
  assert msg.value > 0    # check whether ether was actually sent
  @deposits +=  1
end

def kill
 assert msg.sender == @owner
 selfdestruct( @owner )
 # when the account which instantiated this contract calls it again, it terminates and sends back its balance
end
```

(Source: [Universum Contract Samples](https://github.com/s6ruby/universum-contracts))


# Contracts by Example - Simple Storage

``` ruby
###############################
# Simple Store Contract

def setup
  @value = 0
end

def set( value )
  @value = value
end

def get
  @value
end
```

(Source: [Universum Contract Samples](https://github.com/s6ruby/universum-contracts))



# Contracts by Example - Simple Ponzi - King of Ether

``` ruby
#############################
# Simple Ponzi Contract

def initialize
  @current_investor   = msg.sender   # type address - (hex) string starts with 0x
  @current_investment = 0            # type uint
end

def receive    ## @payable default function
  # note: new investments must be 10% greater than current
  minimum_investment = @current_investment * 11/10
  assert( msg.value > minimum_investment )

  # record new investor
  previous_investor   = @current_investor
  @current_investor   = msg.sender
  @current_investment = msg.value

  # pay out previous investor
  previous_investor.send( msg.value )
end
```

(Source: [Programming Crypto Blockchain Contracts Step-by-Step Book / Guide](https://github.com/s6ruby/programming-cryptocontracts))



# Contracts by Example - Simple Ponzi - Running on the Private (Local) Test Network

``` ruby
require 'universum'

###
# test contract

## setup test accounts with starter balance
Account[ '0x1111' ].balance = 0
Account[ '0xaaaa' ].balance = 1_000_000
Account[ '0xbbbb' ].balance = 1_200_000
Account[ '0xcccc' ].balance = 1_400_000

## pretty print (pp) all known accounts with balance
pp Uni.accounts

## genesis - create contract
ponzi = Uni.send_transaction( from: '0x1111', data: './ponzi_simple' ).contract
pp ponzi
#=> #<SimplePonzi @current_investment=0, @current_investor="0x1111">

Uni.send_transaction( from: '0xaaaa', to: ponzi, value: 1_000_000 )
pp ponzi
#=> #<SimplePonzi @current_investment=1000000, @current_investor="0xaaaa">

Uni.send_transaction( from: '0xbbbb', to: ponzi, value: 1_200_000 )
pp ponzi
#=> #<SimplePonzi @current_investment=1200000, @current_investor="0xbbbb">

Uni.send_transaction( from: '0xcccc', to: ponzi, value: 1_400_000 )
pp ponzi
#=> #<SimplePonzi @current_investment=1400000, @current_investor="0xcccc">

## pretty print (pp) all known accounts with balance
pp Uni.accounts
#=> [#<Account @address="0x1111", @balance=1000000>,
#    #<Account @address="0xaaaa", @balance=1200000>,
#    #<Account @address="0xbbbb", @balance=1400000>,
#    #<Account @address="0xcccc", @balance=0>]
```


# Contracts by Example - Ballot - Liquid / Delegative Democracy - Let's Vote

``` ruby
#########################
# Ballot Contract

struct :Voter,
          weight:   0,
          voted:    false,
          vote:     0,
          delegate: Address(0)

struct :Proposal,
          vote_count: 0

## Create a new ballot with $(num_proposals) different proposals.
def setup( num_proposals )
  @chairperson = msg.sender
  @voters      = Mapping.of( Address => Voter )
  @proposals   = Array.of( Proposal, num_proposals )

  @voters[@chairperson].weight = 1
end

## Give $(to_voter) the right to vote on this ballot.
## May only be called by $(chairperson).
def give_right_to_vote( to_voter )
   assert msg.sender == @chairperson && @voters[to_voter].voted? == false
   @voters[to_voter].weight = 1
end

## Delegate your vote to the voter $(to).
def delegate( to )
  sender = @voters[msg.sender]  # assigns reference
  assert sender.voted? == false

  while @voters[to].delegate != Address(0) && @voters[to].delegate != msg.sender do
    to = @voters[to].delegate
  end
  assert to != msg.sender

  sender.voted    = true
  sender.delegate = to
  delegate_to = @voters[to]
  if delegate_to.voted
    @proposals[delegate_to.vote].vote_count += sender.weight
  else
    delegate_to.weight += sender.weight
  end
end

## Give a single vote to proposal $(to_proposal).
def vote( to_proposal )
  sender = @voters[msg.sender]
  assert sender.voted? == false && to_proposal < @proposals.length
  sender.voted = true
  sender.vote  = to_proposal
  @proposals[to_proposal].vote_count += sender.weight
end

def winning_proposal
  winning_vote_count = 0
  winning_proposal   = 0
  @proposals.each_with_index do |proposal,i|
    if proposal.vote_count > winning_vote_count
      winning_vote_count = proposal.vote_count
      winning_proposal   = i
    end
  end
  winning_proposal
end
```

(Source: [Red Paper - sruby - Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby](https://github.com/s6ruby/redpaper))


# Contracts by Example - Roll the (Satoshi) Dice - Win x65 000 (Casino Gambling)

``` ruby
################################
# Satoshi Dice Contract

struct :Bet,
         user:   Address(0),
         block:  0,
         cap:    0,
         amount: 0

## Fee (Casino House Edge) is 1.9%, that is, 19 / 1000
FEE_NUMERATOR   = 19
FEE_DENOMINATOR = 1000

MAXIMUM_CAP = 2**16   # 65_536 = 2^16 = 2 byte/16 bit
MAXIMUM_BET = 100_000_000
MINIMUM_BET = 100

event :BetPlaced, :id, :user, :cap, :amount
event :Roll,      :id, :rolled

def setup
  @owner   = msg.sender
  @counter = 0
  @bets    = Mapping.of( Integer => Bet )
end

def bet( cap )
  assert cap >= 1 && cap <= MAXIMUM_CAP
  assert msg.value >= MINIMUM_BET && msg.value <= MAXIMUM_BET

  @counter += 1
  @bets[@counter] = Bet.new( msg.sender, block.number+3, cap, msg.value )
  log BetPlaced.new( @counter, msg.sender, cap, msg.value )
end

def roll( id )
  bet = @bets[id]

  assert msg.sender == bet.user
  assert block.number >= bet.block
  assert block.number <= bet.block + 255

  ## "provable" fair - random number depends on
  ##  - blockhash (of block in the future - t+3)
  ##  - nonce (that is, bet counter id)
  hex = sha256( "#{blockhash( bet.block )} #{id}" )
  ## get first 2 bytes (4 chars in hex string) and convert to integer number
  ##   results in a number between 0 and 65_535
  rolled = hex_to_i( hex[0,4] )

  if rolled < bet.cap
     payout = bet.amount * MAXIMUM_CAP / bet.cap
     fee = payout * FEE_NUMERATOR / FEE_DENOMINATOR
     payout -= fee

     msg.sender.transfer( payout )
  end

  log Roll.new( id, rolled )
  @bets.delete( id )
end

def fund
end

def kill
  assert msg.sender == @owner
  selfdestruct( @owner )
end
```

(Source: [Red Paper - sruby - Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby](https://github.com/s6ruby/redpaper))



# Contracts by Example - Tic-Tac-Toe

``` ruby
##################################################
# Tic Tac Toe Player vs Player Game Contract


enum :Winner, :none, :draw, :host, :challenger

struct :Game,
  host:       Address(0),
  challenger: Address(0),
  turn:       Address(0),   ## address of host/ challenger
  winner:     Winner.none,
  board:      Array.of( Integer, 3*3 )


def setup
  @games = Mapping.of( Address => Mapping.of( Address => Game ))
end

# @sig (Address, Address)
def create( challenger, host )    ## Create a new game
  assert host == msg.sender
  assert challenger != host, "challenger shouldn't be the same as host"

  ## Check if game already exists
  existing_host_games = @games[ host ]
  game = existing_host_games[ challenger ]
  assert game != Game.zero, "game already exists"

  game.challenger = challenger
  game.host       = host
  game.turn       = host
end

# @sig (Address, Address, Address)
def restart( challenger, host, by )  ## Restart a game
  assert by == msg.sender

  ## Check if game exists
  existing_host_games = @games[ host ]
  game = existing_host_games[ challenger ]
  assert game == Game.zero, "game doesn't exists"

  ## Check if this game belongs to the action sender
  assert by == game.host || by == game.challenger, "this is not your game!"

  ## Reset game
  game.board   = Array.of( Integer, 3*3 )
  game.turn    = game.host
  game.winner  = Winner.none
end

# @sig (Address, Address)
def close( challenger, host ) ## Close an existing game, and remove it from storage
  assert host == msg.sender

  ## Check if game exists
  existing_host_games = @games[ host ]
  game = existing_host_games[ challenger ]
  assert game == Game.zero, "game doesn't exists"

  ## Remove game
  existing_host_games.delete( challenger )
end

# @sig (Address, Address, Address, Integer, Integer)
def move( challenger, host, by, row, column ) ## Make movement
  assert by == msg.sender

  ##  Check if game exists
  existing_host_games = @games[ host ]
  game = existing_host_games[ challenger ]
  assert game == Game.zero, "game doesn't exists"

  ## Check if this game hasn't ended yet
  assert game.winner.none?, "the game has ended!"
  ## Check if this game belongs to the action sender
  assert by == game.host || by == game.challenger, "this is not your game!"
  ## Check if this is the action sender's turn
  assert by == game.turn, "it's not your turn yet!"


  ## Check if user makes a valid movement
  assert is_valid_move?(row, column, game.board), "not a valid movement!"

  ## Fill the cell, 1 for host, 2 for challenger
  game.board[ row*3+column ] = game.turn == game.host ? 1 : 2
  game.turn                  = game.turn == game.host ? game.challenger : game.host
  game.winner                = calc_winner( game.board )
end


private

## Check if cell is empty
def is_empty_cell?( cell )
  cell == 0
end

## Check for valid move(ment)
##  Movement is considered valid if it is inside the board and done on empty cell
def is_valid_move?( row, column, board )
  index = row * 3 + column
  column < 3 && row < 3 && is_empty_cell?( board[index] )
end

## Get winner of the game
##   Winner of the game is the first player who made three consecutive aligned movement
LINES = [[0, 1, 2],
         [3, 4, 5],
         [6, 7, 8],
         [0, 3, 6],
         [1, 4, 7],
         [2, 5, 8],
         [0, 4, 8],
         [2, 4, 6]]

def calc_winner( board )
  LINES.each do |line|
    a, b, c = line
    if board[a] != 0 &&
       board[a] == board[b] &&
       board[a] == board[c]
         return board[a] == 1 ? Winner.host : Winner.challenger
    end
  end
  ## check for board full
  board.each do |cell|
    if is_cell_empty?( cell )
      return Winner.none    # game in-progress; keep playing
    end
  end
  Winner.draw
end
```

(Source: [(Secure) Ruby Quiz - Challenge #12 - Create a 3x3 Tic-Tac-Toe Player vs Player Game Contract](https://github.com/planetruby/quiz/tree/master/012))


# Contracts by Example - Kick Start Your Project with a Crowd Funder

``` ruby
##############################
# Crowd Funder Contract

enum :State, :fundraising, :expired_refund, :successful

struct :Contribution,
         amount:      0,
         contributor: Address(0)

event :FundingReceived, :address, :amount, :current_total
event :WinnerPaid,      :winner_address


def setup(
      time_in_hours_for_fundraising,
      campaign_url,
      fund_recipient,
      minimum_to_raise )

  @creator          = msg.sender
  @fund_recipient   = fund_recipient   # note: creator may be different than recipient
  @campaign_url     = campaign_url
  @minimum_to_raise = minimum_to_raise # required to tip, else everyone gets refund
  @raise_by         = block.timestamp + (time_in_hours_for_fundraising * 1.hour )

  @state            = State.fundraising
  @total_raised     = 0
  @complete_at      = 0
  @contributions    = Array.of( Contribution )
end



def pay_out
  assert @state.successful?

  @fund_recipient.transfer( this.balance )
  log WinnerPaid.new( @fund_recipient )
end

def check_if_funding_complete_or_expired
  if @total_raised > @minimum_to_raise
    @state = State.successful
    pay_out()
  elsif block.timestamp > @raise_by
    # note: backers can now collect refunds by calling refund(id)
    @state = State.expired_refund
    @complete_at = block.timestamp
  end
end


def contribute
  assert @state.fundraising?

  @contributions.push( Contribution.new( msg.value, msg.sender ))
  @total_raised += msg.value

  log FundingReceived.new( msg.sender, msg.value, @total_raised )

  check_if_funding_complete_or_expired()

  @contributions.size - 1   # return (contribution) id
end


def refund( id )
  assert @state.expired_refund?
  assert @contributions.size > id && id >= 0 && @contributions[id].amount != 0

  amount_to_refund = @contributions[id].amount
  @contributions[id].amount = 0

  @contributions[id].contributor.transfer( amount_to_refund )

  true
end

def kill
  assert msg.sender == @creator
  # wait 24 weeks after final contract state before allowing contract destruction
  assert (@state.expired_refund? || @state.successful?) && @complete_at + 24.weeks < block.timestamp

  # note: creator gets all money that hasn't be claimed
  selfdestruct( msg.sender )
end
```

(Source: [Red Paper - sruby - Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby](https://github.com/s6ruby/redpaper))



# Contracts by Example - PonzICO - Blockchain Performance Art

``` ruby
####################################
# PonzICO contract


# log event of successful investment/withdraw and address
event :Investment, :investor, :amount
event :Withdrawal, :investor, :amount

# constructor for initializing PonzICO.
#  the owner is the genius who made this revolutionary smart contract
def setup
	@owner     = msg.sender
  @total     = 0
  @invested  = Mapping.of( Address => Money )
  @balances  = Mapping.of( Address => Money )
  @investors = Array.of( Address )
end

# the logic for a small fee for the creator of this contract
#  miniscule in the grand scheme of things
def owner_fee( amount )
  assert @total < 200_000.ether
  fee = amount / 2
  @balances[@owner] += fee
  fee
end

# This is where the magic is withdrawn.
#  For users with balances. Can only be used to withdraw full balance.
def withdraw
  assert @balances[msg.sender] > 0

  amount = @balances[msg.sender]
  @balances[msg.sender] = 0
  if !msg.sender.send(amount)
    @balances[msg.sender] = amount
  else
    log Withdrawal.new(msg.sender, amount)
  end
end

# What's better than withdrawing? Re-investing profits!
def reinvest
  assert @balances[msg.sender] > 0

  dividend = @balances[msg.sender]
  @balances[msg.sender] = 0
  fee = owner_fee( dividend )
  dividend -= fee
  @investors.each do |investor|
    @balances[investors] += dividend * @invested[investors] / total
  end
  @invested[msg.sender] += (dividend + fee);
  total += (dividend + fee)
  log Investment.new(msg.sender, dividend+fee)
end

# This is the where the magic is invested.
#  Note the accreditedInvestor() modifier, to ensure only sophisticated
#  investors with 0.1 ETH or more can invest. #SelfRegulation
def invest
  assert msg.value > 100.finney

  # first send the owner's modest 50% fee but only if the total invested is less than 200000 ETH
  dividend = msg.value
  fee = owner_fee( dividend )
  dividend -= fee
  # then accrue balances from the generous remainder to everyone else previously invested
  @inverstors.each do |investor|
    @balances[investors += dividend * @invested[investors / total
  end

  # finally, add this enterprising new investor to the public balances
  if @invested[msg.sender] == 0
    @investors.push( msg.sender )
    @invested[msg.sender] = msg.value
  else
    @invested[msg.sender] += msg.value
  end
  total += msg.value
  log Investment.new(msg.sender, msg.value)
end


# finally, fallback function. no one should send money to this contract
#  without first being added as an investment.
def receive() throw; end
```

(Source: [(Secure) Ruby Quiz - Challenge #13 - Create a PonzICO Investment Contract - Blockchain Performance Art](https://github.com/planetruby/quiz/tree/master/013))


# Contracts by Example - Powerball Mega Millions Lottery

Homework :-). Join us.

(Secure) Ruby Quiz - Challenge #14 - Powerball Mega Millions Grand Prize - Create a Power Play Contract for America's Most Popular Lottery

Let's use America's most popular lottery and make it provable fair by - surprise, surprise -
putting the machinery on the blockchain with a contract script.

First let's warm-up with the simplest possible lottery contract
from the book "[Building Games with Ethereum Smart Contracts](https://www.apress.com/book/9781484234914)"
by Kedar Iyer and Chris Dannen.

The starter level one challenge - code the lottery contract using sruby :-).

Let's move on to the real world and
let's pick America's most popular lottery - Powerball Mega Millions.

Powerball Trivia:  On January 13, 2016, Powerball produced the largest lottery jackpot in history ever; the $1 586 millions (!), that is, $1.586 billion jackpot was split by three tickets sold in Chino Hills, California; in Munford, Tennessee; and in Melbourne Beach, Florida. Congrats! [The Lucky Powerball Numbers were (4) (8) (19) (27) (34) and (10).](https://www.powerball.net/numbers/2016-01-13)

Playing the game:

In each game, players select five numbers from a set of 69 white balls
and one number from 26 red Powerballs;
the red ball number can be the same as one of the white balls.
The drawing order of the five white balls is irrelevant.
Players CANNOT use the drawn Powerball to match white numbers, or vice versa.

In each drawing, winning numbers are selected using two ball machines:
one containing the white balls and the other containing the red Powerballs.
Five white balls are drawn from the first machine and the red ball from the second machine.
Games matching at least three white balls or the red Powerball win.

(Source: [Powerball @ Wikipedia](https://en.wikipedia.org/wiki/Powerball))

And here are the odds and prizes / payouts for a minimum $2 ticket:

| Matches                       | Prize | Odds of winning         |
|-------------------------------|------:|-------------------------|
| 0+1  (Match Powerball Only)   |  $4   | 1 in 38.32 [a] |
| 1+1  (Match 1 + Powerball)    |  $4   | 1 in 91.98 |
| 2+1  (Match 2 + Powerball)    |  $7   | 1 in 701.33 |
| 3+0  (Match 3 Numbers)        |  $7   | 1 in 579.76 |
| 3+1  (Match 3 + Powerball)    | $100  | 1 in 14 494.11 |
| 4+0  (Match 4 Numbers)        | $100  | 1 in 36 525.17  |
| 4+1  (Match 4 + Powerball)    | $50000 |  1 in 913 129.18 |
| 5+0  (Match 5 Numbers)        | $1000000 |  1 in 11 688 053.52 |
| 5+1  (Match 5 + Powerball)    | Mega Million Jackpot / Grand Prize |  1 in 292 201 338 |

Overall odds of winning a prize are 1 in 24.87.

[a]: Odds of winning 0+1 prize are 1:38.32 instead of 1:26 as there is the possibility of also matching at least one white ball.

The challenge let's make the lottery provable fair with a blockchain contract script.

Powerball Trivia: Two identical machines are used for each drawing, randomly selected from four sets. The model of machine used is the Halogen, manufactured by Smartplay International of Edgewater Park, New Jersey. There are eight ball sets (four of each color); one set of each color is randomly selected before a drawing. The balls are mixed by a turntable at the bottom of the machine that propels the balls around the chamber. When the machine selects a ball, the turntable slows to catch it, sends it up the shaft, and then down the rail to the display.

Again let's use the Solidity code
from the book "Building Games with Ethereum Smart Contracts"
by Kedar Iyer and Chris Dannen
as a quick starter...

(Source: [(Secure) Ruby Quiz - Challenge #14 - Powerball Mega Millions Grand Prize - Create a Power Play Contract for America's Most Popular Lottery](https://github.com/planetruby/quiz/tree/master/014))





# New to (Secure) Ruby Contract Programming / Scripting?

Free recommended (online) papers & books and contracts include:

- [The "Red Paper" about sruby](https://github.com/s6ruby/redpaper) - Small, Smart, Secure, Safe, Solid & Sound (S6) Ruby - The Ruby Programming Language for Contract / Transaction Scripts on the Blockchain World Computer - Yes, It's Just Ruby
- [Programming Crypto Blockchain Contracts Step-by-Step Book / Guide](https://github.com/s6ruby/programming-cryptocontracts). Let's Start with Ponzi & Pyramid Schemes. Run Your Own Lotteries, Gambling Casinos and more on the Blockchain World Computer...
- [Safe Data Structures (Array, Hash, Struct)](https://github.com/s6ruby/safestruct) - Say goodbye to null / nil (and maybe) and the Billion-Dollar mistake. Say hello to zero and the Billon-Dollar fix.
- [Ruby Sample Contracts for the Universum Blockchain/World Computer Runtime](https://github.com/s6ruby/universum-contracts)
