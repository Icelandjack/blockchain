module Blockchain.Types

import Data.SortedMap
import Data.SortedMap as Map
import Data.SortedSet
import Data.SortedSet as Set
import Language.JSON

import Blockchain.Util
import Crypto.SHA

%access public export

-- | ERROR
data Error = NoParse | InvalidHash | HashDoesNotMatch | IncorrectDifficulty

Show Error where
 -- showPrec : Prec -> Error -> String
 showPrec prec NoParse             = "NoParse"
 showPrec prec InvalidHash         = "InvalidHash"
 showPrec prec HashDoesNotMatch    = "HashDoesNotMatch"
 showPrec prec IncorrectDifficulty = "IncorrectDifficulty"

-- | INPUT
record Input where
 constructor MkInput
 id       : Int
 inAmount : Int

Show Input where
 -- showPrec : Prec -> Input -> String
 showPrec prec (MkInput a b) = showCon prec "MkInput" (showArg a ++ showArg b)

-- | OUTPUT
record Output where
 constructor MkOutput
 id        : Int
 outAmount : Int

Eq Output where
 (MkOutput a b) == (MkOutput a' b') = and
  [ a == a'
  , b == b'
  ]

Ord Output where
 compare (MkOutput a b) (MkOutput a' b') = concat
  [ compare a a'
  , compare b b'
  ]

Show Output where
 -- showPrec : Prec -> Output -> String
 showPrec prec (MkOutput a b) = showCon prec "MkOutput" (showArg a ++ showArg b)

-- TRANSACTION
record Transaction where
 constructor MkTransaction
 inputs  : List Input
 outputs : List Output

Show Transaction where
 -- showPrec : Prec -> Transaction -> String
 showPrec prec (MkTransaction inputs outputs) = showCon prec "MkTransaction" (showArg inputs ++ showArg outputs)
 
-- | HASH DIFFICULTY
data HashDifficulty : Nat -> String -> Type where
 HashDifficultyZ : HashDifficulty 0 str
 HashDifficultyS : HashDifficulty n str
                -> HashDifficulty (S n) (strCons '0' str)

Show (HashDifficulty n str) where
 showPrec prec HashDifficultyZ        = "HashDifficultyZ"
 showPrec prec (HashDifficultyS diff) = showCon prec "HashDifficultyS" (showArg diff)

data OxHashDifficulty : Nat -> String -> Type where
 MkOxHashDifficulty : HashDifficulty difficulty hash -> OxHashDifficulty difficulty (strCons '0' (strCons 'x' hash))

Show (OxHashDifficulty n str) where
 showPrec prec (MkOxHashDifficulty hashDifficulty) = 
   showCon prec "MkOxHashDifficulty" (showArg hashDifficulty)

-- | BLOCK
record Block where
 constructor MkBlock

 difficulty      : Int
 blockHash       : String
 nonce           : Int
 predecessorHash : String
 transactions    : List Transaction

 check'd : OxHashDifficulty (cast difficulty) blockHash

Show Block where
 -- showPrec : Prec -> Block -> String
 showPrec prec (MkBlock difficulty hash nonce pred transactions check'd) = 
   showCon prec "MkBlock" $ concat 
    [ showArg difficulty
    , showArg hash
    , showArg nonce
    , showArg pred
    , showArg transactions
    , showArg check'd
    ]

-- | HEAD 
record Head where
 constructor MkHead

 headHeight     : Integer
 headTotalWork  : Integer
 headHash       : String
 headUnspent    : SortedSet Output

Eq Head where
 (MkHead a b c d) == (MkHead a' b' c' d') = and
  [ a == a'
  , b == b'
  , c == c'
  , Set.toList d == Set.toList d'
  ]

Ord Head where
 compare (MkHead a b c d) (MkHead a' b' c' d') = concat
  [ compare a a'
  , compare b b'
  , compare c c'
  , compare (Set.toList d) (Set.toList d')
  ]

Show Head where
 -- showPrec : Prec -> Head -> String
 showPrec prec (MkHead height totalWork headHash unspent) = 
  showCon prec "MkHead" $ concat
   [ showArg height
   , showArg totalWork
   , showArg headHash
   , showArg (Set.toList unspent)
   ] 

-- | BLOCK INFORMATION
record BlockInfo where
 constructor MkBlockInfo

 block     : Block
 height    : Integer
 totalWork : Integer
 unspent   : SortedSet Output
 succesors : SortedSet String

-- | BLOCKCHAIN
record Blockchain where
 constructor MkBlockchain

 currentHead : Head
 heads       : SortedSet Head
 blockchain  : SortedMap String BlockInfo

Show BlockInfo where
 -- showPrec : Prec -> BlockInfo -> String
 showPrec prec (MkBlockInfo block height totalWork unspent successors) =
  showCon prec "MkBlockInfo" $ concat
   [ showArg block
   , showArg height
   , showArg totalWork
   , showArg (Set.toList unspent)
   , showArg (Set.toList successors)
   ]

Show Blockchain where
 -- showPrec : Prec -> Blockchain -> String
 showPrec prec (MkBlockchain currentHead heads blockchain) =
  showCon prec "MkBlockchain" $ concat
   [ showArg currentHead
   , showArg (Set.toList heads)
   , showArg (Map.toList blockchain)
   ]

-- COMMAND
data Command : Type where
 Initialize : Block -> Command
 Submit     : Block -> Command
 QueryState : Command
 QueryHeads : Command

Show Command where
 -- shows
 showPrec prec (Initialize block) = showCon prec "Initialize" (showArg block)
 showPrec prec (Submit block)     = showCon prec "Submit"     (showArg block)
 showPrec prec QueryState         = "QueryState"
 showPrec prec QueryHeads         = "QueryHeads"

