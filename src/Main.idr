-- The longest chain should be chosen by total work, calculated as the sum of 16ˆd
-- for d as the difficulty of each block. 

import Control.Monad.State
import Control.Monad.Trans

import Language.JSON
import Data.SortedMap
import Data.SortedMap as Map
import Data.SortedSet
import Data.SortedSet as Set

import Blockchain.Util
import Blockchain
import Blockchain.Types
import Crypto.SHA

Stack : Type -> Type
Stack = StateT Blockchain IO

-- ||| If the block contains an invalid transaction (mismatched
-- inputs/outputs sum, nonexistent inputs, duplicate outputs), an
-- error should be returned: {"error":"invalid transaction"}


||| Makes sure that all transactions are balanced such that the total
||| input amount equals the total output.
invalidTransaction : (newBlock : Block) -> Stack () -> Stack ()
invalidTransaction newBlock loop = do
 let transacts = transactions newBlock

 let ok = all balance'd transacts

 when (not ok) $ do
  lift (putStrLn "{\"error\":\"invalid transaction\"}")
  loop

||| Makes sure that a block has a predecessor with a difficulty lesser
||| or equal to it.
findPredecessor : Block -> Stack () -> Stack ()
findPredecessor newBlock loop = do
 blockchain <- gets blockchain
 case Map.lookup (predecessorHash newBlock) blockchain of
  Nothing => do
    lift (putStrLn "{\"error\":\"no predecessor found\"}")
    loop
  Just (MkBlockInfo predecessor _ _ _ _) => do
    when (difficulty newBlock < difficulty predecessor) $ do
      lift (putStrLn "{\"error\":\"difficulty must not decrease\"}")
      loop

    pure ()
 
||| Displays error message if blockchain has not been initialized.
initialized : Stack () -> Stack ()
initialized loop = do
 blockchain <- gets blockchain

 when (null blockchain) $ do
   lift (putStrLn "{\"error\":\"must initialize first\"}")
   loop

||| Displays error message if hash has been submitted before.
duplicateHash : Block -> Stack () -> Stack ()
duplicateHash block loop = do
 blockchain <- gets blockchain
 case Map.lookup (blockHash block) blockchain of
  Nothing => do
    pure ()
  Just _ => do
    lift (putStrLn "{\"error\":\"duplicate hash\"}")
    loop

emptyBlockchain : Blockchain
emptyBlockchain = MkBlockchain (MkHead 1 1 "" Set.empty) Set.empty Map.empty


initialize : Block -> Stack () -> Stack ()
initialize newBlock go = do
 duplicateHash newBlock go
 modify $ \(MkBlockchain _ heads blockchain) => let

   initUnspent : SortedSet Output = Set.fromList (concatMap outputs (transactions newBlock))

   head   : Head           = MkHead 1 1 (blockHash newBlock) initUnspent
   heads' : SortedSet Head = insert head heads

   blockInfo : BlockInfo = MkBlockInfo newBlock 1 1 initUnspent Set.empty
 
   blockchain' : SortedMap String BlockInfo = 
     Map.insert (blockHash newBlock) blockInfo blockchain

   in MkBlockchain head heads' blockchain'
      
 lift (putStrLn "{\"ok\":[]}")


main : IO ()
main = [ () | _ <- runStateT go emptyBlockchain ] where

 go : StateT Blockchain IO ()
 go = do
  lift (putStr ">> ")

  let Just json = parse !(lift getLine)
   | Nothing => do 
    lift (putStrLn "+")
    go

  case parseCommand json of
    Right (Initialize newBlock) => initialize newBlock

    -- The submit command provides a new block to be added by the
    -- client, if valid.
    Right (Submit newBlock) => do

      -- state <- get
      -- lift $ do
      --   putStrLn "\n"
      --   putStr "pre-STATE: "
      --   print  state
      --   putStrLn "\n"
      
      invalidTransaction newBlock 
        go

      -- If a genesis block has not yet been initialized, an error
      -- should be returned:
      initialized 
        go

      duplicateHash newBlock 
        go

      findPredecessor newBlock 
        go

     --  -- let Right () = !(invalidTransaction newBlock) 
     --  --  | Left error => do lift (putStrLn error); go

      MkBlockchain head heads blockchain <- get

      let Just (MkBlockInfo predBlock predHeight predTotalWork predUnspent predChildren) 
        = Map.lookup (predecessorHash newBlock) blockchain
        | Nothing => go

      let updatePredBlock = Map.insert (predecessorHash newBlock) (MkBlockInfo predBlock predHeight predTotalWork predUnspent (Set.insert (blockHash newBlock) predChildren)) blockchain

      let newTotalWork = predTotalWork + cast (power 16 (cast (difficulty newBlock)))

      let newHeight = 1 + predHeight

      -- lift (putStrLn "\n\n")
      -- lift (do putStrLn "INPUT: "; print $ concatMap inputs (transactions newBlock))
      -- lift (putStrLn "\n\n")
      -- lift (do putStrLn "UNSPE: "; print $ predUnspent)
      -- lift (putStrLn "\n\n")

      let newUnspent : SortedSet Output = Set.fromList (concatMap outputs (transactions newBlock))
       -- newInputs : List Input = concatMap inputs (transactions newBlock)

       -- newOutputs : List Output = inputToOutput <$> newInputs
       -- in Set.toList (difference (Set.fromList predUnspent) (Set.fromList newOutputs))

      let updateNewBlock = Map.insert (blockHash newBlock) (MkBlockInfo newBlock newHeight newTotalWork newUnspent Set.empty) updatePredBlock

      -- TODO match output of previous
      let newHead = if newTotalWork > headTotalWork head
        then MkHead newHeight newTotalWork (blockHash newBlock) newUnspent
        else head

      let predecessorHead : Head = MkHead
        predHeight
        predTotalWork 
        (blockHash predBlock)
        predUnspent

      let newHeads : SortedSet Head = insert newHead (delete predecessorHead heads)

      put (MkBlockchain newHead newHeads updateNewBlock)

      lift (putStrLn "{\"ok\":[]}")


    Right QueryState => do
      -- {"state":{"height":1,"totalWork":1,
      -- "hash":"0xdcb3d5ee85f43e20e5844b787738941cc780eaac8200cb6734ca13cb4f8d1f85",
      -- "outputs":[{"id":73,"amount":30}]}}
      -- initialized 
      --   go
    
      MkBlockchain (MkHead height totalWork hash unspent) _heads _blockchain <- get

      let outputs : List JSON = map outputToJSON (Set.toList unspent)

      let response = JObject 
       [ "state" .- 
        [ "height"    .- height
        , "totalWork" .- totalWork
        , "hash"      .- hash
        , "outputs"   .- outputs
        ]
       ]
      
      lift (putStrLn (noSpaceJSON JNull))

    Right QueryHeads => do
      initialized 
        go

      MkBlockchain _ heads _ <- get

      let response = JObject ["heads" .- map headToJSON (Set.toList heads) ]

      lift (putStrLn (noSpaceJSON response))

    Left HashDoesNotMatch => do 
      lift (putStrLn "{\"error\":\"invalid hash\"}")
    Left IncorrectDifficulty => do 
      lift (putStrLn "{\"error\":\"leading zeroes in block hash did not match difficulty\"}")
    Left InvalidHash => do 
      lift (putStrLn "{\"error\":\"invalid hash\"}")
    Left NoParse => do 
      lift (putStrLn "{\"error\":\"no parse\"}")

  -- MkBlockchain _ heads _ <- get
  -- lift $ do
  --       putStrLn "\n"
  --       putStrLn "HEADS2: "
  --       for_ (Set.toList heads) print
  --       putStrLn "\n"
  go


