module Blockchain

import Data.SortedMap
import Data.SortedMap as Map
import Data.SortedSet
import Data.SortedSet as Set
import Language.JSON
import Language.JSON.Data
import Data.So
import Prelude.List

import Blockchain.Types
import Blockchain.Util
import Crypto.SHA

%access public export

-- inputToOutput : Input -> Output
-- inputToOutput (MkInput id amount) = MkOutput id amount 

outputToJSON : Output -> JSON
outputToJSON (MkOutput id amount) = JObject
 [ "id"     .- cast {to = Integer} id
 , "amount" .- cast {to = Integer} amount
 ] 

-- HASH 
--
-- (I had to try this, when in Rome)
-- 
-- data Hash : String -> Type where
--  MkHash : (hash : String) -> Hash (strCons '0' (strCons 'x' hash))
--
-- parseHash : (hash : String) -> Either Error (Hash hash)
-- parseHash hash with (strM hash) 
--  parseHash (strCons ch hash')
--   | StrCons ch hash' 
--   with (decEq ch '0')
--   parseHash (strCons '0' hash')
--    | _ | Yes Refl 
--    with (strM hash')
--    parseHash (strCons '0' (strCons ch' hash''))
--     | _ | _ | StrCons ch' hash'' 
--     with (decEq ch' 'x')
--     parseHash (strCons '0' (strCons 'x' hash''))
--      | _ | _ | _ | Yes Refl = Right (MkHash hash'')
--     parseHash (strCons '0' (strCons ch' hash''))
--      | _ | _ | _ | _ = Left InvalidHash
--    parseHash "0"
--     | _ | _ | _ = Left InvalidHash
--   parseHash (strCons ch hash')
--    | _ | _ = Left InvalidHash
--  parseHash ""
--   | _ = Left InvalidHash

balance'd : Transaction -> Bool
balance'd transactions = 
 sum (map inAmount (inputs transactions))
  ==
 sum (map outAmount (outputs transactions))
 
checkHashDifficulty : (d : Nat) -> (hash : String) -> Either Error (HashDifficulty d hash)
checkHashDifficulty (S n) hash with (strM hash)
 checkHashDifficulty (S n) (strCons ch hash') 
  | StrCons ch hash' with (decEq ch '0')
  checkHashDifficulty (S n) (strCons '0' hash') 
   | _ | Yes Refl = HashDifficultyS <$> checkHashDifficulty n hash'
  checkHashDifficulty (S n) (strCons ch hash') 
   | _ | No _ = Left IncorrectDifficulty
 checkHashDifficulty (S n) ""
  | _ = Left IncorrectDifficulty
checkHashDifficulty Z _ 
  = Right HashDifficultyZ

checkOxHashDifficulty : (d : Nat) -> (hash : String) -> Either Error (OxHashDifficulty d hash)
checkOxHashDifficulty d hash with (strM hash)
 checkOxHashDifficulty d (strCons ch hash')
  | StrCons ch hash' with (strM hash')
  checkOxHashDifficulty d (strCons ch (strCons ch' hash''))
   | _ | StrCons ch' hash'' with (decEq ch '0')
   checkOxHashDifficulty d (strCons '0' (strCons ch' hash''))
    | _ | _ | Yes Refl with (decEq ch' 'x')
    checkOxHashDifficulty d (strCons '0' (strCons 'x' hash''))
     | _ | _ | _ | Yes Refl = MkOxHashDifficulty <$> checkHashDifficulty d hash''
    checkOxHashDifficulty d (strCons '0' (strCons ch' hash''))
     | _ | _ | _ | _ = Left IncorrectDifficulty
   checkOxHashDifficulty d (strCons ch (strCons ch' hash''))
    | _ | _ | _ = Left IncorrectDifficulty
  checkOxHashDifficulty d (strCons ch "")
   | _ | _ = Left IncorrectDifficulty
 checkOxHashDifficulty d ""
  | _ = Left IncorrectDifficulty

headToJSON : Head -> JSON
headToJSON (MkHead height totalWork hash unspent) = JObject 
 [ "height"    .- height
 , "totalWork" .- totalWork
 , "hash"      .- hash
 ] 


calculateBlockHash : JSON -> Either Error String
calculateBlockHash json = do
 JObject obj <- [| json |] 
   | _ => Left NoParse

 predecessor  <- note NoParse (lookup "predecessor"  obj)
 transactions <- note NoParse (lookup "transactions" obj)
 difficulty   <- note NoParse (lookup "difficulty"   obj)
 nonce        <- note NoParse (lookup "nonce"        obj)

 let bundleJSON : JSON = JArray
  [ predecessor
  , transactions
  , difficulty
  , nonce
  ]

 -- This is how we bundle the data together, 
 --
 --   def hash(block):
 --    bundle  = (block['predecessor'], block['transactions'], block['difficulty'], block['nonce'])
 --    encoded = json.dumps(bundle, sort_keys=True, separators=(',',':'))
 --    return '0x' + base64.b16encode(hashlib.sha256(encoded).digest()).lower()
 let bundle : String = noSpaceJSON bundleJSON

 pure ("0x" ++ sha256 bundle)

parseArray : String -> List (String, JSON) -> Either Error (List JSON)
parseArray key obj = do
 JArray array <- note NoParse (lookup key obj) 
  | _ => Left NoParse
 pure array

parseInt : String -> List (String, JSON) -> Either Error Int
parseInt key obj = do
 JNumber double <- note NoParse (lookup key obj) 
  | _ => Left NoParse
 note NoParse (areYouInt double)

parseString : String -> List (String, JSON) -> Either Error String
parseString key obj = do
 JString hash <- note NoParse (lookup key obj) 
  | _ => Left NoParse
 pure hash

parseInput : JSON -> Either Error Input
parseInput json = do
 JObject obj <- [| json |] | _ => Left NoParse
 id          <- parseInt "id"     obj
 amount      <- parseInt "amount" obj
 pure (MkInput id amount)

parseOutput : JSON -> Either Error Output
parseOutput json = do
 JObject obj <- [| json |] | _ => Left NoParse
 id          <- parseInt "id"     obj
 amount      <- parseInt "amount" obj
 pure (MkOutput id amount)
 
parseTransaction : JSON -> Either Error Transaction
parseTransaction json = do
 JObject obj <- [| json |] | _ => Left NoParse

 inputs  <- parseArray "inputs" obj
 outputs <- parseArray "outputs" obj

 inputs  <- traverse parseInput  inputs
 outputs <- traverse parseOutput outputs

 -- TODO check Refl
 pure (MkTransaction inputs outputs)

parseBlock : JSON -> Either Error Block
parseBlock json = do
 JObject obj  <- [| json |] | _ => Left NoParse
 difficulty   <- parseInt    "difficulty"   obj
 hash         <- parseString "hash"         obj
 nonce        <- parseInt    "nonce"        obj
 predecessor  <- parseString "predecessor"  obj
 transactions <- parseArray  "transactions" obj
 transactions <- traverse parseTransaction transactions

 wantedHash <- calculateBlockHash json

 check'd <- checkOxHashDifficulty (cast difficulty) hash
 
 note HashDoesNotMatch $
   guard (hash == wantedHash)

 pure (MkBlock difficulty hash nonce predecessor transactions check'd)

parseCommand : JSON -> Either Error Command
parseCommand json = do
 JObject obj <- [| json |] 
  | _ => Left NoParse

 case obj of
  ("init", block) :: [] => do
    block <- parseBlock block
    Right (Initialize block)

  ("query", JString "state") :: [] => 
    Right QueryState

  ("query", JString "heads") :: [] => 
    Right QueryHeads

  ("block", block) :: [] => do
    block <- parseBlock block
    Right (Submit block)

  _ => Left NoParse
