module Util

import Language.JSON

%access export


Semigroup Ordering where
 -- (<+>) : Ordering -> Ordering -> Ordering
 (<+>) LT _        = LT
 (<+>) GT _        = GT
 (<+>) EQ ordering = ordering

Monoid Ordering where
 -- neutral : Ordering
 neutral = EQ

--

note : a -> Maybe b -> Either a b
note a Nothing  = Left a
note _ (Just b) = Right b

areYouInt : Double -> Maybe Int
areYouInt d = do
 guard (floor d == d)
 pure (cast d)

--

infixr 5 .-

interface Guess a where
 guess : a -> JSON 

Guess JSON where
 guess = id

Guess Bool where
 guess = JBoolean

Guess String where
 guess = JString

Guess Double where
 guess = JNumber 

Guess Integer where
 guess = JNumber . cast

Guess (List JSON) where
 guess = JArray . map guess

Guess (List (String, JSON)) where
 guess = JObject

(.-) : Guess a => String -> a -> (String, JSON)
a .- b = (a, guess b)

-- Coming from Haskell
undefined : a
undefined = believe_me "UNDEFINED"

noSpaceJSON : JSON -> String
noSpaceJSON = pack . filter (not . isSpace) . unpack . format 0
