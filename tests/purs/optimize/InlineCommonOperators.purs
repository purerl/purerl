module InlineCommonOperators where

import Prelude
import Effect
import Effect.Class
import Erl.Data.List
import Erl.Data.List as List
import Erl.Atom
import Unsafe.Coerce
import Safe.Coerce
import Data.Int as Int
import Data.Newtype

inlineBinary = do
  let divInt = 1 / 2
      divNum = 1.0 / 2.0
      andBool = true && false
      orBool = true || false
      appendList (l1 :: List Int) l2 = l1 <> l2

      addInt = 1 + 2
      mulInt = 1 * 2
      subInt = 1 - 2
      addNum = 1.0 + 2.0
      mulNum = 1.0 * 2.0
      subNum = 1.0 - 2.0

      eqNum = 1.0 == 2.0
      notEqNum = 1.0 /= 2.0
      eqInt = 1 == 2
      notInt = 1 /= 2
      eqString = "aaa" == "bbb"
      notEqString = "aaa" /= "bbb"
      eqChar = 'a' == 'b'
      notEqChar = 'a' /= 'b'
      eqBoolean = false == false
      notEqBoolean = false == false

      ltInt = 1 < 2
      lteInt = 1 <= 2
      gtInt = 1 > 2
      gteInt = 1 >= 2
      minInt = min 1 2
      maxInt = max 1 2

  unit

inlineUnary = do
  let negNum = - 1.2
      negInt = - 1
      notBoolean = not false
  
  unit

inlineListCons x (xs :: List Int) = x : xs
inlineListSingleton = List.singleton 1
inlineAtom = atom "an_atom"
inlineVoid = void (pure 49 :: Effect _)

inlineUnsafeCoerce :: Int -> String
inlineUnsafeCoerce = unsafeCoerce 42

inlineIntToNumber = Int.toNumber 42

newtype N a = N a
derive instance Newtype (N a) _

inlineCoerce :: Int
inlineCoerce = coerce (N 42)

inlineUnwrap = unwrap (N 1) :: Int
inlineWrap = wrap 1 :: N Int

inlineOver :: N Int -> N Int
inlineOver = over N (_+1)

inlineOver2 :: N Int -> N Int -> N Int
inlineOver2 = over2 N (+)

-- discard Unit - see magicdo tests
-- onNFn - see fnN tests