module DataBounded where

import Prelude

test3 :: Boolean
test3 = top || bottom

-- top should be inlined as true
test1 :: Boolean
test1 = top

-- bottom should be inlined as false
test2 :: Boolean
test2 = bottom