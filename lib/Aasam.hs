module Aasam (m) where

import Grammars
import Data.Set

m :: ContextFree -> Precedence
m (s, prods) = empty
