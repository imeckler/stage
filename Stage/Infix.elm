module Stage.Infix
  ( (<>)
  , (+>)
  ) where
{-| Infix synonyms. 
# Operators
@docs (<>), (+>)
-}

import Stage(..)

{-| (<>) = Stage.followedBy -}
(<>) : Stage ForATime a -> Stage t a -> Stage t a
(<>) = followedBy

infixl 9 <>

{-| (+>) = Stage.chainTo -}
(+>) : Stage ForATime a -> (a -> Stage t a) -> Stage t a
(+>) = chainTo

infixl 9 +>
