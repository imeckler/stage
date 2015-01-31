module Stage.Infix
  ( (<>)
  , (+>)
  ) where
{-| Infix synonyms. 
# Operators
@docs (<>), (+>), (>+>)
-}

import Stage(..)

{-| (<>) = Stage.followedBy -}
(<>) : Stage ForATime a -> Stage t a -> Stage t a
(<>) = followedBy

infixl 9 <>

{-| (+>) = Stage.chainTo -}
(+>) : Stage ForATime a -> (a -> Stage t a) -> Stage t a
(+>) = chainTo

{-| (>+>) f g = \x -> f x +> g -}
(>+>) : (a -> Stage ForATime a) -> (a -> Stage t a) -> (a -> Stage t a)
(>+>) f g = \x -> f x +> g

infixl 9 +>
