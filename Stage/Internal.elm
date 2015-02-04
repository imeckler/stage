module Stage.Internal where

{-| Internal implementation of Stage if you need access to it.
    This is all subject to change. -}

import Time (Time)

type Duration = ForATime Time | Forever

type Stage t a = Stage Duration (Time -> a)

