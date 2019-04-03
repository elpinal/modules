{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.Display
  ( Display(..)
  , displays
  , DisplayName(..)
  , displayWithName
  , NameContext
  , nameContext
  , newType
  , newTypes
  , newValue
  , newValues
  , displayVariable
  , displayTypeVariable
  ) where

class Display a where
  {-# MINIMAL displaysPrec | display #-}
  display :: a -> String
  displaysPrec :: Int -> a -> ShowS

  display x = displaysPrec 0 x ""
  displaysPrec _ x s = display x ++ s

displays :: Display a => a -> ShowS
displays = displaysPrec 0

instance Display Char where
  display ch = [ch]

class Display a => DisplayName a where
  displaysWithName :: (?nctx :: NameContext) => Int -> a -> ShowS

displayWithName :: (DisplayName a, ?nctx :: NameContext) => a -> String
displayWithName x = displaysWithName 0 x ""

data NameContext = NameContext
  { tnameCtx :: [String]
  , vnameCtx :: [String]
  , count :: Int
  }

nameContext :: NameContext
nameContext = NameContext
  { tnameCtx = []
  , vnameCtx = []
  , count = 0
  }

newType :: (?nctx :: NameContext) => NameContext
newType = ?nctx
  { count = 1 + count ?nctx
  , tnameCtx = toName 't' (count ?nctx) : tnameCtx ?nctx
  }

newTypes :: (?nctx :: NameContext) => Int -> NameContext
newTypes 0 = ?nctx
newTypes n = let ?nctx = newType in newTypes $ n - 1

newValue :: (?nctx :: NameContext) => NameContext
newValue = ?nctx
  { count = 1 + count ?nctx
  , vnameCtx = toName 'v' (count ?nctx) : vnameCtx ?nctx
  }

newValues :: (?nctx :: NameContext) => Int -> NameContext
newValues 0 = ?nctx
newValues n = let ?nctx = newValue in newValues $ n - 1

toName :: Char -> Int -> String
toName ch n = ch: show n

displayTypeVariable :: (?nctx :: NameContext) => Int -> ShowS
displayTypeVariable n = case tnameCtx ?nctx of
  xs | 0 <= n && n < length xs -> showString $ xs !! n
  _                            -> showString "v[" . shows n . showString "]"

displayVariable :: (?nctx :: NameContext) => Int -> ShowS
displayVariable n = case vnameCtx ?nctx of
  xs | 0 <= n && n < length xs -> showString $ xs !! n
  _                            -> showString "v[" . shows n . showString "]"
