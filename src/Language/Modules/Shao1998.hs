module Language.Modules.Shao1998
  (
  ) where

newtype StrIdent = StrIdent Int
  deriving (Eq, Show)

newtype FctIdent = FctIdent Int
  deriving (Eq, Show)

newtype TypeIdent = TypeIdent Int
  deriving (Eq, Show)

data StrPath = StrPath [StrIdent] StrIdent
  deriving (Eq, Show)

data FctPath = FctPath (Maybe StrIdent) FctIdent
  deriving (Eq, Show)

data TypePath = TypePath (Maybe StrIdent) TypeIdent
  deriving (Eq, Show)

data Kind
  = Mono
  | KFun Kind
  deriving (Eq, Show)

data TyCon
  = TyConPath TypePath
  | TyConInt
  | TyConFun TyCon TyCon
  | TyConAbs TypeIdent TyCon
  | TyConApp TyCon TyCon
  deriving (Eq, Show)

data Spec
  = AbsTypeDef TypeIdent Kind
  | ManTypeDef TypeIdent Kind TyCon
  | StrDef StrIdent Sig
  | FctDef FctIdent FSig
  deriving (Eq, Show)

newtype Sig = Sig [Spec]
  deriving (Eq, Show)

data FSig = FSig StrIdent Sig Sig
  deriving (Eq, Show)

data Decl
  = TypeDecl TypeIdent TyCon
  | StrDecl StrIdent Struct
  | FctDecl FctIdent Fct
  | Local [Decl] [Decl]
  deriving (Eq, Show)

data Struct
  = StrP StrPath
  | FctApp FctIdent StrIdent
  | Asc StrIdent Sig
  | DefStr DefStruct
  deriving (Eq, Show)

newtype DefStruct = DefStruct [Decl]
  deriving (Eq, Show)

data Fct
  = FctP FctPath
  | DefFct StrIdent Sig DefStruct
  deriving (Eq, Show)
