{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Package.Impl
  ( runPM
  , H(..)
  , buildMain
  , PrettyError(..)
  ) where

import Control.Comonad
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Polysemy.Trace
import System.Directory
import System.FilePath
import System.IO.Error

import Language.Modules.Ros2018 as R
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Impl (Y(..), runMN)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Generated)
import qualified Language.Modules.Ros2018.Internal.Erased as E
import qualified Language.Modules.Ros2018.Internal.Erased.Dynamic as D
import qualified Language.Modules.Ros2018.Internal.Impl as II
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Package
import Language.Modules.Ros2018.Package.Config
import Language.Modules.Ros2018.Package.UnitParser
import Language.Modules.Ros2018.Parser (whileParser)
import Language.Modules.Ros2018.Position

data Evidence e where
  EvidPM :: Evidence PMError
  EvidConfig :: Evidence ConfigError
  EvidConfigParse :: Evidence ConfigParseError
  EvidUnitParse :: Evidence UnitParseError
  EvidElaborate :: Evidence ElaborateError

data PrettyError = forall e. Display e => PrettyError (Evidence e) e

instance Display PrettyError where
  display (PrettyError _ e) = display e

class Display a => Evidential a where
  evidence :: Evidence a

throwP :: (Evidential e, Member (Error PrettyError) r) => e -> Sem r a
throwP = throw . PrettyError evidence

data PMError
  = NoLib IOError
  | NoConfigFile IOError
  | DuplicateModule (Positional Ident) RootRelativePath
  | NoSuchModule (Positional Ident) RootRelativePath
  | ListDir FilePath IOError
  | UnboundUsePath UsePath S
  deriving (Eq, Show)

instance Display PMError where
  display (NoLib e)                = "no lib.1ml: " ++ show e
  display (NoConfigFile e)         = "no config file: " ++ show e
  display (DuplicateModule id dir) = display (getPosition id) ++ ": " ++ show dir ++ ": duplicate module: " ++ display id
  display (NoSuchModule id dir)    = display (getPosition id) ++ ": " ++ show dir ++ ": no such module: " ++ display id
  display (ListDir path e)         = "cannot obtain a list of entries in " ++ show path ++ ": " ++ show e
  display (UnboundUsePath up s)    = "unbound use path: " ++ show up ++ ": known mapping: " ++ show s

instance Evidential PMError where
  evidence = EvidPM

instance Evidential ConfigParseError where
  evidence = EvidConfigParse

instance Evidential UnitParseError where
  evidence = EvidUnitParse

instance Evidential ElaborateError where
  evidence = EvidElaborate

traverseDirS :: FileSystem m => (FilePath -> Bool) -> FilePath ->
               (FilePath -> T.Text -> m a) -> m (Maybe (Map.Map RootRelativePath a))
traverseDirS p path f = do
  m <- traverseDir p path f
  case m of
    Just m -> Just <$> sequence m
    Nothing -> return Nothing

getMName :: Unit -> Sem r Ident
getMName u = return $ extract $ mname u

refer :: UsePath -> I.Name
refer (Root pname)     = unIdent pname
refer (UsePath _ _ id) = unIdent id

toUsePath :: T.Text -> UsePath
toUsePath txt =
  let ts = T.split (== '/') txt in
  case ts of
    [t] -> Root $ ident t
    _   -> UsePath (ident $ head ts) (T.unpack $ f $ tail $ init ts) $ ident $ last ts
      where
        f :: [T.Text] -> T.Text
        f [] = "."
        f ws = foldr1 g ws

        g w s =
          let s' = if T.null s then "." else s in
          w <> ('/' `T.cons` s' :: T.Text)

newtype S = S (Map.Map UsePath (Generated, AbstractType))
  deriving newtype Show
  deriving Eq

type PMEffs =
 '[ Error I.Failure
  , Error PrettyError
  , Lift IO
  , Reader FilePath
  , State Int
  , State S
  , State [(Generated, I.Term)]
  , Trace
  ]

instance Members PMEffs r => PM (Sem r) where
    parse path = do
      root <- ask
      txt <- readFileT $ root </> path
      parseT path txt
    readConfig = do
      txt <- readFileT configFile
      cfg <- either throwP return $ parseConfig configFile txt
      let is = imports cfg
      m <- buildMap is
      return (m, g . extract <$> toList is)
        where
          g :: Import -> Ident
          g (Import id _) = extract id
    elaborate xs ts e = do
      let ups = map (toUsePath . extract) ts
      let lookupUsePath :: UsePath -> Sem r (Generated, AbstractType)
          lookupUsePath up = do
            S m <- get
            maybe (throwP $ UnboundUsePath up $ S m) return $ Map.lookup up m
      ps <- mapM lookupUsePath ups
      let w f (up, (g, aty)) env = let ?env = f env in
                                   let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty in
                                   I.insertTempValueWithName (refer up) g $ getBody aty
      let z f (id, g, aty) env = let ?env = f env in
                                 let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty in
                                 I.insertTempValueWithName (unIdent id) g $ getBody aty
      elab (foldl z (foldl w id $ zip ups ps) xs) e
    evaluate (U t) = do
      xs <- get @[(Generated, I.Term)]
      let t0 = U $ foldr (\(g, t1) t2 -> E.unpack (Just g) (E.erase t1) t2) t xs
      let _ = D.evaluate $ unU t0
      trace $ renderString $ layoutSmart defaultLayoutOptions $ unU t0 (0 :: Int)
    getMapping path = do
      root <- ask
      mm <- traverseDirS (".1ml" `isSuffixOf`) (root </> path) $ \fp content -> parseT fp content >>= getMName
      return $ fromMaybe mempty mm
    getFileName m dir id = do
      let m' = Map.filter (== extract id) m
      maybe (throwP $ NoSuchModule id dir) f $ Map.minViewWithKey m'
        where
          f :: forall z. ((RootRelativePath, z), FileModuleMap) -> Sem r RootRelativePath
          f ((path, _), rest) = if Map.null rest then return path else throwP $ DuplicateModule id dir
    register up aty = do
      g <- generateVar
      modify $ \(S m) -> S $ Map.insert up (g, aty) m
      return g
    combine pn _ lib main = do
      case lib of
        Nothing  -> return $ U $ E.erase main
        Just lib -> do
          let l = U $ E.erase lib
          let m = U $ E.erase main
          g <- gets $ \(S m) -> maybe (error "unexpected error: no lib") fst $ Map.lookup (Root pn) m
          return $ U $ E.unpack (Just g) (unU l) (unU m)
    emit g t = modify (++ [(g, t)])
    catchE m x = m `catch` f
      where
        f (PrettyError EvidPM (NoLib{}))        = return x -- The absence of lib.1ml is OK.
        f (PrettyError EvidPM (NoConfigFile{})) = return x -- The absence of 1ml.package is OK.
        f e                                     = throw e

instance Members '[State Int, Error I.Failure, Error PrettyError] r => Elab (Sem r) where
  elab f e = do
    n <- get
    let ?env = f I.builtins
    let ww = runMN n (Y II.runFailure) $ R.elaborate e
    (x, n') <- return ww >>= either (throw @I.Failure) return >>= either throwP return
    put n'
    return x

data ConfigError
  = ShadowedImport (Positional Ident)
  deriving (Eq, Show)

instance Display ConfigError where
  display (ShadowedImport id) = display (getPosition id) ++ ": shadowed import: " ++ display id

instance Evidential ConfigError where
  evidence = EvidConfig

buildMap :: Member (Error PrettyError) r => NDList (C Positional Import) -> Sem r ImportMap
buildMap is = foldlM f mempty is
  where
    f :: Member (Error PrettyError) r => ImportMap -> C Positional Import -> Sem r ImportMap
    f m i = do
      let (Import id s) = extract i
      if extract id `Map.member` m
        then throwP $ ShadowedImport id
        else return $ Map.insert (extract id) (toAbsolute $ extract s) m

-- TODO
toAbsolute :: T.Text -> AbsolutePath
toAbsolute = T.unpack

instance Member (Error PrettyError) r => Parser (Sem r) where
  parseT fp txt = either throwP return $ parseUnit whileParser fp txt

instance Members '[Error PrettyError, Lift IO] r => FileSystem (Sem r) where
  readFileT path = do
    x <- sendM $ tryIOError $ TIO.readFile path
    either g return x
      where
        g :: forall a. IOError -> Sem r a
        g e
          | isDoesNotExistError e && isLib e = throwP $ NoLib e
          | isDoesNotExistError e && isCF e  = throwP $ NoConfigFile e
          | otherwise                        = sendM $ ioError e
        isLib e = maybe False (== "lib.1ml") $ ioeGetFileName e
        isCF e = maybe False (== configFile) $ ioeGetFileName e
  traverseDir p path f = do
    z <- sendM $ tryIOError $ filter p <$> listDirectory path
    entries <- either g (return . Just) z
    case entries of
      Nothing -> return Nothing
      Just entries -> do
        xs <- sendM $ forM entries $ \e -> do
          let rpath = makeRelative path e
          (,) rpath . f rpath <$> TIO.readFile e
        return $ Just $ Map.fromList xs
      where
        g e
          | isDoesNotExistError e = return Nothing
          | otherwise             = throwP $ ListDir path e

instance Member (State Int) r => VariableGenerator (Sem r) where
  generateVar = do
    n <- get
    put $ n + 1
    return $ I.generated n

newtype H a = H { unH :: forall m. PM m => m a }

runPM :: FilePath -> H a -> IO (Either PrettyError (Either I.Failure a))
runPM fp m = runM $ runError $ runError $ fmap snd $ runState [] $ runTraceIO $
             fmap snd $ runState (S mempty) $ fmap snd $ runState (0 :: Int) $ runReader fp $ unH m
