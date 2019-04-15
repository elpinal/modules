{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec

import Text.RawString.QQ

import Control.Exception.Safe
import qualified Data.Text as T

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Parser
import Language.Modules.Ros2018.Internal
import Language.Modules.Ros2018.Display

main :: IO ()
main = hspec spec

mustBeRight :: (HasCallStack, Display e, MonadThrow m) => Either e a -> m a
mustBeRight (Right x) = return x
mustBeRight (Left e)  = throwString $ "unexpected left value: " ++ display e

instance Display Failure where
  display (Failure e _ f) = f e

shouldBeRight :: (HasCallStack, Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldBeRight (Left e) _         = expectationFailure $ show e
shouldBeRight (Right x) expected = x `shouldBe` expected

integration :: MonadThrow m => T.Text -> m ()
integration txt = do
  e <- mustBeRight $ parseText "<filename>" txt
  (t, aty, _) <- mustBeRight (translate e) >>= mustBeRight
  ty <- mustBeRight $ typecheck t
  mustBeRight $ equalType (toType aty) ty

spec ::  Spec
spec = do
    describe "integration" $
      it "converts text to well-typed F_omega term" $ do
        integration "a = 1"                                                                  `shouldBeRight` ()
        integration "a = 1;"                                                                 `shouldBeRight` ()
        integration "a = 1; b = a"                                                           `shouldBeRight` ()
        integration "a = 1; b = a; b = 'w'"                                                  `shouldBeRight` ()
        integration "M = struct end; include M"                                              `shouldBeRight` ()
        integration "x = true; y = false; M1 = struct x = x; y = x; z = 'c' end; include M1" `shouldBeRight` ()
        integration "t = type int"                                                           `shouldBeRight` ()
        integration "t = type char"                                                          `shouldBeRight` ()
        integration "t = type bool"                                                          `shouldBeRight` ()
        integration "t = type type"                                                          `shouldBeRight` ()
        integration "t = type type; u = t; t = type int"                                     `shouldBeRight` ()

        integration "n = 97; s = n :> int"                  `shouldBeRight` ()
        integration "t = type (int ~> char); u = t :> type" `shouldBeRight` ()
        integration "f = fun (t : type) => t"               `shouldBeRight` ()
        integration "f = fun (t : type) => t :> type;"      `shouldBeRight` ()
        integration "f = fun (n : int) => n"                `shouldBeRight` ()
        integration "f = fun (n : int) => n :> int"         `shouldBeRight` ()

        integration "f = fun (n : int) => fun (s : type) => fun (ch : char) => n" `shouldBeRight` ()

        integration "1"        `shouldThrow` anyException
        integration "x = t"    `shouldThrow` anyException
        integration "x = 1;;"  `shouldThrow` anyException
        integration "i = int"  `shouldThrow` anyException
        integration "g = type" `shouldThrow` anyException

        integration [r|
          a = 1;
          b = a;
          C = true;
          D = 'v';
          M1 = struct
            x = 3;
            y = false;
            z = D;
            a = 3
          end;
          b = 'w';
          include M1;
          q9 = 2;
          |] `shouldBeRight` ()
