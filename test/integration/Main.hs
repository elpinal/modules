{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec

import Text.RawString.QQ

import Control.Exception.Safe
import qualified Data.Text as T

import Language.Modules.Ros2018 hiding (Env)
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Impl
import Language.Modules.Ros2018.Internal
import Language.Modules.Ros2018.Internal.Impl
import Language.Modules.Ros2018.Parser

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
  (t, aty, _) <- mustBeRight (translate (Y runFailure) e) >>= mustBeRight
  ty <- mustBeRight $ typecheck (toType <$> (builtins :: Env f SemanticType)) t
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
        integration "id = fun (t : type) => fun (x : t) => x"                     `shouldBeRight` ()

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

        integration [r|
          id = fun (a : type) => fun (v : a) => v;
          i = type int;
          id_int = id i;
          m = 128;
          n = id_int m;
          |] `shouldBeRight` ()

        integration [r|
          i = type (= type int);
          f = fun (t : i) => t;
          x = type int;
          n = f x;
          |] `shouldBeRight` ()

        integration [r|
          t = type (X : type) ~> (= type X);
          b = type bool;
          z = fun (v : t) => v;
          f = fun (t : type) => type t;
          y = z f;
          |] `shouldBeRight` ()

        integration [r|
          x = true;
          M = struct
            x = 40;
            y = 30;
          end;
          x = M.x;
          f = fun (y : type) => M.y;
          |] `shouldBeRight` ()

        integration [r|
          x = true;
          y =
            if x
              then 'w'
              else 'h'
            end : char
          |] `shouldBeRight` ()

        integration [r|
          x = true;
          y =
            if x
              then type int
              else type bool
            end : type
          |] `shouldBeRight` ()

        integration [r|
          S = type sig
            x : int;
            t : type;
            i : (= type int);
          end;

          T = type sig
            x : int;
            y : char;
          end;
          |] `shouldBeRight` ()

        integration [r|
          S = type sig
            t : type ~> bool;
          end;

          M = struct
            t = fun (X : type) => true;
            k = 4;
          end;

          M = M :> S;
          |] `shouldBeRight` ()

        integration [r|
          S = type sig
            e : int;
            t : type -> type;
          end;

          M = struct
            t = fun (X : type) => X;
            e = 0;
          end;

          M = M :> S;
          |] `shouldBeRight` ()

        integration [r|
          S = type sig
            t : type;
            f : int -> t;
            g : t -> int;
          end;

          M = struct
            t = type int;
            f = fun (n : int) => n;
            g = fun (n : t) => n;
          end;

          N = M :> S;

          T = type S where (t : (= type int));

          O = M :> T;
          |] `shouldBeRight` ()

        integration [r|
          x = 1;

          local
            M = let
              M = struct
                t = type int;
                v = x;
              end;
            in
              M :> sig
                t : type;
                v : t;
              end;
          in
            v = M.v;
            w = v;
          end;

          y = 'o';
          |] `shouldBeRight` ()

        integration [r|
          K = struct
            M = struct
              t = type int;
              u = type char
            end;

            open M;

            v = t;
            w = u;
          end;

          z = K.v
          |] `shouldBeRight` ()

        integration [r|
          f = fun (n : int) => struct
            b = true;
            n = 90;
          end;

          k = 4;

          x = if (f k).b then (f k).n else 2 end : int;
          |] `shouldBeRight` ()

        integration [r|
          f = fun (x : bool) => fun (y : int) => fun t => fun (v : t) => fun (w : t -> int -> t) => w v y;

          z = f false 128 (type char) 'd' (fun (ch : char) => fun (n : int) => 'r');
          |] `shouldBeRight` ()

        integration [r|
          type t = int;
          type t a = a -> a;
          type t a b c = a -> t b -> c;

          f = (fun (n : int) => struct
            type t = char
          end) :> int -> sig t : type end;

          type u (n : int) = (f n).t;
          |] `shouldBeRight` ()

        integration [r|
          S = type sig
            t : type -> type -> type;
          end;
          |] `shouldBeRight` ()

        integration [r|
          type S a = sig
            type t a b;
            type u a b c = t (t b c) (type char);
            type v;
            type w t (v : int) = t ~> u t a t;
          end;

          T = S (type int);

          f = fun (M : T) => fun (x : M.w (type bool) 800) => x true;
          |] `shouldBeRight` ()

        integration [r|
          type S = sig
            type s;
            type t;
          end;

          F = fun (x : type) => struct
            M = struct
              s = x;
              type t = char;
            end;
            N = M :> S;
          end;
          |] `shouldBeRight` ()

        integration [r|
          type S = sig
            type s;
            type t;
          end;

          F x : S = struct
            s = x;
            type t = char;
          end;
          |] `shouldBeRight` ()

        integration [r|
          type T = (a : type -> type) -> type -> type;
          t : T = fun (a : type -> type) b => a b;
          |] `shouldBeRight` ()
