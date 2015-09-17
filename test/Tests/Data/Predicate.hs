{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Tests.Data.Predicate (tests) where

import Control.Applicative hiding (Const, empty)
import Data.Predicate
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))
import Prelude

tests :: TestTree
tests = testGroup "Data.Predicate"
    [ testProperty ".&." testAnd
    , testProperty "|||" testOr
    , testProperty ".|." testOr'
    ]

testAnd :: Predicate () Int Char -> Predicate () Int Char -> Bool
testAnd a b = case (a (), b ()) of
    (Okay d x, Okay w y) -> (a .&. b) () == Okay (d + w) (x ::: y)
    (Okay _ _, Fail   y) -> (a .&. b) () == Fail y
    (Fail   x, Okay _ _) -> (a .&. b) () == Fail x
    (Fail   x, Fail   _) -> (a .&. b) () == Fail x

testOr :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr a b = case (a (), b ()) of
    (Okay d x, Okay e y) -> (a ||| b) () == if d <= e then Okay d (Left x) else Okay e (Right y)
    (Okay d x, Fail   _) -> (a ||| b) () == Okay d (Left x)
    (Fail   _, Okay d y) -> (a ||| b) () == Okay d (Right y)
    (Fail   _, Fail   y) -> (a ||| b) () == Fail y

testOr' :: Predicate () Int Char -> Predicate () Int Char -> Bool
testOr' a b = case (a (), b ()) of
    (Okay d x, Okay e y) -> (a .|. b) () == if d <= e then Okay d x else Okay e y
    (Okay d x, Fail   _) -> (a .|. b) () == Okay d x
    (Fail   _, Okay d y) -> (a .|. b) () == Okay d y
    (Fail   _, Fail   y) -> (a .|. b) () == Fail y

instance Arbitrary (Result Int Char) where
    arbitrary =
        oneof [ Okay <$> (arbitrary :: Gen Double) <*> (arbitrary :: Gen Char)
              , Fail <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary (Predicate () Int Char) where
    arbitrary = (\r -> const r) <$> (arbitrary :: Gen (Result Int Char))

instance Show (Predicate () Int Char) where
    show _ = "Predicate () Int Char"
