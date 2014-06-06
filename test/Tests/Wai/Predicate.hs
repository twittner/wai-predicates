{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Wai.Predicate (tests) where

import Data.ByteString (ByteString)
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Wai.Util

tests :: TestTree
tests = testGroup "Wai.Predicate"
    [ testCase "Accept application/json" testAcceptJson
    , testCase "Accept application/thrift " testAcceptThrift
    , testCase "Accept application/*" testAcceptAll
    , testCase "Content-Type text/plain" testContentTypePlain
    , testCase "Content-Type text/*" testContentTypeAll
    , testCase "Query" testQuery
    , testCase "QueryOpt" testQueryOpt
    , testCase "exec" testExec
    ]

testAcceptJson :: IO ()
testAcceptJson = do
    let rq0 = fromRequest . json $ defRequest "/"
    Okay 0 (Media "application" "json" 1.0 []) @=? (accept "application" "json") rq0

    let rq1 = fromRequest . withHeader "Accept" "foo/bar" $ defRequest "/"
    Fail (e406 & setMessage "Expected 'Accept: application/json'.")
        @=? (accept "application" "json") rq1

testAcceptThrift :: IO ()
testAcceptThrift = do
    let rq0 = fromRequest . withHeader "Accept" "application/x-thrift" $ defRequest "/"
    Okay 0 (Media "application" "x-thrift" 1.0 []) @=? (accept "application" "x-thrift") rq0

    let rq1 = fromRequest . json $ defRequest "/"
    Fail (e406 & setMessage "Expected 'Accept: application/x-thrift'.")
        @=? (accept "application" "x-thrift") rq1

testAcceptAll :: IO ()
testAcceptAll = do
    let rq0 = fromRequest . withHeader "Accept" "application/*" $ defRequest "/"
    Okay 0 (Media "application" "*"    1.0 []) @=? (accept "application" "*") rq0
    Okay 0 (Media "application" "json" 1.0 []) @=? (accept "application" "json") rq0

testContentTypePlain :: IO ()
testContentTypePlain = do
    let rq0 = fromRequest . withHeader "Content-Type" "text/plain" $ defRequest "/"
    Okay 0 (Media "text" "plain" 1.0 []) @=? (contentType "text" "plain") rq0

    let rq1 = fromRequest . withHeader "Content-Type" "text/html" $ defRequest "/"
    Fail (e415 & setMessage "Expected 'Content-Type: text/plain'.")
        @=? (contentType "text" "plain") rq1

testContentTypeAll :: IO ()
testContentTypeAll = do
    let rq0 = fromRequest . withHeader "Content-Type" "text/plain" $ defRequest "/"
    Okay 0.5 (Media "text" "plain" 0.5 []) @=? (contentType "text" "*") rq0

testQuery :: IO ()
testQuery = do
    let rq0 = fromRequest . withQuery "x" "y" . withQuery "x" "z" $ defRequest "/"
    Okay 0 ("y" :: ByteString) @=? (query "x") rq0

    let rq1 = fromRequest $ defRequest "/"
    Fail (e400 & setReason NotAvailable . setSource "x" . addLabel "query")
        @=? (query "x" :: Predicate Req Error ByteString) rq1

testQueryOpt :: IO ()
testQueryOpt = do
    let rq0 = fromRequest . withQuery "x" "y" . withQuery "x" "z" $ defRequest "/"
    Okay 0 (Just ("y" :: ByteString)) @=? (opt (query "x")) rq0

    let rq1 = fromRequest $ defRequest "/"
    Okay 0 Nothing @=? (opt (query "x" :: Predicate Req Error ByteString)) rq1

testExec :: IO ()
testExec = do
    let unit = const $ return ()
    let rq = fromRequest . withQuery "x" "42" . withHeader "a" "b" $ defRequest "/"
    exec (query "x" .&. header "a") rq unit $ \args -> do
        args#_1  @=? (42  :: Int)
        args#_2' @=? ("b" :: ByteString)

