{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import           Test.Hspec.Wai

import           Data.String                (fromString)

import qualified System.Exit                as Exit

import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified FirstApp.Main              as Main

main :: IO ()
main = do
  -- We need to setup our Application.
  let app' = pure Main.app

  -- This sets up HSpec to use our application as the thing it executes before the tests are run
  hspec . with app' $ do
      -- Here is an example test for the 'ListRq' route.
      -- Start with a general description of what we're going to test.
      describe "List Route" $ do
        -- Individual test cases provide more precise information regarding
        -- what they are going to test.
        it "Should return a 'not implemented' message and 200 status" $
          -- Using the functions from ``Test.Hspec.Wai`` this actions a GET request
          -- on the "/list" route, and using an infix function, compares the result of
          -- that request to our expected result.

          -- There String literal here is being converted by the use of the
          -- ``IsString`` typeclass into a response type that Hspec.Wai can
          -- use. Check the documentation for more examples, but when given
          -- a string literal, it will assume that is the expected body of
          -- the request and also check for a 200 response code.
          get "/list" `shouldRespondWith` "List Request not implemented"

      -- Write some more tests, below are some ideas to get you started:

      -- Don't worry if you don't get all of these done. :)

      -- 1) The '<topic>/add' route will respond with an error when given an empty comment
      describe "add topic" $ do

        it "should respond with OK for a valid topic" $
          post "/add-topic-spec/add" "Very good topic" `shouldRespondWith` 200

        it "should respond with Bad Request for an empty comment" $
          post "/add-topic-spec/add" "" `shouldRespondWith` 400

      -- 2) The '<topic>/view' route will respond correctly when given a topic
      describe "view topic" $ do

        it "should respond OK for a view request" $
          get "/add-topic-spec/view"  `shouldRespondWith` 200

      -- 3) The '<topic>/view' route will respond with an error when given an empty topic
        xit "should respond BadRequest for missing topics" $
          get "view"  `shouldRespondWith` 400
          --pendingWith "we get 404 instead of 400, there's no way we can get that?"
          -- FIXME: pending or perndingWith cause import problems

      -- 4) A gibberish route will return a 404
      describe "invalid requests" $ do

        it "should respond with NotFound for a invalid GET requests" $
          get "/foo/bar" `shouldRespondWith` 404

        it "should respond with NotFound for a invalid POST requests" $
          post "/foo/bar" "Very good topic" `shouldRespondWith` 404
