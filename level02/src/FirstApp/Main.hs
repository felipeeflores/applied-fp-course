{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404, status500)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType (..), Error (..), RqType (..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType body =
  responseLBS status [("Content-Type", renderContentType contentType)] body

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 contentType body = mkResponse status200 contentType body

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 contentType errorMessage = mkResponse status200 contentType errorMessage

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 contentType errorMessage = mkResponse status400 contentType errorMessage

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest rawTopic rawComment = do
  topic <- mkTopic rawTopic
  commentText <- mkCommentText $ decodeUtf8 $ LBS.toStrict rawComment
  return $ AddRq topic commentText

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest rawText = (mkTopic rawText) >>= \topic -> Right(ViewRq topic)

mkListRequest
  :: Either Error RqType
mkListRequest = Right(ListRq)

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse TopicNotFound = resp404 PlainText "TopicNotFound"
mkErrorResponse InvalidTopic = resp400 PlainText "InvalidTopic"
mkErrorResponse InvalidCommentText = resp400 PlainText "InvalidCommentText"
mkErrorResponse NotFound = resp400 PlainText "NotFound"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r =
  case (requestMethod r, pathInfo r) of
    ("GET", [topic, "view"]) -> pure $ mkViewRequest topic
    ("GET", ["list"]) -> pure $ mkListRequest
    ("POST", [topic, "add"]) ->
      fmap (\body -> mkAddRequest topic body) (strictRequestBody r)
    _ -> pure $ Left NotFound

  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.


-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest r =
  case r of --TODO: Print out topic and doc
    ListRq      ->  Right $ resp200 PlainText "PlainText ListRq not implemented yet"
    AddRq topic _   ->  Right $ resp200 PlainText "PlainText AddRq not implemented yet"
    ViewRq _    ->  Right $ resp200 PlainText "PlainText ViewRq not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app req cb = do
  requestOrError <- mkRequest req
  let errorOrResult = requestOrError >>= handleRequest
  case errorOrResult of
    Right response -> cb $ response
    Left err -> cb $ (mkErrorResponse err)

runApp :: IO ()
runApp = run 3000 app
