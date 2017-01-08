{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Mailer where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mail.Hailgun

import Secrets
import Types

downloadLinkPrefix :: T.Text
downloadLinkPrefix = "http://snokums.com/download?fid="

myContext :: HailgunContext
myContext = HailgunContext  { hailgunDomain = "mg.snokums.com"
                            , hailgunApiKey = mailgunAPIkey --from Secrets.hs
                            , hailgunProxy = Nothing }
                            
sendUploadedFilesMessage :: [FID] -> T.Text -> IO (Either HailgunErrorResponse HailgunSendResponse)
sendUploadedFilesMessage fids email = case message of
    Left _ -> return (Left $ HailgunErrorResponse "Message could not be constructed, probably an invalid email adress was supplied.")
    Right m -> sendEmail myContext m
    where message = hailgunMessage subject content sender recipients attachments
          subject = "Your uploaded files"
          sender = "noreply@mg.snokums.com"
          recipients = (emptyMessageRecipients { recipientsTo = [TE.encodeUtf8 email] })
          attachments = []
          content = makeUploadContent fids
          
makeUploadContent :: [T.Text] -> MessageContent
makeUploadContent fids = TextOnly . TE.encodeUtf8 $
    "Dear user,\n\n Your files have been uploaded succesfully. You can use the links below to download them again.\n\n" <>
    (T.intercalate "\n" $ map (downloadLinkPrefix <>) fids) <>
    "\n\nBe aware that we will delete the file after it has been downloaded five times or after a week, whichever happens first.\n\nKind regards,\nWander from snokums.com"
    