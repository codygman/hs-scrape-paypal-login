{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text              as T
import           Network.Scraper.State
import           Text.XML.Cursor

main = do
  runScraper $ do
    -- get the paypal login page
    get "https://www.paypal.com/login"

    -- Find a login_form with the Name "login_form" and post credentials to all visible (not input type="hidden") form inputs
    postToForm (Name "login_form") (Just creds) AllVisible

    -- this gets us an xml-conduit cursor for the current page (the response of our login attempt)
    c <- liftM (fromMaybe (error "Couldn't get cursor")) getCurrentCursor
    let title = getTitle c
    liftIO $ print $ title
      where creds =  [ ("login_email", "youremail@example.com") -- put your credentials here
                     , ("login_password", "password")]
            getTitle c = do
              let elems = c $// element "title"
                  elem = head elems -- Try not to use head in your actual code, and use headMay from the Safe package instead
                  title = head $ elem $// content
                in title
