{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.IO           (putStrLn)
import           Network.Scraper.State
import           Prelude                hiding (putStrLn)
import           Text.XML.Cursor        (attributeIs, content, element, ($//),
                                         (&/))

getPaypalBalance cursor = fromMaybe (error "Failed to get balance") $ listToMaybe $
                          cursor $//
                          element "div" >=> attributeIs "class" "balanceNumeral" &/
                          element "span" >=> attributeIs "class" "h2" &/
                          content

main = do
  runScraper $ do
    get "https://www.paypal.com/login"

    postToForm (Name "login_form") (Just creds) AllVisible

    get "https://www.paypal.com/myaccount/home"

    cursor <- liftM (fromMaybe (error "Couldn't get cursor")) getCurrentCursor
    liftIO . putStrLn $ "Your Paypal balance is: " <> getPaypalBalance cursor

  where creds =  [ ("login_email", "email@example.com") -- put your credentials here
                 , ("login_password", "password")]
