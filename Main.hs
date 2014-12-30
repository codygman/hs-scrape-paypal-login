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

-- At the bottom of this file you'll find a repl session[0] to help understand the getPaypalBalance function.
-- Additionally there is a more verbose version of the getPaypalBalance function that makes the composition
-- and order of operations more explicit.
getPaypalBalance cursor = fromMaybe (error "Failed to get balance") $ listToMaybe $
                          cursor $//
                          -- Create 'Axis' that matches element named "div" who has an attribute
                          -- named "class" and attribute value named "balanceNumeral"
                          -- This axis will apply to the descendants of cursor.
                          element "div" >=> attributeIs "class" "balanceNumeral" &/
                          -- The Axis following &/ below matches the results of the previous Axis.
                          -- In other words, the following Axis will match all descendants inside of
                          -- <div class="balanceNumeral"></div>
                          element "span" >=> attributeIs "class" "h2" &/
                          -- The content Axis is applied to the results of the previous Axis.
                          -- In other words, it gets the <span class="h2">content</span> out.
                          content

main = do
  runScraper $ do
    get "https://www.paypal.com/login"

    -- Find a login_form with the Name "login_form" and post credentials to all
    -- visible (not input type="hidden") form inputs
    postToForm (Name "login_form") (Just creds) AllVisible

    get "https://www.paypal.com/myaccount/home"

    cursor <- liftM (fromMaybe (error "Couldn't get cursor")) getCurrentCursor
    liftIO . putStrLn $ "Your Paypal balance is: " <> getPaypalBalance cursor

  where creds =  [ ("login_email", "email@example.com") -- put your credentials here
                 , ("login_password", "password")]


-- 0: Further explaining the getPaypalBalance function

-- λ> prettyPrintHtml $ cursor $// element "div" >=> attributeIs "class" "balanceNumeral"
-- <div class="balanceNumeral"><span class="h2">$0.00</span><span class="small-text numeralLabel">Available</span></div>
-- λ> prettyPrintHtml $ cursor $// element "div" >=> attributeIs "class" "balanceNumeral"
--   &/ element "span"
-- <span class="h2">$0.00</span><span class="small-text numeralLabel">Available</span>
-- λ> prettyPrintHtml $ cursor $// element "div" >=> attributeIs "class" "balanceNumeral" &/
--    element "span" >=> attributeIs "class" "h2"
-- <span class="h2">$0.00</span>
-- λ> cursor $// element "div" >=> attributeIs "class" "balanceNumeral" &/
--    element "span" >=> attributeIs "class" "h2" &/
--    content
-- ["$0.00"]
-- λ> listToMaybe $ cursor $// element "div" >=> attributeIs "class" "balanceNumeral" &/
--    element "span" >=> attributeIs "class" "h2"
--    &/ content
-- Just "$0.00"
-- λ> fromMaybe (error "Failed to get balance") $
--    listToMaybe $ cursor $// element "div" >=> attributeIs "class" "balanceNumeral" &/
--    element "span" >=> attributeIs "class" "h2"
--    &/ content
-- "$0.00"

-- 1: A more verbose getPaypalBalance function.
getPaypalBalance' cursor = do
  let matchDiv = element "div" >=> attributeIs "class" "balanceNumeral" -- <div class="balanceNumeral">
      matchSpan = element "span" >=> attributeIs "class" "h2" -- <div class="h2">
      matchBalance = matchDiv &/ matchSpan &/ content -- compose together our matchers/selectors
      maybeBalance = listToMaybe $ cursor $// matchBalance -- returns Just "$0.00" or Nothing
      balance = fromMaybe (error "Failed to get balance") maybeBalance
  balance -- returns balance
