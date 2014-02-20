{-# LANGUAGE OverloadedStrings #-}
-- | This example implements a really simple JQuery wrapper, to demonstrate
--   how foreign imports work.
--
--   For more information, see doc/js-externals.txt.
module Main where
import Prelude hiding (head, id, div)

import Haste
import Haste.Markup.Html4.Strict hiding (map)
import Haste.Markup.Html4.Strict.Attributes hiding (title)

import Haste.Markup.Renderer.String (renderMarkup)

page1 :: Markup
page1 = html $ do
   head $ do
       title "Introduction page."
       link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
   body $ do
       div ! id "header" $ "Syntax"
       p "This is an example of BlazeMarkup syntax."
       ul $ mapM_ (li . toMarkup . show) [1, 2, 3]

main :: IO ()
main = writeLog $ renderMarkup page1


