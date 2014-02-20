{-# LANGUAGE ForeignFunctionInterface #-}
-- | This example implements a really simple JQuery wrapper, to demonstrate
--   how foreign imports work.
--
--   For more information, see doc/js-externals.txt.
module Main where
import Haste
import Control.Monad (when)

newtype JQuery = JQuery JSAny

foreign import ccall js_jquery :: JSString -> IO (JQuery)
foreign import ccall js_click :: JQuery -> JSFun (Int -> IO ()) -> IO ()
foreign import ccall js_hide :: JQuery -> IO ()

-- | Since we can't name it '$', let's just call it 'j'.
j :: String -> (JQuery -> IO ()) -> IO ()
j s action = js_jquery (toJSString s) >>= action

-- | Register an onclick callback.
click :: (Int -> IO ()) -> JQuery -> IO ()
click f jq = js_click jq (mkCallback f)

-- | Hide an element.
hide :: JQuery -> IO ()
hide jq = js_hide jq

main = do
  j "#closeBlack" $ click (\button -> when (button == 0) (j "#blackBox" $ hide))
  j "#closeRed" $ click (\button -> when (button == 0) (j "#redBox" $ hide))



{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (head, id, div)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)

import Text.Blaze.Renderer.Utf8 (renderMarkup)

page1 :: Markup
page1 = html $ do
   head $ do
       title "Introduction page."
       link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
   body $ do
       div ! id "header" $ "Syntax"
       p "This is an example of BlazeMarkup syntax."
       ul $ mapM_ (li . toMarkup . show) [1, 2, 3]

renderMarkup page1

