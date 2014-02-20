module Haste.Markup.Html
    ( module Haste.Markup
    , Html
    , toHtml
    , preEscapedToHtml
    ) where

import Haste.Markup

type Html = Markup

toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup

preEscapedToHtml :: ToMarkup a => a -> Html
preEscapedToHtml = preEscapedToMarkup

