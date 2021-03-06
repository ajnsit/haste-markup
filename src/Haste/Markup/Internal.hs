{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification,
             DeriveDataTypeable #-}
-- | The BlazeMarkup core, consisting of functions that offer the power to
-- generate custom markup elements. It also offers user-centric functions,
-- which are exposed through 'Text.Blaze'.
--
-- While this module is exported, usage of it is not recommended, unless you
-- know what you are doing. This module might undergo changes at any time.
--
module Haste.Markup.Internal
    (
      -- * Important types.
      ChoiceString (..)
    , StaticString (..)
    , MarkupM (..)
    , Markup
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating custom tags and attributes.
    , customParent
    , customLeaf
    , attribute
    , dataAttribute
    , customAttribute

      -- * Converting values to Markup.
    , text
    , preEscapedText
    , string
    , preEscapedString

      -- * Converting values to tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , stringValue
    , preEscapedStringValue

      -- * Setting attributes
    , Attributable
    , (!)
    , (!?)

      -- * Modifying Markup elements
    , contents
    , external
    ) where

import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Unsafe.Coerce (unsafeCoerce)

import Haste.Prim (JSString, toJSStr, fromJSStr)

import Data.Typeable (Typeable)
import GHC.Exts (IsString (..))

-- | A static string that supports efficient output to all possible backends.
--
data StaticString = StaticString
    { getString :: String -> String  -- ^ Appending haskell string
    , getText   :: JSString        -- ^ A Native JS String
    }

-- 'StaticString's should only be converted from string literals, as far as I
-- can see.
--
instance IsString StaticString where
    fromString s = let t = toJSStr s
                   in StaticString (s ++) t

-- | A string denoting input from different string representations.
--
data ChoiceString
    -- | Static data
    = Static {-# UNPACK #-} !StaticString
    -- | A Haskell String
    | String String
    -- | A JSString value
    | Text JSString
    -- | A pre-escaped string
    | PreEscaped ChoiceString
    -- | External data in style/script tags, should be checked for validity
    | External ChoiceString
    -- | Concatenation
    | AppendChoiceString ChoiceString ChoiceString
    -- | Empty string
    | EmptyChoiceString

instance Monoid ChoiceString where
    mempty = EmptyChoiceString
    {-# INLINE mempty #-}
    mappend = AppendChoiceString
    {-# INLINE mappend #-}

instance IsString ChoiceString where
    fromString = String
    {-# INLINE fromString #-}

-- | The core Markup datatype.
--
data MarkupM a
    -- | Tag, open tag, end tag, content
    = forall b. Parent StaticString StaticString StaticString (MarkupM b)
    -- | Custom parent
    | forall b. CustomParent ChoiceString (MarkupM b)
    -- | Tag, open tag, end tag
    | Leaf StaticString StaticString StaticString
    -- | Custom leaf
    | CustomLeaf ChoiceString Bool
    -- | HTML content
    | Content ChoiceString
    -- | Concatenation of two HTML pieces
    | forall b c. Append (MarkupM b) (MarkupM c)
    -- | Add an attribute to the inner HTML. Raw key, key, value, HTML to
    -- receive the attribute.
    | AddAttribute StaticString StaticString ChoiceString (MarkupM a)
    -- | Add a custom attribute to the inner HTML.
    | AddCustomAttribute ChoiceString ChoiceString (MarkupM a)
    -- | Empty HTML.
    | Empty
    deriving (Typeable)

-- | Simplification of the 'MarkupM' datatype.
--
type Markup = MarkupM ()

instance Monoid a => Monoid (MarkupM a) where
    mempty = Empty
    {-# INLINE mempty #-}
    mappend = Append
    {-# INLINE mappend #-}
    mconcat = foldr Append Empty
    {-# INLINE mconcat #-}

instance Functor MarkupM where
    -- Safe because it does not contain a value anyway
    fmap _ = unsafeCoerce

instance Monad MarkupM where
    return _ = Empty
    {-# INLINE return #-}
    (>>) = Append
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f
        (error "Text.Blaze.Internal.MarkupM: invalid use of monadic bind")
    {-# INLINE (>>=) #-}

instance IsString (MarkupM a) where
    fromString = Content . fromString
    {-# INLINE fromString #-}

-- | Type for an HTML tag. This can be seen as an internal string type used by
-- BlazeMarkup.
--
newtype Tag = Tag { unTag :: StaticString }
    deriving (IsString)

-- | Type for an attribute.
--
newtype Attribute = Attribute (forall a. MarkupM a -> MarkupM a)

instance Monoid Attribute where
    mempty                            = Attribute id
    Attribute f `mappend` Attribute g = Attribute (g . f)

-- | The type for the value part of an attribute.
--
newtype AttributeValue = AttributeValue { unAttributeValue :: ChoiceString }
    deriving (IsString, Monoid)

-- | Create a custom parent element
customParent :: Tag     -- ^ Element tag
             -> Markup  -- ^ Content
             -> Markup  -- ^ Resulting markup
customParent tag = CustomParent (Static $ unTag tag)

-- | Create a custom leaf element
customLeaf :: Tag     -- ^ Element tag
           -> Bool    -- ^ Close the leaf?
           -> Markup  -- ^ Resulting markup
customLeaf tag = CustomLeaf (Static $ unTag tag)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Raw key
          -> Tag             -- ^ Shared key string for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute rawKey key value = Attribute $
    AddAttribute (unTag rawKey) (unTag key) (unAttributeValue value)
{-# INLINE attribute #-}

-- | From HTML 5 onwards, the user is able to specify custom data attributes.
--
-- An example:
--
-- > <p data-foo="bar">Hello.</p>
--
-- We support this in BlazeMarkup using this funcion. The above fragment could
-- be described using BlazeMarkup with:
--
-- > p ! dataAttribute "foo" "bar" $ "Hello."
--
dataAttribute :: Tag             -- ^ Name of the attribute.
              -> AttributeValue  -- ^ Value for the attribute.
              -> Attribute       -- ^ Resulting HTML attribute.
dataAttribute tag value = Attribute $ AddCustomAttribute
    (Static "data-" `mappend` Static (unTag tag))
    (unAttributeValue value)
{-# INLINE dataAttribute #-}

-- | Create a custom attribute. This is not specified in the HTML spec, but some
-- JavaScript libraries rely on it.
--
-- An example:
--
-- > <select dojoType="select">foo</select>
--
-- Can be produced using:
--
-- > select ! customAttribute "dojoType" "select" $ "foo"
--
customAttribute :: Tag             -- ^ Name of the attribute
                -> AttributeValue  -- ^ Value for the attribute
                -> Attribute       -- ^ Resulting HTML attribtue
customAttribute tag value = Attribute $ AddCustomAttribute
    (Static $ unTag tag)
    (unAttributeValue value)
{-# INLINE customAttribute #-}

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: JSString -- ^ Text to render.
     -> Markup   -- ^ Resulting HTML fragment.
text = Content . Text
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: JSString    -- ^ Text to insert
               -> Markup  -- ^ Resulting HTML fragment
preEscapedText = Content . PreEscaped . Text
{-# INLINE preEscapedText #-}

-- | Create an HTML snippet from a 'String'.
--
string :: String  -- ^ String to insert.
       -> Markup  -- ^ Resulting HTML fragment.
string = Content . String
{-# INLINE string #-}

-- | Create an HTML snippet from a 'String' without escaping
--
preEscapedString :: String  -- ^ String to insert.
                 -> Markup  -- ^ Resulting HTML fragment.
preEscapedString = Content . PreEscaped . String
{-# INLINE preEscapedString #-}

-- | Create a 'Tag' from some 'Text'.
--
textTag :: JSString  -- ^ Text to create a tag from
        -> Tag   -- ^ Resulting tag
textTag t = Tag $ StaticString (fromJSStr t ++) t

-- | Create a 'Tag' from a 'String'.
--
stringTag :: String  -- ^ String to create a tag from
          -> Tag     -- ^ Resulting tag
stringTag = Tag . fromString

-- | Render an attribute value from 'Text'.
--
textValue :: JSString       -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . Text
{-# INLINE textValue #-}

-- | Render an attribute value from 'Text' without escaping.
--
preEscapedTextValue :: JSString        -- ^ The actual value
                    -> AttributeValue  -- ^ Resulting attribute value
preEscapedTextValue = AttributeValue . PreEscaped . Text
{-# INLINE preEscapedTextValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . String
{-# INLINE stringValue #-}

-- | Create an attribute value from a 'String' without escaping.
--
preEscapedStringValue :: String -> AttributeValue
preEscapedStringValue = AttributeValue . PreEscaped . String
{-# INLINE preEscapedStringValue #-}

-- | Used for applying attributes. You should not define your own instances of
-- this class.
class Attributable h where
    -- | Apply an attribute to an element.
    --
    -- Example:
    --
    -- > img ! src "foo.png"
    --
    -- Result:
    --
    -- > <img src="foo.png" />
    --
    -- This can be used on nested elements as well.
    --
    -- Example:
    --
    -- > p ! style "float: right" $ "Hello!"
    --
    -- Result:
    --
    -- > <p style="float: right">Hello!</p>
    --
    (!) :: h -> Attribute -> h

instance Attributable (MarkupM a) where
    (!) h (Attribute f) = f h
    {-# INLINE (!) #-}

instance Attributable (MarkupM a -> MarkupM b) where
    h ! f = (! f) . h
    {-# INLINE (!) #-}

-- | Shorthand for setting an attribute depending on a conditional.
--
-- Example:
--
-- > p !? (isBig, A.class "big") $ "Hello"
--
-- Gives the same result as:
--
-- > (if isBig then p ! A.class "big" else p) "Hello"
--
(!?) :: Attributable h => h -> (Bool, Attribute) -> h
(!?) h (c, a) = if c then h ! a else h

-- | Mark HTML as external data. External data can be:
--
-- * CSS data in a @<style>@ tag;
--
-- * Script data in a @<script>@ tag.
--
-- This function is applied automatically when using the @style@ or @script@
-- combinators.
--
external :: MarkupM a -> MarkupM a
external (Content x) = Content $ External x
external (Append x y) = Append (external x) (external y)
external (Parent x y z i) = Parent x y z $ external i
external (CustomParent x i) = CustomParent x $ external i
external (AddAttribute x y z i) = AddAttribute x y z $ external i
external (AddCustomAttribute x y i) = AddCustomAttribute x y $ external i
external x = x
{-# INLINE external #-}

-- | Take only the text content of an HTML tree.
--
-- > contents $ do
-- >     p ! $ "Hello "
-- >     p ! $ "Word!"
--
-- Result:
--
-- > Hello World!
--
contents :: MarkupM a -> MarkupM b
contents (Parent _ _ _ c)           = contents c
contents (CustomParent _ c)         = contents c
contents (Content c)                = Content c
contents (Append c1 c2)             = Append (contents c1) (contents c2)
contents (AddAttribute _ _ _ c)     = contents c
contents (AddCustomAttribute _ _ c) = contents c
contents _                          = Empty

