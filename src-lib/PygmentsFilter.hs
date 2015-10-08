--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


module PygmentsFilter
    (
      applyPygments
    ) where

import           Control.Applicative
import           Control.Monad               (guard, (>=>))
import           Data.Char                   (toLower)
import           Data.List.Utils             (join)
import           Hakyll

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk

import qualified Text.Blaze                  as B
import qualified Text.Blaze.Html5            as H5
import qualified Text.Blaze.Html5.Attributes as H5A
import           Text.Blaze.Renderer.String

--------------------------------------------------------------------------------

applyPygments :: Pandoc -> Compiler Pandoc
applyPygments = walkM changeBlocks >=> walkM changeInline
  where
    changeBlocks :: Block -> Compiler Block
    changeBlocks (CodeBlock attrs@(_, cls, _) code) = RawBlock "html" <$> (wrapPre attrs <$> pygments cls code)
    changeBlocks x = return x

    changeInline :: Inline -> Compiler Inline
    changeInline (Code attrs@(_, cls@(_:_), _) code) = RawInline "html" <$> (wrapCode attrs <$> pygments cls code)
    changeInline x = return x

    setAttrs :: Attr -> (H5.Html -> H5.Html) -> (H5.Html -> H5.Html)
    setAttrs (id_, cls, kv) tag = foldl (H5.!) tag attributes
      where
        kv' = case cls of
                (lang : _) -> ("data-language", lang) : kv
                _ -> kv
        attributes :: [B.Attribute]
        attributes = concat [ [ H5A.class_ (B.toValue $ mkClasses cls) ]
                            , H5A.id (B.toValue id_) <$ guard (not $ null id_)
                            , fmap (\(k, v) -> B.customAttribute (B.stringTag k) (B.toValue v)) kv'
                            ]

    wrapCode attrs code = renderMarkup $ setAttrs attrs H5.code (H5.preEscapedToHtml code)
    wrapPre attrs code = renderMarkup $ setAttrs attrs H5.div $ H5.pre (H5.preEscapedToHtml code)

    mkClasses :: [String] -> String
    mkClasses (lang:other) = join " " ("highlight" : ("language-" ++ lang) : other)
    mkClasses _ = "highlight"

-- https://gist.github.com/fizruk/6620756 adapted and improved
pygments :: [String] -> String -> Compiler String
pygments cls = unixFilter "pygmentize" (pygOptions cls)
  where
    pygOptions []       = ["-g", "-f", "html"] ++ args
    pygOptions (lang:_) = ["-l", toLower <$> lang, "-f", "html"] ++ args

    mkOpts     = concatMap $ \o -> ["-O", o]
    args       = concat [ mkOpts ["linenos=inline"] <* guard ("inline_linenos" `elem` cls)
                        , mkOpts ["nowrap"]
                        ]

