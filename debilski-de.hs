--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List.Utils (split,join)
import           Control.Applicative
import           Control.Monad ((>=>),guard)
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import qualified Text.Pandoc.Options as P

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Data.Char(toLower)

import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as H5A
import Text.Blaze.Renderer.String

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "javascripts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "humans.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "scss/**.scss" $ do
        compile getResourceBody

    scssDependencies <- makePatternDependency "scss/**.scss"
    rulesExtraDependencies [scssDependencies] $ do
        create ["css/styles.css"] $ do
            route idRoute
            compile $ loadBody (fromFilePath "scss/styles.scss")
                >>= makeItem
                >>= withItemBody (bundleFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
                >>= return . fmap compressCss

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyDefaultTemplate
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyDefaultTemplate
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyDefaultTemplate
                >>= relativizeUrls


    match (fromList ["index.html", "index.markdown"]) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyDefaultTemplate
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

loadAndApplyDefaultTemplate :: Item String -> Compiler (Item String)
loadAndApplyDefaultTemplate item = do
    histCtx <- historyContext <$> getResourceFilePath
    loadAndApplyTemplate "templates/default.html" (postCtx `mappend` defaultContext `mappend` histCtx) item


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWithTransformM readers writers applyPygments
  where
    readers = defaultHakyllReaderOptions { P.readerExtensions = P.pandocExtensions }
    writers = pandocOptions

pandocOptions :: P.WriterOptions
pandocOptions = defaultHakyllWriterOptions { P.writerHtml5 = True
                                           , P.writerHTMLMathMethod = P.MathML (Just "") } -- P.MathJax ""

applyPygments :: Pandoc -> Compiler Pandoc
applyPygments = walkM changeBlocks >=> walkM changeInline
  where
    changeBlocks :: Block -> Compiler Block
    changeBlocks (CodeBlock attrs@(_, cls, _) code) = RawBlock "html" <$> (wrapPre attrs <$> pygments cls code)
    changeBlocks x = return x

    changeInline :: Inline -> Compiler Inline
    changeInline (Code attrs@(_, cls@(_:_), _) code) = RawInline "html" <$> (wrapCode attrs <$> pygments cls code)
    changeInline x = return x

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
    mkClasses (lang:other) = join " " ("highlight" : ("language-" ++ lang) : other)
    mkClasses _ = "highlight"

-- https://gist.github.com/fizruk/6620756 adapted and improved
pygments :: [String] -> String -> Compiler String
pygments cls = unixFilter "pygmentize" (pygOptions cls)
  where
    pygOptions []       = ["-g", "-f", "html"]
    pygOptions (lang:_) = ["-l", toLower <$> lang, "-f", "html"] ++ args
      where
        mkOpts     = concatMap $ \o -> ["-O", o]
        args       = concat [ mkOpts ["linenos=inline"] <* guard ("inline_linenos" `elem` cls)
                            , mkOpts ["nowrap"]
                            ]

bundleFilter :: String -> [String] -> String -> Compiler String
bundleFilter cmd args = unixFilter "bundle" (["exec", cmd] ++ args)

data FileHistory = FileHistory { _hash :: String
                               , _isodate :: String
                               , _date :: String
                               , _log :: String
                               }

fileHistory :: FilePath -> Compiler [FileHistory]
fileHistory fp = extractHistory <$> unixFilter "git" ["log", "--format=%h%x09%ai%x09%aD%x09%s", "--", fp] ""
  where
    extractHistory s = fmap (extractLine . split "\009") (lines s)
    extractLine (hash : isodate : date : log : _) = FileHistory { _hash=hash, _isodate=isofix, _date=date, _log=log }
      where
        isofix = d ++ " " ++ t ++ zone
        d : t : zone : _ = split " " isodate
    extractLine _ = FileHistory { _hash="unknown", _isodate="unknown", _date="unknown", _log="unknown" }

historyContext :: FilePath -> Context b
historyContext fp = listField "history" fields items
  where
    items = fileHistory fp >>= mapM makeItem
    fields = mconcat [ historyField "histhash" _hash
                     , historyField "histisodate" _isodate
                     , historyField "histdate" _date
                     , historyField "histlog" _log
                     ]
    historyField name v = field name (return . v . itemBody)

