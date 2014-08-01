--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List.Utils (split,join)
import           Control.Applicative
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import qualified Text.Pandoc.Options as P

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Data.Char(toLower)

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
applyPygments = walkM changeBlocks
  where
    changeBlocks :: Block -> Compiler Block
    changeBlocks (CodeBlock (_, options, _) code) = RawBlock "html" <$> pygments code options
    changeBlocks x = return x

-- https://gist.github.com/fizruk/6620756 adapted and improved
pygments:: String -> [String] -> Compiler String
pygments code options = unixFilter "pygmentize" pygOptions code
                        -- return $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
  where
    pygOptions = ["-l", lang, "-f", format, "-O", "cssclass=" ++ cssClasses] ++ otherOpts
    lang       = toLower <$> head options
    format     = "html"
    cssClasses = join " " ("highlight" : tail options)
    otherOpts  = if "inline_linenos" `elem` options then ["-O", "linenos=inline"]
                   else []

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

