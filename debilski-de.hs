--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.Pandoc.Options as P

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Data.Char(toLower)
import System.Process (readProcess)
import System.IO.Unsafe



--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "scss/styles.scss" $ do
        route   $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
        compile $ getResourceString
            >>= withItemBody (bundleFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
            >>= return . fmap compressCss

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

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` postCtx `mappend` defaultContext)
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
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocMathCompiler = pandocCompilerWithTransform readers writers applyPygments
  where
    readers = defaultHakyllReaderOptions { P.readerExtensions = P.pandocExtensions }
    writers = pandocOptions

pandocOptions :: P.WriterOptions
pandocOptions = defaultHakyllWriterOptions { P.writerHTMLMathMethod = P.MathML (Just "") } -- P.MathJax ""

applyPygments :: Pandoc -> Pandoc
applyPygments = walk changeBlocks
  where
    changeBlocks :: Block -> Block
    changeBlocks (CodeBlock (_, options, _) code) = (RawBlock "html" (pygments code options))
    changeBlocks x = x

-- https://gist.github.com/fizruk/6620756
pygments:: String -> [String] -> String
pygments code options
         | (length options) == 1 = unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)),  "-f", "html"] code
         | (length options) == 2 = unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)), "-O linenos=inline",  "-f", "html"] code
         | otherwise = "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
                  then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                  else ""


bundleFilter cmd args = unixFilter "bundle" (["exec", cmd] ++ args)

