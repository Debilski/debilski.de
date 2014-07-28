--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import           Data.List.Utils (split)
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

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "javascripts/**" $ do
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
pandocOptions = defaultHakyllWriterOptions { P.writerHTMLMathMethod = P.MathML (Just "") } -- P.MathJax ""

applyPygments :: Pandoc -> Compiler Pandoc
applyPygments = walkM changeBlocks
  where
    changeBlocks :: Block -> Compiler Block
    changeBlocks (CodeBlock (_, options, _) code) = RawBlock "html" <$> pygments code options
    changeBlocks x = return x

-- https://gist.github.com/fizruk/6620756
pygments:: String -> [String] -> Compiler String
pygments code options
         | (length options) == 1 = unixFilter "pygmentize" ["-l", (map toLower (head options)),  "-f", "html"] code
         | (length options) == 2 = unixFilter "pygmentize" ["-l", (map toLower (head options)), "-O linenos=inline",  "-f", "html"] code
         | otherwise = return $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"

bundleFilter cmd args = unixFilter "bundle" (["exec", cmd] ++ args)

fileHistory :: FilePath -> Compiler [(String, String, String)]
fileHistory fp = fmap extractHistory $ unixFilter "git" ["log", "--format=%h%x09%aD%x09%s", "--", fp] ""
  where
    extractHistory s = fmap extractLine (lines s)
    extractLine l = (words !! 0, words !! 1, words !! 2)
      where
        words = split "\009" l

historyContext :: FilePath -> Context b
historyContext fp = listField "history" fields items
  where
    items = fileHistory fp >>= mapM makeItem
    fields = mconcat [historyField "histhash" hash, historyField "histlog" log, historyField "histdate" date]
    historyField name v = field name (return . v . itemBody)
    hash (a, _, _) = a
    date (_, a, _) = a
    log (_, _, a) = a

