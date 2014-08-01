--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Monoid         (mappend)
import           Hakyll

import qualified Text.Pandoc.Options as P

import           HistoryContext
import           PygmentsFilter

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
                                           , P.writerHTMLMathMethod = P.MathML (Just "") }

bundleFilter :: String -> [String] -> String -> Compiler String
bundleFilter cmd args = unixFilter "bundle" (["exec", cmd] ++ args)

