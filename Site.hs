{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
import           Control.Monad               ((>=>))
import           Data.Monoid                 (mappend, mconcat)
import           Text.Blaze.Html             (toHtml, toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "css/*" $ compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["site.css"] $ do
        route     idRoute
        compile $ bundleCss 
            ["css/bootstrap.css", "css/theme.css", "css/sugar.css"]

    match "templates/*" $ compile templateCompiler

    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField tag "title"                          `mappend`
                      listField "posts" (postCtx tags) (return posts) `mappend`
                      defaultContext
            makeItem "" >>= applyPanel ctx

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler 
            >>= saveSnapshot "postRaw" >>= applyPanel (postCtx tags)

    match "about.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler >>= applyPanel defaultContext

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "postRaw" 
                >>= mapM (loadAndApplyTemplate 
                            "templates/linked-post.html" 
                            (postCtx tags)) 
                >>= recentFirst
            let indexCtx = 
                    listField "posts" (postCtx tags) (return posts) `mappend` 
                    constField "title" "Type Checked"               `mappend`
                    defaultContext
            
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/layout.html" indexCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    tagBadges "tags" tags        `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

applyPanel :: Context String -> Item String -> Compiler (Item String)
applyPanel ctx = 
    loadAndApplyTemplate "templates/panel.html"  ctx >=>
    loadAndApplyTemplate "templates/layout.html" ctx >=>
    relativizeUrls

--------------------------------------------------------------------------------
-- | Bundle css files to minimize HTTP requests.
bundleCss :: [Identifier] -> Compiler (Item String)
bundleCss ids = concatMap itemBody `fmap` mapM load ids >>= makeItem

-- | Render tag badges instead of a simple comma-delimited list of links.
tagBadges :: String -> Tags -> Context a
tagBadges = tagsFieldWith getTags renderTagBadge mconcat

renderTagBadge :: String -> Maybe FilePath -> Maybe H.Html
renderTagBadge _ Nothing           = Nothing
renderTagBadge tag (Just filePath) = Just $ 
    H.span ! A.class_ "badge tag" $ 
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
