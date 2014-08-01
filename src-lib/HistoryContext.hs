--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module HistoryContext
    (
      historyContext
    ) where

import           Control.Applicative
import           Data.List.Utils     (split)
import           Data.Monoid         (mconcat)
import           Hakyll

--------------------------------------------------------------------------------

data FileHistory = FileHistory { _hash    :: String
                               , _isodate :: String
                               , _date    :: String
                               , _log     :: String
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

