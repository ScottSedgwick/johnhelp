{-# LANGUAGE OverloadedStrings #-}
module Parser 
  ( Language
  , SourceType (..)
  , SourceText (..)
  , Segment (..)
  ) where

import Data.Text
import Data.Default

type Language = Text

data SourceType 
  = Xliff

newtype SourceText 
  = SingleText Text

data Segment = Segment
  { sourceType :: SourceType
  , sourceText :: SourceText
  , identifier :: Text
  , sourceLang :: Language
  , context :: Text 
  } 
  
instance Default Segment where
  def = Segment 
      { sourceType = Xliff
      , sourceText = SingleText ""
      , identifier = ""
      , sourceLang = ""
      , context    = ""
      }