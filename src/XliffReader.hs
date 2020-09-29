{-# LANGUAGE OverloadedStrings #-}
module XliffReader
       ( parserXliffStrings
       , extractXliffEntries
       , extractElementFromEntry
       ) where

import Prelude as P hiding (FilePath)
import Data.Text as T hiding (map, null, head, filter)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Parser
import Data.Default
import Filesystem.Path.CurrentOS hiding (null)

parserXliffStrings :: ArrowXml a => FilePath -> a XmlTree [Segment]
parserXliffStrings entry = deep ( hasName "trans-unit")
                        >>> arr (extractElementFromEntry "en" "ar")

extractXliffEntries :: FilePath -> IO (Either Text [Segment])
extractXliffEntries entry = do
  result <- runX $ readDocument [] ( encodeString entry) >>> parserXliffStrings entry
  return $ Right (P.concat result)
--      <trans-unit id="Acj-8h-dlJ.text" xml:space="preserve">
--        <source>No</source>
--        <target>لا</target>
--        <note>Class = "UILabel"; text = "No"; ObjectID = "Acj-8h-dlJ";</note>
--      </trans-unit>
--
extractElementFromEntry :: Language -> Language -> XmlTree -> [Segment]
extractElementFromEntry fromLang toLang (NTree (XTag qname attrs) children)
  | localPart qname == "trans-unit" = [ def { sourceType = Xliff
                                            , sourceText = SingleText $ getFirst (byQName "source") children
                                            , identifier = identifier'
                                            , sourceLang = fromLang
                                            , context = note'
                                            }
                                      , def { sourceType = Xliff
                                            , sourceText = SingleText $ getFirst (byQName "target") children
                                            , identifier = identifier'
                                            , sourceLang = toLang
                                            , context = note'
                                            }
                                      ]
  where
    identifier' = getFirst (byQName "id") attrs
    note' = getFirst (byQName "note") children

getFirst :: (NTree XNode -> Maybe Text) -> XmlTrees -> Text
getFirst selector children = head $ mapMaybe selector children
  
byQName :: String -> NTree XNode -> Maybe Text
byQName name (NTree (XTag qname attrs) [NTree (XText a) []])
      | localPart qname == name = Just $ pack a