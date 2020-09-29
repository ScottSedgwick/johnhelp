{-# LANGUAGE OverloadedStrings #-}
module XliffXeno
  ( extractXliffEntries
  , extractElementFromEntry
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Default          as D
import qualified Data.Text             as T
import           Xeno.DOM        (Content(..), Node, attributes, children, contents, name, parse)

import           Parser

--      <trans-unit id="Acj-8h-dlJ.text" xml:space="preserve">
--        <source>No</source>
--        <target>لا</target>
--        <note>Class = "UILabel"; text = "No"; ObjectID = "Acj-8h-dlJ";</note>
--      </trans-unit>
--
extractXliffEntries :: FilePath -> IO(Either T.Text [Segment])
extractXliffEntries filename = do
  xmldata <- B.readFile filename
  case parse xmldata of
    Left e   -> return $ Left (T.pack $ show e)
    Right ns -> return $ Right (extractElementFromEntry "en" "ar" (getFirst "trans-unit" [ns]))

extractElementFromEntry :: Language -> Language -> Node -> [Segment]
extractElementFromEntry fromLang toLang node
  | name node == "trans-unit" = [ D.def { sourceType = Xliff
                                        , sourceText = SingleText $ getText $ getFirst "source" (children node)
                                        , identifier = identifier'
                                        , sourceLang = fromLang
                                        , context = note'
                                        }
                                , D.def { sourceType = Xliff
                                        , sourceText = SingleText $ getText $ getFirst "target" (children node)
                                        , identifier = identifier'
                                        , sourceLang = toLang
                                        , context = note'
                                        }
                                ]
  where
    identifier' = T.pack $ BC.unpack $ getAttr "id" node
    note' = getText $ getFirst "note" (children node)

getFirst :: B.ByteString -> [Node] -> Node
getFirst nm ns = head $ deep nm ns
  where
    deep _ []      = []
    deep nm' (n:ns') = if name n == nm' 
                       then n : deep nm' ns
                       else deep nm' (children n) ++ deep nm' ns'

getText :: Node -> T.Text
getText n = T.concat $ map getText' (contents n)
  where
    getText' (Element n') = getText n'
    getText' (Text bs)    = T.pack $ BC.unpack bs
    getText' (CData bs)   = T.pack $ BC.unpack bs

getAttr :: B.ByteString -> Node -> B.ByteString
getAttr n ns = snd $ head $ filter (\(a,_) -> a == n) (attributes ns)