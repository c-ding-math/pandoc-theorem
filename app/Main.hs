{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.JSON
--import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.IORef             (IORef, modifyIORef, newIORef, readIORef) 
import Data.Text              

main :: IO ()
main = toJSONFilter theoremFilter where
    theoremFilter::Pandoc->IO Pandoc
    theoremFilter doc = do
        newdoc<-addTheoremLabels doc
        return $ walk (autoLink newdoc) newdoc

-- add theorem label data to theorem-like blocks
addTheoremLabel :: IORef Int ->  Block -> IO Block
addTheoremLabel counter (Div (divId,classes,attrs)  x ) | "theorem-like" `Prelude.elem` classes = do
    if ("unnumbered" `Prelude.notElem` classes)
        then do 
            modifyIORef counter (+1)
            n<-readIORef counter
            return $ Div (divId,classes,attrs++[("data-label", (extractAttrValue "name" attrs) <> " " <> (pack (show n)) <> renderTitle (extractAttrValue "title"))])  x
        else
            return $ Div (divId,classes,attrs++[("data-label", (extractAttrValue "name" attrs)<> renderTitle (extractAttrValue "title"))])  x
addTheoremLabel _ x = return x

addTheoremLabels :: Pandoc -> IO Pandoc
addTheoremLabels doc = do
    counter <- newIORef 0
    walkM (addTheoremLabel counter) doc

-- add proof label data to proof-like blocks
addProofLabel :: Block -> Block
addProofLabel (Div (divId,classes,attrs)  x ) | "proof-like" `Prelude.elem` classes = 
    Div (divId,classes,attrs++[("data-label", (extractAttrValue "name" attrs) <> renderTitle (extractAttrValue "title"))])  x
addProofLabels :: Pandoc -> Pandoc
addProofLabels doc = walk (addProofLabel) doc

-- clever reference to theorem-like blocks
autoLink::Pandoc -> Inline-> Inline
autoLink doc (Link attr [] (src,x))=case query (queryTheorem src) doc of
    [] -> Link attr [Str src] (src,x)
    a:_ -> Link attr [Str a] (src,x)
autoLink _  x= x

queryTheorem::Text->Block->[Text]
queryTheorem src (Div (divId,classes,attrs)  _ )| ("theorem-like" `Prelude.elem` classes) && ("#"<>divId==src)= 
    [extractAttrValue "data-label" attrs]
queryTheorem _ _ = []

-- extract attribute value
extractAttrValue::Text->[(Text,Text)]->Text
extractAttrValue attr=mconcat . (Prelude.map helper) where 
    helper::(Text,Text)-> Text
    helper (name, value)|name==attr=value
    helper _=""

-- render title
renderTitle::Text->Text
renderTitle title|title==""=""
renderTitle title="("<>title<>")"