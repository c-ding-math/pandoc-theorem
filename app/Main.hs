{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Monad.State
import qualified Data.Text as T
import TextShow

main :: IO ()
main = toJSONFilter theoremFilter where
    theoremFilter :: Pandoc -> Pandoc
    theoremFilter doc = 
        let newdoc = (addTheoremLabels . addProofLabels) doc
        in walk (autoLink newdoc) newdoc

-- add theorem label data to theorem-like blocks
addTheoremLabel :: Block -> State Int Block
addTheoremLabel (Div (divId,classes,attrs)  x ) | "theorem-like" `elem` classes = do
    let title = renderTitle (extractAttrValue "title" attrs)
    name <- if "unnumbered" `notElem` classes
        then do 
            modify (+1)
            n <- get
            return $ extractAttrValue "name" attrs <> " " <> showt n
        else
            return $ extractAttrValue "name" attrs
    return $ case x of
        (Para inlines):blocks -> Div (divId,classes,attrs) $ Para (Span ("",["theorem-like-label"],[]) [Span ("",["theorem-like-name"],[]) [Strong [Str name]],Span ("",["theorem-like-title"],[]) [Strong [Str title]]]:inlines):blocks
        _ -> Div (divId,classes,attrs) $ Div ("",["theorem-like-label"],[]) [Plain [Span ("",["theorem-like-name"],[]) [Strong [Str name]],Span ("",["theorem-like-title"],[]) [Strong [Str title]]]]:x
addTheoremLabel x = return x

addTheoremLabels :: Pandoc -> Pandoc
addTheoremLabels doc = evalState (walkM addTheoremLabel doc) 0

-- add proof label data to proof-like blocks
addProofLabel :: Block -> Block
addProofLabel (Div (divId,classes,attrs)  x ) | "proof-like" `elem` classes = do
    let (name, title) = (extractAttrValue "name" attrs, renderTitle (extractAttrValue "title" attrs))
    case x of
        (Para inlines):blocks -> Div (divId,classes,attrs) $ Para (Span ("",["theorem-like-label"],[]) [Span ("",["theorem-like-name"],[]) [Strong [Str name]],Span ("",["theorem-like-title"],[]) [Strong [Str title]]]:inlines):blocks
        _ -> Div (divId,classes,attrs) $ Div ("",["theorem-like-label"],[]) [Plain [Span ("",["theorem-like-name"],[]) [Strong [Str name]],Span ("",["theorem-like-title"],[]) [Strong [Str title]]]]:x
addProofLabel x = x

addProofLabels :: Pandoc -> Pandoc
addProofLabels = walk addProofLabel

-- clever reference to theorem-like blocks
autoLink::Pandoc -> Inline-> Inline
autoLink doc (Link attr [] (src,x))=case query (queryTheorem src) doc of
    [] -> Link attr [Str src] (src,x)
    a:_ -> Link attr [Str a] (src,x)
autoLink _  x= x

queryTheorem :: T.Text -> Block -> [T.Text]
queryTheorem src (Div (divId,classes,_) ((Para (Span ("",["theorem-like-label"],[]) [Span ("",["theorem-like-name"],[]) [Strong [Str name]],_]:_)):_)) | ("theorem-like" `elem` classes) && ("#"<>divId==src)= 
    [name]
queryTheorem src (Div (divId,classes,_) ((Div ("",["theorem-like-label"],[]) [Plain [Span ("",["theorem-like-name"],[]) [Strong [Str name]],_]]):_)) | ("theorem-like" `elem` classes) && ("#"<>divId==src)= 
    [name]
queryTheorem _ _ = []

-- extract attribute value
extractAttrValue :: T.Text -> [(T.Text, T.Text)] -> T.Text
extractAttrValue attr=mconcat . map helper where 
    helper :: (T.Text, T.Text) -> T.Text
    helper (name, value)|name==attr=value
    helper _=""

-- render title
renderTitle:: T.Text -> T.Text
renderTitle title = if T.strip title=="" 
    then "  " 
    else " ("<>title<>")  "