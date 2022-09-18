{-# LANGUAGE OverloadedStrings #-}

module Plugin.GhcTags.CTag where

import qualified Data.Text as Text
import Data.Version

import Paths_ghc_tags_plugin

import           GhcTags.CTag.Header


headers :: [Header]
headers =
    [ Header FileFormat     Nothing 2 ""
    , Header FileSorted     Nothing 1 ""
    , Header FileEncoding   Nothing "utf-8" ""
    , Header ProgramAuthor  Nothing "Marcin Szamotulski" ""
    , Header ProgramName    Nothing "ghc-tags-plugin" ""
    , Header ProgramUrl     Nothing "https://hackage.haskell.org/package/ghc-tags-plugin" ""
    , Header ProgramVersion Nothing (Text.pack $ showVersion version) ""

    , Header FieldDescription haskellLang "type" "type of expression"
    , Header FieldDescription haskellLang "ffi"  "foreign object name"
    , Header FieldDescription haskellLang "file" "not exported term"
    , Header FieldDescription haskellLang "instance" "class, type or data type instance"
    , Header FieldDescription haskellLang "Kind" "kind of a type"

    , Header KindDescription haskellLang "M" "module name"
    , Header KindDescription haskellLang "`" "module top level term, but not a function"
    , Header KindDescription haskellLang "λ" "module top level function term"
    , Header KindDescription haskellLang "Λ" "type constructor"
    , Header KindDescription haskellLang "c" "data constructor"
    , Header KindDescription haskellLang "g" "gadt constructor"
    , Header KindDescription haskellLang "r" "record field"
    , Header KindDescription haskellLang "≡" "type synonym"
    , Header KindDescription haskellLang "~" "type signature"
    , Header KindDescription haskellLang "p" "pattern synonym"
    , Header KindDescription haskellLang "C" "type class"
    , Header KindDescription haskellLang "m" "type class member"
    , Header KindDescription haskellLang "i" "type class instance"
    , Header KindDescription haskellLang "x" "type class instance member"
    , Header KindDescription haskellLang "F" "type family"
    , Header KindDescription haskellLang "f" "type family instance"
    , Header KindDescription haskellLang "D" "data type family"
    , Header KindDescription haskellLang "d" "data type family instance"
    , Header KindDescription haskellLang "I" "foreign import"
    , Header KindDescription haskellLang "E" "foreign export"
    ]
  where
    haskellLang = Just "Haskell"
