{-# LANGUAGE TemplateHaskell #-}

module Advent
    ( input
    ) where

import Data.Char (toLower)
import System.FilePath ((</>), replaceExtension, takeFileName)
import Language.Haskell.TH (Q, Exp(..), Lit(..), Loc(..))
import Language.Haskell.TH.Syntax (qLocation)

input :: Q Exp
input = do
    fp <- map toLower . loc_filename <$> qLocation
    let lit = pure $ LitE (StringL ("inputs" </> replaceExtension (takeFileName fp) "txt"))
    [e|readFile $(lit)|]
