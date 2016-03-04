
module DBus.TH.Introspection.Output where

import Control.Monad
import Data.List
import Language.Haskell.TH

import DBus
import DBus.Client
import qualified DBus.Introspection as I
import DBus.TH as TH

import DBus.TH.Introspection.Types

pprintFunc :: TH.Function -> String
pprintFunc fn = "\"" ++ fnName fn ++ "\" =:: " ++ formatSignature (fnSignature fn)
  where
    formatSignature (Return name) = "Return ''" ++ nameBase name
    formatSignature (name :-> sig) = "''" ++ nameBase name ++ " :-> " ++ formatSignature sig

pprintInterface :: String -> Maybe String -> String -> [String] -> String
pprintInterface serviceName (Just objectPath) ifaceName funcs =
    "interface \"" ++ serviceName ++ "\" \"" ++ objectPath ++ "\" \"" ++ ifaceName ++ "\" Nothing [\n" ++
    (intercalate ",\n" $ map (\s -> "    " ++ s) funcs) ++
    "\n  ]\n"
pprintInterface serviceName Nothing ifaceName funcs =
    "interface' \"" ++ serviceName ++ "\" Nothing \"" ++ ifaceName ++ "\" Nothing [\n" ++
    (intercalate ",\n" $ map (\s -> "    " ++ s) funcs) ++
    "\n  ]\n"

header :: String -> String
header modName =
  "{-# LANGUAGE TemplateHaskell #-}\n" ++
  "module " ++ modName ++ " where\n" ++
  "\n" ++
  "import DBus.TH\n" ++
  "import DBus.TH.Introspection\n"

