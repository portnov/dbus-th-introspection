
module DBus.TH.Introspection.Output where

import Data.List
import Language.Haskell.TH

import DBus.TH as TH

-- | Gernerate DBus function declaration
pprintFunc :: TH.Function -> String
pprintFunc fn = "\"" ++ fnName fn ++ "\" =:: " ++ formatSignature (fnSignature fn)
  where
    formatSignature (Return name) = "Return ''" ++ nameBase name
    formatSignature (name :-> sig) = "''" ++ nameBase name ++ " :-> " ++ formatSignature sig

-- | Generate DBus interface declaration
pprintInterface :: String        -- ^ Service name
                -> Maybe String  -- ^ Just name for static object name; Nothing for dynamic object name
                -> String        -- ^ Interface name
                -> [String]      -- ^ Rendered function declarations
                -> String
pprintInterface serviceName (Just objectPath) ifaceName funcs =
    "interface \"" ++ serviceName ++ "\" \"" ++ objectPath ++ "\" \"" ++ ifaceName ++ "\" Nothing [\n" ++
    (intercalate ",\n" $ map (\s -> "    " ++ s) funcs) ++
    "\n  ]\n"
pprintInterface serviceName Nothing ifaceName funcs =
    "interface' \"" ++ serviceName ++ "\" Nothing \"" ++ ifaceName ++ "\" Nothing [\n" ++
    (intercalate ",\n" $ map (\s -> "    " ++ s) funcs) ++
    "\n  ]\n"

-- | Module header
header :: String -> String
header modName =
  "{-# LANGUAGE TemplateHaskell #-}\n" ++
  "module " ++ modName ++ " where\n" ++
  "\n" ++
  "import DBus.TH\n" ++
  "import DBus.TH.Introspection\n"

