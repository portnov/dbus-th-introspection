{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Control.Monad
import Data.List
import Language.Haskell.TH

import DBus
import DBus.Client
import qualified DBus.Introspection as I
import DBus.TH as TH

import System.Environment

introspect :: Client -> BusName -> ObjectPath -> IO I.Object
introspect client service path = do
	reply <- call_ client (methodCall path "org.freedesktop.DBus.Introspectable" "Introspect")
		{ methodCallDestination = Just service
		}
	let Just xml = fromVariant (methodReturnBody reply !! 0)
	case I.parseXML path xml of
		Just info -> return info
		Nothing -> error ("Invalid introspection XML: " ++ show xml)

formatMethodArg :: I.MethodArg -> String
formatMethodArg a =
  show (I.methodArgDirection a) ++ " " ++
  show (I.methodArgType a) ++ " " ++
  (I.methodArgName a)

dbusType2haskell :: DBus.Type -> Either String Name
dbusType2haskell TypeBoolean = return ''Bool
dbusType2haskell TypeWord8 = return ''Word8
dbusType2haskell TypeWord16 = return ''Word16
dbusType2haskell TypeWord32 = return ''Word32
dbusType2haskell TypeInt16 = return ''Int16
dbusType2haskell TypeInt32 = return ''Int32
dbusType2haskell TypeDouble = return ''Double
dbusType2haskell TypeString = return ''String
dbusType2haskell t = Left $ "Unsupported type " ++ show t

method2function :: I.Method -> Either String TH.Function
method2function m = do
    signature <- toSignature (I.methodArgs m)
    return $ name =:: signature
  where
    name = formatMemberName (I.methodName m)

    toSignature :: [I.MethodArg] -> Either String TH.Signature
    toSignature args = do
      let (inArgs, outArgs) = partition (\a -> I.methodArgDirection a == I.directionIn) args
      if length outArgs > 1
        then fail $ "Method " ++ name ++ " has more than one out parameter"
        else do
             outArgName <- if null outArgs
                             then return ''()
                             else dbusType2haskell $ I.methodArgType $ head outArgs
             transformInArgs outArgName inArgs

    transformInArgs :: Name -> [I.MethodArg] -> Either String TH.Signature
    transformInArgs retType [] = return $ Return retType
    transformInArgs retType (a:as) = do
        argName <- dbusType2haskell (I.methodArgType a)
        rest <- transformInArgs retType as
        return $ argName :-> rest
      
pprintFunc :: TH.Function -> String
pprintFunc fn = "\"" ++ fnName fn ++ "\" =:: " ++ formatSignature (fnSignature fn)
  where
    formatSignature (Return name) = "Return ''" ++ nameBase name
    formatSignature (name :-> sig) = "''" ++ nameBase name ++ " :-> " ++ formatSignature sig

pprintInterface :: String -> String -> String -> [String] -> String
pprintInterface serviceName objectPath ifaceName funcs =
    "interface \"" ++ serviceName ++ "\" \"" ++ objectPath ++ "\" \"" ++ ifaceName ++ "\" Nothing [\n" ++
    (intercalate ",\n" $ map (\s -> "    " ++ s) funcs) ++
    "\n  ]\n"

header :: String -> String
header modName =
  "{-# LANGUAGE TemplateHaskell #-}\n" ++
  "module " ++ modName ++ " where\n" ++
  "\n" ++
  "import DBus.TH\n"

main :: IO ()
main = do
  [modName, serviceName, objectPath] <- getArgs
  dbus <- connectSession
  -- let serviceName =  "im.pidgin.purple.PurpleService"
  -- let objectPath =  "/im/pidgin/purple/PurpleObject"
  ob <- introspect dbus (busName_ serviceName) (objectPath_ objectPath)
  putStrLn $ header modName
  forM_ (I.objectInterfaces ob) $ \iface -> do
    let ifaceName = formatInterfaceName (I.interfaceName iface)
    putStrLn $ "-- Interface " ++ ifaceName
    funcs <- forM (I.interfaceMethods iface) $ \m -> do
               -- putStrLn $ "    -- Method: " ++ 
               let methodName = formatMemberName (I.methodName m)
               case method2function m of
                 Left err -> do
                             return [ "-- Error: method " ++  methodName ++ ": " ++ err ]
                 Right fn -> return [ pprintFunc fn ]
    putStrLn $ pprintInterface serviceName objectPath ifaceName (concat funcs)

--       forM_ (I.methodArgs m) $ \a -> do
--         putStrLn $ "    " ++ formatMethodArg a
