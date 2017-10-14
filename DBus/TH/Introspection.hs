{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module DBus.TH.Introspection
  (
   module DBus.TH.Introspection.Types,
   module DBus.TH.Introspection.Output,
   listNames,
   introspect,
   getServiceObjects,
   dbusType2haskell,
   method2function
  ) where

import Control.Monad
import Data.List
import Language.Haskell.TH

import DBus
import DBus.Client
import qualified DBus.Introspection as I
import DBus.TH as TH

import DBus.TH.Introspection.Types
import DBus.TH.Introspection.Output

interface "org.freedesktop.DBus" "/" "org.freedesktop.DBus" Nothing [
    "ListNames" =:: Return ''ListStr
  ]

-- | Run DBus introspection on specified object
introspect :: Client -> BusName -> ObjectPath -> IO I.Object
introspect client service path = do
	reply <- call_ client (methodCall path "org.freedesktop.DBus.Introspectable" "Introspect")
		{ methodCallDestination = Just service
		}
	let Just xml = fromVariant (methodReturnBody reply !! 0)
	case I.parseXML path xml of
		Just info -> return info
		Nothing -> error ("Invalid introspection XML: " ++ show xml)

-- | Obtain list of all objects exported by given interface, starting
-- with specified path.
getServiceObjects :: Client        -- ^ DBus connection
                  -> BusName       -- ^ Service name
                  -> ObjectPath    -- ^ Object path to start with. For example, \"/\".
                  -> IO [I.Object]
getServiceObjects dbus service path = do
  ob <- introspect dbus service path
  children <- forM (I.objectChildren ob) $ \child ->
                  getServiceObjects dbus service (I.objectPath child)
  return $ ob : concat children


-- | Try to convert DBus type to Haskell type.
-- Only some relatively simple types are supported as for now.
dbusType2haskell :: DBus.Type -> Either String Name
dbusType2haskell TypeBoolean = return ''Bool
dbusType2haskell TypeWord8 = return ''Word8
dbusType2haskell TypeWord16 = return ''Word16
dbusType2haskell TypeWord32 = return ''Word32
dbusType2haskell TypeInt16 = return ''Int16
dbusType2haskell TypeInt32 = return ''Int32
dbusType2haskell TypeDouble = return ''Double
dbusType2haskell TypeString = return ''String
dbusType2haskell (TypeArray TypeWord8) = return ''ListWord8
dbusType2haskell (TypeArray TypeWord16) = return ''ListWord16
dbusType2haskell (TypeArray TypeWord32) = return ''ListWord32
dbusType2haskell (TypeArray TypeInt16) = return ''ListInt16
dbusType2haskell (TypeArray TypeInt32) = return ''ListInt32
dbusType2haskell (TypeArray TypeString) = return ''ListStr
dbusType2haskell (TypeDictionary TypeString TypeString) = return ''DictStrStr
dbusType2haskell (TypeDictionary TypeString TypeVariant) = return ''DictStrVariant
dbusType2haskell t = Left $ "Unsupported type " ++ show t

-- | Try to convert DBus.Introspection method description into DBus.TH description.
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
        then Left $ "Method " ++ name ++ " has more than one out parameter"
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

