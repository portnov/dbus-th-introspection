{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

import Control.Monad
import Data.List
import Language.Haskell.TH

import DBus
import DBus.Client
import qualified DBus.Introspection as I
import DBus.TH as TH

import DBus.TH.Introspection

import System.Console.CmdArgs
import System.Environment

data Options = Options {
    moduleName :: String,
    outputFile :: Maybe String,
    system :: Bool,
    serviceName :: String,
    objectPath :: String,
    interfaceName :: String,
    dynamicObject :: Bool
  }
  deriving (Data, Typeable, Show, Eq)

options = Options {
    moduleName = "Interface" &= typ "NAME" &= name "module" &= help "Haskell module name",
    outputFile = def &= typFile &= name "output" &= help "Output file",
    system = False &= help "Use system bus instead of sesion bus",
    serviceName = def &= typ "SERVICE.NAME" &= argPos 0 &= opt ("" :: String),
    objectPath = def &= typ "/PATH/TO/OBJECT" &= argPos 1 &= opt ("" :: String),
    interfaceName = def &= typ "INTERFACE.NAME" &= argPos 2 &= opt ("" :: String),
    dynamicObject = def &= name "dynamic" &= help "If specified - generated functions will get object path as 2nd argument"
  } &=
  help "Introspect specified DBus object/interface and generate TemplateHaskell source for calling all functions from Haskell" &=
  program "dbus-introspect-hs" &= 
  summary "dbus-introspect-hs program"

main :: IO ()
main = do
  opts <- cmdArgs options
  dbus <- if system opts
            then connectSystem
            else connectSession
  services <- if serviceName opts == ""
                then do
                     ss <- listNames dbus
                     case ss of
                       Nothing -> fail $ "Can't obtain list of services"
                       Just list -> return $ map busName_ list
                else return [busName_ $ serviceName opts]

  putStrLn $ header (moduleName opts)

  forM_ services $ \service -> do
    putStrLn $ "-- Service " ++ show service
    objects <- if objectPath opts == ""
                 then do
                      obs <- getServiceObjects dbus service "/"
                      return $ map I.objectPath obs
                 else return [objectPath_ $ objectPath opts]
    forM_ objects $ \object -> do
      ob <- introspect dbus service object
      forM_ (I.objectInterfaces ob) $ \iface -> do
        let ifaceName = formatInterfaceName (I.interfaceName iface)
            useIface = case interfaceName opts of
                         "" -> True
                         name -> name == ifaceName
        when useIface $ do
            putStrLn $ "-- Interface " ++ ifaceName
            funcs <- forM (I.interfaceMethods iface) $ \m -> do
                       -- putStrLn $ "    -- Method: " ++ 
                       let methodName = formatMemberName (I.methodName m)
                       case method2function m of
                         Left err -> do
                                     return [ "-- Error: method " ++  methodName ++ ": " ++ err ]
                         Right fn -> return [ pprintFunc fn ]
            let path = if dynamicObject opts 
                         then Nothing
                         else Just (formatObjectPath object)
            putStrLn $ pprintInterface (serviceName opts) path ifaceName (concat funcs)
