{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import DBus.TH
import DBus.Client

func :: Function
func =  "AccountsFind" =:: ''String :-> ''String :-> Return ''Int32

main :: IO ()
main = do
  decs <- runQ $ interface "im.pidgin.purple.PurpleService"
                    "/im/pidgin/purple/PurpleObject"
                    "im.pidgin.purple.PurpleInterface" (Just "Purple") [func]
  putStrLn $ pprint decs


