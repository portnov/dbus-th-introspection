{-# LANGUAGE TemplateHaskell #-}
module Login1 where

import DBus.TH
import DBus.TH.Introspection

-- Interface org.freedesktop.DBus.Peer
interface' "org.freedesktop.login1" Nothing "org.freedesktop.DBus.Peer" Nothing [
    "Ping" =:: Return ''(),
    "GetMachineId" =:: Return ''String
  ]

-- Interface org.freedesktop.DBus.Introspectable
interface' "org.freedesktop.login1" Nothing "org.freedesktop.DBus.Introspectable" Nothing [
    "Introspect" =:: Return ''String
  ]

-- Interface org.freedesktop.DBus.Properties
interface' "org.freedesktop.login1" Nothing "org.freedesktop.DBus.Properties" Nothing [
    -- Error: method Get: Unsupported type Variant,
    -- Error: method GetAll: Unsupported type Dict String Variant,
    -- Error: method Set: Unsupported type Variant
  ]

-- Interface org.freedesktop.login1.Session
interface' "org.freedesktop.login1" Nothing "org.freedesktop.login1.Session" Nothing [
    "Terminate" =:: Return ''(),
    "Activate" =:: Return ''(),
    "Lock" =:: Return ''(),
    "Unlock" =:: Return ''(),
    "SetIdleHint" =:: ''Bool :-> Return ''(),
    "Kill" =:: ''String :-> ''Int32 :-> Return ''(),
    "TakeControl" =:: ''Bool :-> Return ''(),
    "ReleaseControl" =:: Return ''(),
    -- Error: method TakeDevice: Method TakeDevice has more than one out parameter,
    "ReleaseDevice" =:: ''Word32 :-> ''Word32 :-> Return ''(),
    "PauseDeviceComplete" =:: ''Word32 :-> ''Word32 :-> Return ''()
  ]

