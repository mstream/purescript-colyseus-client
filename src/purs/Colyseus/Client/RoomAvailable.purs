module Colyseus.Client.RoomAvailable (RoomAvailable, getClients, getMaxClients, getRoomId) where

import Data.Function.Uncurried (Fn1, runFn1)

getClients :: RoomAvailable -> Int
getClients = runFn1 getClientsImpl

getMaxClients :: RoomAvailable -> Int
getMaxClients = runFn1 getMaxClientsImpl

getRoomId :: RoomAvailable -> String
getRoomId = runFn1 getRoomIdImpl

foreign import data RoomAvailable :: Type

foreign import getClientsImpl :: Fn1 RoomAvailable Int
foreign import getMaxClientsImpl :: Fn1 RoomAvailable Int
foreign import getRoomIdImpl :: Fn1 RoomAvailable String

