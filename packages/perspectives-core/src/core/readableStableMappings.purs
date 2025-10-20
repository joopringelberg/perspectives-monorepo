module Perspectives.ModelDependencies.ReadableStableMappings where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

------------------------------------------------------------------------------------
-- MODEL CUIDS
------------------------------------------------------------------------------------
-- We moved from Readable identifiers to Stable ones in October 2025. In transitioning,
-- we have public resources in perspectives.domains that, under the new logic, should have their
-- names derived from model CUIDs. Instead, they are still derived from the Readable identifiers.
-- In the transition period, we need to be able to refer to these resources by their old names.

modelStableToReadable :: Object String
modelStableToReadable = fromFoldable
  [ (Tuple "model://perspectives.domains#tiodn6tcyc" "model://perspectives.domains#System")
  , (Tuple "model://perspectives.domains#xyfxpg3lzq" "model://perspectives.domains#CouchdbManagement")
  , (Tuple "model://perspectives.domains#bxxptg50jp" "model://perspectives.domains#BodiesWithAccounts")
  , (Tuple "model://perspectives.domains#xjrfkxrzyt" "model://perspectives.domains#SharedFileServices")
  , (Tuple "model://perspectives.domains#zjuzxbqpgc" "model://perspectives.domains#BrokerServices")
  , (Tuple "model://perspectives.domains#hkfgpmwt93" "model://perspectives.domains#HyperContext")
  , (Tuple "model://perspectives.domains#l75w588kuk" "model://perspectives.domains#Utilities")
  , (Tuple "model://perspectives.domains#nip6odtx4r" "model://perspectives.domains#Couchdb")
  , (Tuple "model://perspectives.domains#dcm0arlqnz" "model://perspectives.domains#Serialise")
  , (Tuple "model://perspectives.domains#s2gyoyohau" "model://perspectives.domains#Sensor")
  , (Tuple "model://perspectives.domains#salp36dvb9" "model://perspectives.domains#Parsing")
  , (Tuple "model://perspectives.domains#piln392sut" "model://perspectives.domains#Files")
  , (Tuple "model://perspectives.domains#m203lt2idk" "model://perspectives.domains#RabbitMQ")

  ]

modelReadableToStable :: Object String
modelReadableToStable = fromFoldable
  [ (Tuple "model://perspectives.domains#System" "model://perspectives.domains#tiodn6tcyc")
  , (Tuple "model://perspectives.domains#CouchdbManagement" "model://perspectives.domains#xyfxpg3lzq")
  , (Tuple "model://perspectives.domains#BodiesWithAccounts" "model://perspectives.domains#bxxptg50jp")
  , (Tuple "model://perspectives.domains#SharedFileServices" "model://perspectives.domains#xjrfkxrzyt")
  , (Tuple "model://perspectives.domains#BrokerServices" "model://perspectives.domains#zjuzxbqpgc")
  , (Tuple "model://perspectives.domains#HyperContext" "model://perspectives.domains#hkfgpmwt93")
  , (Tuple "model://perspectives.domains#Utilities" "model://perspectives.domains#l75w588kuk")
  , (Tuple "model://perspectives.domains#Couchdb" "model://perspectives.domains#nip6odtx4r")
  , (Tuple "model://perspectives.domains#Serialise" "model://perspectives.domains#dcm0arlqnz")
  , (Tuple "model://perspectives.domains#Sensor" "model://perspectives.domains#s2gyoyohau")
  , (Tuple "model://perspectives.domains#Parsing" "model://perspectives.domains#salp36dvb9")
  , (Tuple "model://perspectives.domains#Files" "model://perspectives.domains#piln392sut")
  , (Tuple "model://perspectives.domains#RabbitMQ" "model://perspectives.domains#m203lt2idk")

  ]

modelReadableToCuid :: Object String
modelReadableToCuid = fromFoldable
  [ (Tuple "System" "tiodn6tcyc")
  , (Tuple "CouchdbManagement" "xyfxpg3lzq")
  , (Tuple "BodiesWithAccounts" "bxxptg50jp")
  , (Tuple "SharedFileServices" "xjrfkxrzyt")
  , (Tuple "BrokerServices" "zjuzxbqpgc")
  , (Tuple "HyperContext" "hkfgpmwt93")

  , (Tuple "Utilities" "l75w588kuk")
  , (Tuple "Couchdb" "nip6odtx4r")
  , (Tuple "Serialise" "dcm0arlqnz")
  , (Tuple "Sensor" "s2gyoyohau")
  , (Tuple "Parsing" "salp36dvb9")
  , (Tuple "Files" "piln392sut")
  , (Tuple "RabbitMQ" "m203lt2idk")

  ]