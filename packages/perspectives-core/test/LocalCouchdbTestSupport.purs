module Test.LocalCouchdbTestSupport
  ( addLocalCouchdbCredentials
  ) where

import Prelude

import Effect.Aff (Aff)
import Perspectives.Persistence.Authentication (addCredentials)
import Test.PDRInstance.Types (PDRInstance, runInPDR)

addLocalCouchdbCredentials :: PDRInstance -> Aff Unit
addLocalCouchdbCredentials pdr = runInPDR pdr do
  addCredentials "https://perspectives.domains/" "alice" "alice"
  addCredentials "https://joopringelberg.nl/" "alice" "alice"
