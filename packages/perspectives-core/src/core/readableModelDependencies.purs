module Perspectives.ModelDependencies.Readable where

-- | This module contains Arc identifiers that are used in the PDR source code and should be available in Readable form.

chatAspect :: String
chatAspect = "model://perspectives.domains#System$Chat"

rootContext :: String
rootContext = "model://perspectives.domains#System$RootContext$External"

contextWithNotification :: String
contextWithNotification = "model://perspectives.domains#System$ContextWithNotification"

socialEnvironment :: String
socialEnvironment = "model://perspectives.domains#System$SocialEnvironment"

socialEnvironmentPersons :: String
socialEnvironmentPersons = "model://perspectives.domains#System$SocialEnvironment$Persons"

theWorld :: String
theWorld = "model://perspectives.domains#System$TheWorld"

perspectivesUsers :: String
perspectivesUsers = "model://perspectives.domains#System$TheWorld$PerspectivesUsers"
