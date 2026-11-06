-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-------------------------------------------------------------------------------
---- TRANSLATION
-------------------------------------------------------------------------------

module Perspectives.ModelTranslation where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.State (State) as ST
import Control.Monad.State (get, lift, modify, runStateT, execState)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array (catMaybes, concat, cons, filter, fold, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Interpolate (i)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, notNull, toMaybe, toNullable) as NULL
import Data.Set (toUnfoldable) as SET
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split, stripPrefix)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, message)
import Effect.Exception (Error, error)
import Foreign (Foreign, isArray, isNull, typeOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (filter) as OBJ
import Foreign.Object (Object, empty, filterKeys, fromFoldable, insert, isEmpty, keys, lookup, mapWithKey, singleton, toUnfoldable, union, values)
import IDBKeyVal (idbGet)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, TranslationTable(..), Translations(..))
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Data.EncodableMap (keys, lookup, values) as EM
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Fuzzysort (fuzzyLookup)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, domeinFileVersion, isExternalRole, startsWithSegments, typeUri2LocalName_, typeUri2ModelUri, typeUri2typeNameSpace_)
import Perspectives.ModelTranslation.Normalize (fromReadableModelTranslation, toReadableModelTranslation)
import Perspectives.ModelTranslation.Representation (ActionsPerStateTranslation(..), ActionsTranslation(..), ContextTranslation(..), ContextsTranslation(..), IndexedContextNameTranslation(..), MarkdownsTranslation(..), ModelTranslation(..), NotificationsTranslation(..), PropertiesTranslation(..), RoleTranslation(..), RolesTranslation(..), TitlesTranslation(..))
import Perspectives.PerspectivesState (getCurrentLanguage, getTranslationTable)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.Perspective (StateSpec, stateSpec2StateIdentifier)
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..), TableFormDef(..), TableFormOrWhenDef(..), TypeAheadFillerDef(..), TypeAheadFormDef(..), What(..), WhenDef(..), WhenTableFormDef(..), WhereTo(..), Who(..), WhoWhatWhereScreenDef(..))
import Perspectives.Representation.State (Notification(..), State(..), StateFulObject(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), IndexedContext(..), PropertyType(..), RoleType(..), roletype2string)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable, Stable)
import Perspectives.Sidecar.StableIdMapping (StableIdMapping)
import Purescript.YAML (load)
import Simple.JSON (read_, writeJSON)

-------------------------------------------------------------------------------
---- YAML FILE PARSE REPRESENTATION
---- In a YAML file we omit members that are empty in the ModelTranslation.
---- On reconstructing the Model Translation from the YAML, we should add them again.
-------------------------------------------------------------------------------
newtype ModelTranslation_ = ModelTranslation_
  { namespace :: NULL.Nullable String
  , contexts :: NULL.Nullable ContextsTranslation_
  , roles :: NULL.Nullable RolesTranslation_
  , version :: NULL.Nullable String
  }

newtype ContextTranslation_ = ContextTranslation_
  { translations :: Translations_
  , external :: NULL.Nullable RoleTranslation_
  , users :: NULL.Nullable RolesTranslation_
  , things :: NULL.Nullable RolesTranslation_
  , contextroles :: NULL.Nullable RolesTranslation_
  -- The keys are the local context names
  , contexts :: NULL.Nullable ContextsTranslation_
  -- The keys are generated numbers and are of no interest.
  -- The original texts are stored under the key 'orig'.
  -- These notifications are lexically embedded in context state transitions.
  , notifications :: NULL.Nullable Notifications_
  , indexedName :: NULL.Nullable IndexedContextNameTranslation_
  }

newtype IndexedContextNameTranslation_ = IndexedContextNameTranslation_
  { name :: Foreign
  , translations :: Translations_
  }

newtype ContextsTranslation_ = ContextsTranslation_ (Object ContextTranslation_)

newtype RolesTranslation_ = RolesTranslation_ (Object RoleTranslation_)

newtype RoleTranslation_ = RoleTranslation_
  { translations :: Translations_
  , properties :: NULL.Nullable PropertiesTranslation_
  , actions :: NULL.Nullable ActionsPerStateTranslation_
  , markdowns :: NULL.Nullable Markdowns_
  -- These notifications are lexically embedded in role state transitions.
  , notifications :: NULL.Nullable Notifications_
  , titles :: NULL.Nullable Titles_
  }

newtype PropertiesTranslation_ = PropertiesTranslation_ (Object { translations :: Translations_ })

-- Keys are state identifiers.
newtype ActionsPerStateTranslation_ = ActionsPerStateTranslation_ (Object ActionsTranslation_)

-- Keys are action identifiers as they occur in the model.
newtype ActionsTranslation_ = ActionsTranslation_ (Object { translations :: Translations_ })

-- Values are whatever the YAML parser produced: usually strings, but a modeller may write
-- an unquoted number or boolean, or leave a value empty (null).
newtype Translations_ = Translations_ (Object Foreign)

-- The keys are generated numbers and are of no interest.
-- The original texts are stored under the key 'orig'.
-- newtype Notifications_ = Notifications_ (Object Translations_)
-- eg:
-- notifications: { '0': { translations: { orig: 'You now have an account with CouchdbServer $1' } } }
newtype Notifications_ = Notifications_ (Object { translations :: Translations_ })

newtype Markdowns_ = Markdowns_ (Object { translations :: Translations_ })

newtype Titles_ = Titles_ (Object { translations :: Translations_ })

-- | True for a YAML mapping (a non-null, non-array JavaScript object).
isMapping :: Foreign -> Boolean
isMapping f = typeOf f == "object" && not (isNull f) && not (isArray f)

-- | The object itself if it is a mapping, the empty object otherwise (e.g. for an empty YAML block).
objectOrEmpty :: forall a. Object a -> Object a
objectOrEmpty obj = if isMapping (unsafeToForeign obj) then obj else empty

-- | Only the entries that are themselves mappings. Protects against empty or malformed entries in hand-edited files.
objectEntries :: forall a. Object a -> Object a
objectEntries obj = OBJ.filter (isMapping <<< unsafeToForeign) (objectOrEmpty obj)

-- | Interpret a YAML scalar as a string. An unquoted number or boolean is converted to its textual form.
scalarToString :: Foreign -> Maybe String
scalarToString f = case typeOf f of
  "string" -> Just (unsafeFromForeign f)
  "boolean" -> Just (if unsafeFromForeign f then "true" else "false")
  "number" ->
    let
      n = unsafeFromForeign f :: Number
    in
      Just $ maybe (show n) show (fromNumber n)
  _ -> Nothing

-------------------------------------------------------------------------------
---- CLASS REHYDRATE
-------------------------------------------------------------------------------

class (Translation modeltranslation) <= Rehydrate fromyaml modeltranslation | fromyaml -> modeltranslation where
  hydrate :: fromyaml -> modeltranslation

instance Rehydrate Translations_ Translations where
  hydrate (Translations_ obj) = Translations $ fromFoldable $ catMaybes
    ( (toUnfoldable (objectOrEmpty obj)) <#> \(Tuple language value) -> Tuple language <$> scalarToString value
    )

-- The keys are the index numbers we have generated. They are of no interest.
-- Entries without an original text cannot be matched against the model and are skipped.
instance Rehydrate Notifications_ NotificationsTranslation where
  hydrate (Notifications_ obj) = NotificationsTranslation $ hydrateOriginals identity obj

instance Rehydrate Titles_ TitlesTranslation where
  hydrate (Titles_ obj) = TitlesTranslation $ hydrateOriginals identity obj

instance Rehydrate Markdowns_ MarkdownsTranslation where
  hydrate (Markdowns_ obj) = MarkdownsTranslation $ hydrateOriginals normaliseFoldedMarkdown obj

-- | Re-index a block of numbered entries by their original text (the `orig` translation).
hydrateOriginals :: (String -> String) -> Object { translations :: Translations_ } -> Object Translations
hydrateOriginals normalise obj = fromFoldable $ catMaybes $ values (objectEntries obj) <#> \{ translations } ->
  let
    Translations t = hydrate translations
    t' = normalise <$> t
  in
    (\orig -> Tuple orig (Translations t')) <$> lookup "orig" t'

instance Rehydrate ActionsTranslation_ ActionsTranslation where
  -- Do away with the extra "translations" layer.
  hydrate (ActionsTranslation_ obj) = ActionsTranslation (objectEntries obj <#> _.translations >>> hydrate)

instance Rehydrate RoleTranslation_ RoleTranslation where
  hydrate (RoleTranslation_ { translations, properties, actions, markdowns, notifications, titles }) =
    RoleTranslation { translations: translations', properties: properties', actions: actions', notifications: notifications', markdowns: markdowns', titles: titles' }
    where
    properties' = case NULL.toMaybe properties of
      Nothing -> PropertiesTranslation empty
      Just (PropertiesTranslation_ obj) -> PropertiesTranslation (objectEntries obj <#> _.translations >>> hydrate)
    actions' = case NULL.toMaybe actions of
      Nothing -> ActionsPerStateTranslation empty
      -- Keys are the string representations of StateSpecs.
      -- Values are Object ActionsTranslation, where keys are local action names.
      Just (ActionsPerStateTranslation_ acts) -> ActionsPerStateTranslation (hydrate <$> objectEntries acts)
    translations' = hydrate translations
    notifications' = case NULL.toMaybe notifications of
      Nothing -> NotificationsTranslation empty
      Just n@(Notifications_ _) -> hydrate n
    markdowns' = case NULL.toMaybe markdowns of
      Nothing -> MarkdownsTranslation empty
      Just m@(Markdowns_ _) -> hydrate m
    titles' = case NULL.toMaybe titles of
      Nothing -> TitlesTranslation empty
      Just t@(Titles_ _) -> hydrate t

instance Rehydrate ContextTranslation_ ContextTranslation where
  hydrate (ContextTranslation_ { translations, external, users, things, contextroles, contexts, notifications, indexedName }) =
    let
      translations' = hydrate translations
      external' = case NULL.toMaybe external of
        Nothing -> RoleTranslation
          { translations: Translations empty
          , properties: PropertiesTranslation empty
          , actions: ActionsPerStateTranslation empty
          , notifications: NotificationsTranslation empty
          , markdowns: MarkdownsTranslation empty
          , titles: TitlesTranslation empty
          }
        Just e@(RoleTranslation_ _) -> (hydrate e)
      users' = case NULL.toMaybe users of
        Nothing -> RolesTranslation empty
        Just (RolesTranslation_ u) -> RolesTranslation (map hydrate (objectEntries u))
      things' = case NULL.toMaybe things of
        Nothing -> RolesTranslation empty
        Just (RolesTranslation_ t) -> RolesTranslation (map hydrate (objectEntries t))
      contextroles' = case NULL.toMaybe contextroles of
        Nothing -> RolesTranslation empty
        Just (RolesTranslation_ t) -> RolesTranslation (map hydrate (objectEntries t))
      contexts' = case NULL.toMaybe contexts of
        Nothing -> ContextsTranslation empty
        Just (ContextsTranslation_ t) -> ContextsTranslation (map hydrate (objectEntries t))
      notifications' = case NULL.toMaybe notifications of
        Nothing -> NotificationsTranslation empty
        Just n@(Notifications_ _) -> hydrate n
      indexedName' = case NULL.toMaybe indexedName of
        Nothing -> Nothing
        Just (IndexedContextNameTranslation_ i) -> scalarToString i.name <#> \name -> IndexedContextNameTranslation { name: IndexedContext name, translations: hydrate i.translations }
    in
      ContextTranslation
        { translations: translations'
        , external: external'
        , notifications: notifications'
        , users: users'
        , things: things'
        , contextroles: contextroles'
        , contexts: contexts'
        , indexedName: indexedName'
        }

instance Rehydrate ModelTranslation_ ModelTranslation where
  hydrate (ModelTranslation_ { namespace, contexts, roles, version }) =
    let
      namespace' = case NULL.toMaybe namespace of
        Nothing -> "MissingNamespace"
        Just n -> n
      version' = case NULL.toMaybe version of
        Nothing -> "MissingVersion"
        Just v -> v
      contexts' = case NULL.toMaybe contexts of
        Nothing -> ContextsTranslation empty
        Just (ContextsTranslation_ t) -> ContextsTranslation (map hydrate (objectEntries t))
      roles' = case NULL.toMaybe roles of
        Nothing -> RolesTranslation empty
        Just (RolesTranslation_ t) -> RolesTranslation (map hydrate (objectEntries t))
    in
      ModelTranslation { namespace: namespace', version: version', contexts: Just contexts', roles: Just roles' }

-------------------------------------------------------------------------------
---- GENERATE FIRST TRANSLATION FROM DOMEINFILE
---- Generate a structure that can be declared to be JSON.
---- This is the canonical form that we generate from the YAML.
---- From this, generate the first YAML file;
---- From this, generate the key-value translation table.
-------------------------------------------------------------------------------
generateFirstTranslation :: DomeinFile Stable -> ModelTranslation
generateFirstTranslation (DomeinFile dr) = flip runReader dr do
  -- Nested contexts are translated as part of their enclosing context (see translateContext).
  -- The domain context itself is not part of the translation; its roles are listed under `roles`.
  toplevelContexts <- pure $ filterKeys (\c -> c /= unwrap dr.id && isDefinedAtTopLevel c) dr.contexts
  contexts <-
    if isEmpty toplevelContexts then pure Nothing
    else Just <<< ContextsTranslation <$> (for toplevelContexts translateContext)
  eroles <- for (filterKeys isDefinedAtTopLevel dr.enumeratedRoles)
    (translateRole <<< E)
  croles <- for (filterKeys isDefinedAtTopLevel dr.calculatedRoles)
    (translateRole <<< C)
  allRoles <- pure $ fromFoldable ((toUnfoldable eroles <> toUnfoldable croles) :: Array (Tuple String RoleTranslation))
  roles <- if isEmpty allRoles then pure Nothing else pure $ Just $ RolesTranslation allRoles
  version <- pure $ unsafePartial fromJust $ domeinFileVersion dr._id
  pure $ ModelTranslation { namespace: unwrap dr.id, contexts, roles, version }
  where
  -- Excludes the external role of the domain context, but not roles whose local name merely ends in "External".
  isDefinedAtTopLevel :: String -> Boolean
  isDefinedAtTopLevel s = unwrap dr.id == typeUri2typeNameSpace_ s && s /= buitenRol (unwrap dr.id)

translateContext :: Context -> Reader (DomeinFileRecord Stable) ContextTranslation
translateContext (Context { id: contextId, gebruikerRol, contextRol, rolInContext, nestedContexts, indexedContext }) = do
  { enumeratedRoles } <- ask
  external <- unsafePartial case lookup (buitenRol $ unwrap contextId) enumeratedRoles of
    Just e -> translateRole (E e)
  users <- translateRoles gebruikerRol
  things <- translateRoles rolInContext
  contextroles <- translateRoles contextRol
  contexts <- ContextsTranslation <<< fromFoldable <$> for
    (filter (\ct -> (unwrap ct) `startsWithSegments` (unwrap contextId)) nestedContexts)
    \ct -> (lookupContextType ct >>= translateContext >>= pure <<< Tuple (unwrap ct))
  notifications <- do
    { states } <- ask
    pure $ NotificationsTranslation
      ( execWriter $ for_ states
          ( \(State { stateFulObject, notifyOnEntry, notifyOnExit }) -> case stateFulObject of
              Cnt c | c == contextId -> translateNotifications notifyOnEntry notifyOnExit
              _ -> pure unit
          )
      )
  -- If this context type is indexed, add the indexed name to the ContextTranslation.
  pure $ ContextTranslation
    { translations: Translations empty
    , external
    , notifications
    , users
    , things
    , contextroles
    , contexts
    , indexedName: (\(ContextInstance s) -> IndexedContextNameTranslation { name: IndexedContext s, translations: Translations empty }) <$> indexedContext
    }
  where
  -- Writes singletons with the original sentence and empty translations

  translateRoles :: Array RoleType -> Reader (DomeinFileRecord Stable) RolesTranslation
  translateRoles roles = RolesTranslation <$> fromFoldable <$> for
    (filter (\rt -> (roletype2string rt) `startsWithSegments` (unwrap contextId)) roles)
    (\rt -> (lookupRoleType rt >>= translateRole >>= pure <<< Tuple (roletype2string rt)))

-- | Notifications on entry and on exit of a state are both compiled with translationOf at runtime,
-- | so both must be offered for translation.
translateNotifications :: EncodableMap RoleType Notification -> EncodableMap RoleType Notification -> Writer (Object Translations) Unit
translateNotifications onEntry onExit = do
  for_ (EM.values onEntry) translateNotification
  for_ (EM.values onExit) translateNotification

-- | The same sentence may occur several times in a model; such occurrences share a single entry
-- | (and thus a single translation), as the Writer monoid merges equal keys.
translateNotification :: Notification -> Writer (Object Translations) Unit
translateNotification (ContextNotification { sentence }) = tell $ singleton (_.sentence $ unwrap sentence) (Translations empty)
translateNotification (RoleNotification { sentence }) = tell $ singleton (_.sentence $ unwrap sentence) (Translations empty)

-- Aspect roles that have been added as is will turn up here, but should not be translated.
lookupRoleType :: RoleType -> Reader (DomeinFileRecord Stable) Role
lookupRoleType rt = do
  { enumeratedRoles, calculatedRoles } <- ask
  case lookup (roletype2string rt) enumeratedRoles of
    Nothing -> pure $ C $ unsafePartial fromJust $ lookup (roletype2string rt) calculatedRoles
    Just r -> pure $ E r

lookupContextType :: ContextType -> Reader (DomeinFileRecord Stable) Context
lookupContextType (ContextType ct) = do
  { contexts } <- ask
  unsafePartial case lookup ct contexts of
    Just c -> pure c

translateRole :: Role -> Reader (DomeinFileRecord Stable) RoleTranslation
translateRole (E (EnumeratedRole rec)) = do
  -- The translations of all properties.
  (properties :: PropertiesTranslation) <- do
    (ps :: Array (Tuple String Translations)) <- (for rec.properties translatePropertyType)
    pure $ PropertiesTranslation $ fromFoldable ps
  contextActions <- translateActions rec.context rec.actions
  (roleActions :: Array ActionsPerStateTranslation) <- for rec.perspectives (translateActions rec.context <<< _.actions <<< unwrap)

  notifications <- do
    { states } <- ask
    pure $ NotificationsTranslation
      ( execWriter $ for_ states
          ( \(State { stateFulObject, notifyOnEntry, notifyOnExit }) -> case stateFulObject of
              Orole r | r == rec.id -> translateNotifications notifyOnEntry notifyOnExit
              Srole r | r == rec.id -> translateNotifications notifyOnEntry notifyOnExit
              _ -> pure unit
          )
      )
  markdowns <- markDownsInScreen (ScreenKey rec.context (ENR rec.id))
  titles <- titlesInScreen (ScreenKey rec.context (ENR rec.id))
  pure $ RoleTranslation
    { translations: Translations empty
    , properties
    , actions: contextActions <> (fold roleActions)
    , notifications
    , markdowns
    , titles
    }
translateRole (C (CalculatedRole rec)) = do
  contextactions <- translateActions rec.context rec.actions
  (roleActions :: Array ActionsPerStateTranslation) <- for rec.perspectives (translateActions rec.context <<< _.actions <<< unwrap)
  markdowns <- markDownsInScreen (ScreenKey rec.context (CR rec.id))
  titles <- titlesInScreen (ScreenKey rec.context (CR rec.id))
  pure $ RoleTranslation
    { translations: Translations empty
    , properties: PropertiesTranslation empty
    , actions: contextactions <> (fold roleActions)
    , notifications: NotificationsTranslation empty
    , markdowns
    , titles
    }

markDownsInScreen :: ScreenKey -> Reader (DomeinFileRecord Stable) MarkdownsTranslation
markDownsInScreen screenKey = do
  -- screens holds an EncodableMap where the keys are ScreenKey instances: ScreenKey ContextType RoleType.
  -- The ContextType and RoleType are the lexical context and subject for the definition of the screen.
  -- A screen definition is lexically embedded in a user role.
  -- So if we take the lexical context of the role and combine it with its RoleType, we have the key to obtain all relevant screens. 
  { screens } <- ask
  case EM.lookup screenKey screens of
    Nothing -> pure $ MarkdownsTranslation empty
    Just s@(ScreenDefinition _) ->
      let
        markdowns = (collectMarkdowns s)
        translations = markdowns <#> \markdown -> Tuple markdown (Translations $ singleton "orig" markdown)
      in
        pure $ MarkdownsTranslation $ fromFoldable translations

titlesInScreen :: ScreenKey -> Reader (DomeinFileRecord Stable) TitlesTranslation
titlesInScreen screenKey = do
  -- screens holds an EncodableMap where the keys are ScreenKey instances: ScreenKey ContextType RoleType.
  -- The ContextType and RoleType are the lexical context and subject for the definition of the screen.
  -- A screen definition is lexically embedded in a user role.
  -- So if we take the lexical context of the role and combine it with its RoleType, we have the key to obtain all relevant screens. 
  { screens } <- ask
  case EM.lookup screenKey screens of
    Nothing -> pure $ TitlesTranslation empty
    Just s@(ScreenDefinition _) ->
      let
        titles = (collectTitles s)
        translations = titles <#> \title -> Tuple title (Translations $ singleton "orig" title)
      in
        pure $ TitlesTranslation $ fromFoldable translations

translateActions :: ContextType -> EncodableMap StateSpec (Object Action) -> Reader (DomeinFileRecord Stable) ActionsPerStateTranslation
translateActions (ContextType cType) actions = ActionsPerStateTranslation <<< fromFoldable <$>
  ( for (SET.toUnfoldable $ EM.keys actions :: Array StateSpec) \state -> pure $ Tuple
      (unwrap $ stateSpec2StateIdentifier state)
      ( ActionsTranslation
          let
            qualifiedActionNames = keys (unsafePartial fromJust $ EM.lookup state actions)
          in
            fromFoldable $ flip Tuple (Translations empty) <$> qualifiedActionNames
      )
  )

-- The translations of a single property.
translatePropertyType :: PropertyType -> Reader (DomeinFileRecord Stable) (Tuple String Translations)
translatePropertyType (CP (CalculatedPropertyType p)) = pure $ Tuple p (Translations empty)
translatePropertyType (ENP (EnumeratedPropertyType p)) = pure $ Tuple p (Translations empty)

-------------------------------------------------------------------------------
---- CLASS TRANSLATION 
-------------------------------------------------------------------------------
class Translation a where
  qualify :: String -> a -> a
  writeKeys :: a -> ST.State (Object Translations) Unit
  writeYaml :: String -> a -> Writer String Unit
  addTranslations :: TranslationTable -> a -> a

instance Translation Translations where
  qualify _ a = a
  writeKeys _ = pure unit
  writeYaml indent trans = writeTranslationsYaml quoteForYaml indent trans
  addTranslations _ t = t

-- | Write a `translations` block, encoding each value with the given function.
-- | Languages that have no translation yet receive a "MISSING <lang>" placeholder.
writeTranslationsYaml :: (String -> String) -> String -> Translations -> Writer String Unit
writeTranslationsYaml encode indent trans = do
  tell (i indent "translations" colonNl)
  translations' <- case addDefaultLanguages trans of
    (Translations translations) -> pure $ object2array translations
  void $ for translations'
    \(Tuple lang translation) -> tell (i indent tab lang colonSpace (encode translation) nl)

-- | Encode a string as a YAML double-quoted scalar. JSON string syntax is a subset of YAML's
-- | double-quoted style, so this preserves every character (':', '#', '{', quotes, backslashes,
-- | leading '-' or '*', strings that look like numbers or booleans, etc.).
quoteForYaml :: String -> String
quoteForYaml = writeJSON

instance Translation NotificationsTranslation where
  qualify _ n = n
  writeKeys (NotificationsTranslation notifications) = void $ modify \allTranslations -> allTranslations `union` notifications
  writeYaml indent (NotificationsTranslation notifications) =
    if isEmpty notifications then pure unit
    else do
      tell (i indent "notifications" colonNl)
      void $ runStateT
        ( forWithIndex notifications
            \original (Translations translations) -> do
              nr <- get
              lift $ tell (i indent tab nr colonNl)
              lift $ writeYaml (indent <> tab <> tab) (addDefaultLanguages (Translations $ insert "orig" original translations))
              modify ((+) 1)
        )
        0
  addTranslations (TranslationTable table) (NotificationsTranslation notifications) = NotificationsTranslation $
    flip mapWithKey notifications \original translations -> case fuzzyLookup original table of
      Nothing -> translations
      Just ts -> ts

instance Translation TitlesTranslation where
  qualify _ n = n
  writeKeys (TitlesTranslation titles) = void $ modify \allTranslations -> allTranslations `union` titles
  writeYaml indent (TitlesTranslation titles) =
    if isEmpty titles then pure unit
    else do
      tell (i indent "titles" colonNl)
      void $ runStateT
        ( forWithIndex titles
            \original (Translations translations) -> do
              nr <- get
              lift $ tell (i indent tab nr colonNl)
              lift $ writeYaml (indent <> tab <> tab) (addDefaultLanguages (Translations $ insert "orig" original translations))
              modify ((+) 1)
        )
        0
  addTranslations (TranslationTable table) (TitlesTranslation titles) = TitlesTranslation $
    flip mapWithKey titles \original translations -> case lookup original table of
      Nothing -> translations
      Just ts -> ts

instance Translation MarkdownsTranslation where
  qualify _ n = n
  writeKeys (MarkdownsTranslation markdowns) = void $ modify \allTranslations -> allTranslations `union` markdowns
  writeYaml indent (MarkdownsTranslation markdowns) =
    if isEmpty markdowns then pure unit
    else do
      tell (i indent "markdowns" colonNl)
      void $ runStateT
        ( forWithIndex markdowns
            \original (Translations translations) -> do
              nr <- get
              lift $ tell (i indent tab nr colonNl)
              lift $ writeTranslationsYaml (escapeMarkdownForYaml (indent <> tab <> tab <> tab <> tab)) (indent <> tab <> tab) (Translations $ insert "orig" original translations)
              modify ((+) 1)
        )
        0
  addTranslations (TranslationTable table) (MarkdownsTranslation markdowns) = MarkdownsTranslation $
    flip mapWithKey markdowns \original translations -> case fuzzyLookup original table of
      Nothing -> translations
      Just ts -> ts

addDefaultLanguages :: Translations -> Translations
addDefaultLanguages trans = addLanguage "en" $ addLanguage "nl" trans
  where
  addLanguage :: String -> Translations -> Translations
  addLanguage lang t@(Translations translations) =
    if isJust $ lookup lang translations then t
    else Translations $ insert lang ("MISSING " <> lang) translations

-- | Markdown is parsed from ARC to a string containing newlines (\n). The ARC indentation has been removed.
-- | To keep multi-line texts readable for the modeller, we write them as a YAML double-quoted scalar
-- | that spans several lines:
-- |   * each newline is written as the escape sequence `\n`, followed by a real line break and indentation;
-- |   * backslashes and double quotes are escaped;
-- |   * a space or tab at the start of a line is escaped (`\ ` or `\t`), as YAML would otherwise strip it.
-- | On reading, YAML folds each real line break into a single space. The parser removes that space again
-- | (see `normaliseFoldedMarkdown`), so that the text read equals the original text exactly.
-- | E.g. "A\n  * b" is written as:
-- |   "A\n
-- |   \  * b"
escapeMarkdownForYaml :: String -> String -> String
escapeMarkdownForYaml indent' s = "\"" <> joinWith ("\\n\n" <> indent') (escapeLine <$> split (Pattern "\n") s) <> "\""
  where
  escapeLine :: String -> String
  escapeLine line =
    let
      escaped = replaceAll (Pattern "\"") (Replacement "\\\"") (replaceAll (Pattern "\\") (Replacement "\\\\") line)
    in
      case stripPrefix (Pattern " ") escaped of
        Just rest -> "\\ " <> rest
        Nothing -> case stripPrefix (Pattern "\t") escaped of
          Just rest -> "\\t" <> rest
          Nothing -> escaped

-- | Undo the space that YAML inserts when folding the line breaks written by `escapeMarkdownForYaml`.
normaliseFoldedMarkdown :: String -> String
normaliseFoldedMarkdown = replaceAll (Pattern "\n ") (Replacement "\n")

instance Translation PropertiesTranslation where
  qualify namespace (PropertiesTranslation propsAndTranslations) = PropertiesTranslation $ qualifyObjectKeys namespace propsAndTranslations
  writeKeys (PropertiesTranslation propTranslations) = void $ modify \allTranslations -> allTranslations `union` propTranslations
  writeYaml indent (PropertiesTranslation properties) =
    if isEmpty properties then pure unit
    else do
      tell (i indent "properties" colonNl)
      void $ forWithIndex properties
        \propName translations -> do
          tell (i (indent <> tab) (typeUri2LocalName_ propName) colonNl)
          writeYaml (indent <> tab <> tab) translations
  addTranslations (TranslationTable table) (PropertiesTranslation obj) = PropertiesTranslation $
    flip mapWithKey obj \propName translations -> case lookup propName table of
      Nothing -> translations
      Just ts -> ts

instance Translation ActionsTranslation where
  -- keys are action names, namespace SHOULD be state name.
  qualify namespace (ActionsTranslation obj) = ActionsTranslation $ qualifyObjectKeys namespace obj
  writeKeys (ActionsTranslation actionTranslations) = void $ modify \allTranslations -> allTranslations `union` actionTranslations
  writeYaml indent (ActionsTranslation actions) = void $ forWithIndex actions
    \actionName translations -> do
      tell (i indent (typeUri2LocalName_ actionName) colonNl)
      writeYaml (indent <> tab) translations
  addTranslations (TranslationTable table) (ActionsTranslation a) = ActionsTranslation $
    flip mapWithKey a \actionName translations -> case lookup actionName table of
      Nothing -> translations
      Just ts -> ts

instance Translation ActionsPerStateTranslation where
  -- qualify namespace (ActionsPerStateTranslation obj) =
  --   ActionsPerStateTranslation $ singleton namespace
  --     ( ActionsTranslation $ fromFoldable $ join
  --         ( ((toUnfoldable obj) :: Array (Tuple String ActionsTranslation)) <#>
  --             \(Tuple key val) -> toUnfoldable (unwrap (qualify namespace val))
  --         )
  --     )
  qualify namespace (ActionsPerStateTranslation obj) =
    ActionsPerStateTranslation $ fromFoldable
      ( ((toUnfoldable obj) :: Array (Tuple String ActionsTranslation)) <#>
          \(Tuple state val) -> Tuple state (qualify namespace val)
      )

  -- ActionsPerStateTranslation $ fromFoldable 
  --   (((toUnfoldable obj) :: Array (Tuple String ActionsTranslation)) <#> 
  --     \(Tuple key val) -> Tuple namespace' (qualify namespace' val))

  -- Keys are the string representations of StateSpecs. Combine each key with the local action names 
  -- to generate a pseudo qualified action name.
  -- Do so by qualifying the keys of the (Object ActionsTranslation) with the state.
  writeKeys (ActionsPerStateTranslation actiontranslations) = void $ for
    actiontranslations
    writeKeys
  writeYaml indent (ActionsPerStateTranslation actions) = case actions of
    none | isEmpty none -> pure unit
    actions' -> do
      tell (i indent "actions" colonNl)
      void $ forWithIndex actions'
        \stateName acts -> do
          tell (i (indent <> tab) stateName colonNl) -- Pas typeUri2LocalName_ niet toe. We verliezen dan informatie die we nodig hebben om terug te vertalen naar een ModelTranslation.
          writeYaml (indent <> tab <> tab) acts
  addTranslations table (ActionsPerStateTranslation a) = ActionsPerStateTranslation $ addTranslations table <$> a

instance Translation RolesTranslation where
  qualify namespace (RolesTranslation obj) =
    RolesTranslation $ fromFoldable
      ( ( toUnfoldable obj <#> \(Tuple unqualifiedName role) ->
            let
              qualifiedName = namespace <> "$" <> unqualifiedName
            in
              Tuple qualifiedName (qualify qualifiedName role)
        ) :: Array (Tuple String RoleTranslation)
      )
  writeKeys (RolesTranslation roleTranslations) = forWithIndex_ roleTranslations
    \roleName (RoleTranslation { translations, properties, actions, notifications, markdowns, titles }) -> do
      -- tell (singleton roleName translations)
      void $ modify \allTranslations -> insert roleName translations allTranslations

      writeKeys properties
      writeKeys actions
      writeKeys notifications
      writeKeys markdowns
      writeKeys titles
  writeYaml indent (RolesTranslation userRoles) = void $ for (object2array userRoles)
    \(Tuple roleName roleTranslation) -> do
      tell (i indent (typeUri2LocalName_ roleName) colonNl)
      writeYaml (indent <> tab) roleTranslation
  addTranslations tbl@(TranslationTable table) (RolesTranslation a) = RolesTranslation $ flip mapWithIndex a
    \roleName r@(RoleTranslation rec) -> case lookup roleName table of
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (RoleTranslation rec { translations = ts })

instance Translation RoleTranslation where
  qualify namespace (RoleTranslation { translations, properties, actions, notifications, markdowns, titles }) =
    let
      properties' = qualify namespace properties
      actions' = qualify namespace actions
    in
      RoleTranslation { translations, properties: properties', actions: actions', notifications, markdowns, titles }
  writeKeys _ = pure unit
  writeYaml indent (RoleTranslation { translations, properties, actions, notifications, markdowns, titles }) = do
    writeYaml indent translations
    writeYaml indent notifications
    writeYaml indent properties
    writeYaml indent actions
    writeYaml indent markdowns
    writeYaml indent titles
  addTranslations table (RoleTranslation { translations, properties, actions, notifications, markdowns, titles }) = RoleTranslation $
    { translations
    , properties: addTranslations table properties
    , actions: addTranslations table actions
    , notifications: addTranslations table notifications
    , markdowns: addTranslations table markdowns
    , titles: addTranslations table titles
    }

instance Translation ContextTranslation where
  qualify namespace (ContextTranslation { translations, external: ext, users, things, contextroles, contexts, notifications, indexedName }) =
    let
      external = qualify (namespace <> "$External") ext
      users' = qualify namespace users
      things' = qualify namespace things
      contextroles' = qualify namespace contextroles
      contexts' = qualify namespace contexts
      indexedName' = qualify namespace <$> indexedName
    in
      ContextTranslation { translations, external, users: users', things: things', contextroles: contextroles', contexts: contexts', notifications, indexedName: indexedName' }
  writeKeys _ = pure unit
  writeYaml indent (ContextTranslation { translations, external, users, things, contextroles, contexts: ctxts, notifications, indexedName }) = do
    writeYaml indent translations
    case indexedName of
      Nothing -> pure unit
      Just i -> writeYaml indent i
    -- The translation of the external role itself is that of the context, so we do not write it.
    case external of
      RoleTranslation { properties, actions, notifications: enotes, markdowns, titles } -> case properties, actions, enotes, markdowns, titles of
        PropertiesTranslation properties', ActionsPerStateTranslation actions', NotificationsTranslation notifications', MarkdownsTranslation markdowns', TitlesTranslation titles' ->
          if isEmpty properties' && isEmpty actions' && isEmpty notifications' && isEmpty markdowns' && isEmpty titles' then pure unit
          else do
            tell (i indent "external" colonNl)
            writeYaml (indent <> tab) enotes
            writeYaml (indent <> tab) properties
            writeYaml (indent <> tab) actions
            writeYaml (indent <> tab) markdowns
            writeYaml (indent <> tab) titles
    writeYaml indent notifications
    case users of
      RolesTranslation users' ->
        if isEmpty users' then pure unit
        else do
          tell (i indent "users" colonNl)
          writeYaml (indent <> tab) users
    case things of
      RolesTranslation things' ->
        if isEmpty things' then pure unit
        else do
          tell (i indent "things" colonNl)
          writeYaml (indent <> tab) things
    case contextroles of
      RolesTranslation contextroles' ->
        if isEmpty contextroles' then pure unit
        else do
          tell (i indent "contextroles" colonNl)
          writeYaml (indent <> tab) contextroles
    case ctxts of
      ContextsTranslation ctxts' ->
        if isEmpty ctxts' then pure unit
        else do
          tell (i indent "contexts" colonNl)
          writeYaml (indent <> tab) ctxts
  addTranslations table (ContextTranslation { translations, external, users, things, contextroles, contexts: ctxts, notifications, indexedName }) = ContextTranslation
    { translations
    , external: addTranslations table external
    , users: addTranslations table users
    , things: addTranslations table things
    , contextroles: addTranslations table contextroles
    , contexts: addTranslations table ctxts
    , notifications: addTranslations table notifications
    , indexedName: addTranslations table <$> indexedName
    }

instance Translation IndexedContextNameTranslation where
  qualify namespace i = i
  writeKeys (IndexedContextNameTranslation { name, translations }) = void $ modify \allTranslations -> allTranslations `union` singleton (unwrap name) translations
  writeYaml indent (IndexedContextNameTranslation { name, translations }) = do
    tell (i indent "indexedName" colonNl)
    tell (i (indent <> tab) "name" colonSpace (unwrap name) nl)
    writeYaml (indent <> tab) translations
  addTranslations (TranslationTable table) (IndexedContextNameTranslation { name, translations }) = IndexedContextNameTranslation
    { name
    , translations: case lookup (unwrap name) table of
        Nothing -> translations
        Just ts -> ts
    }

instance Translation ContextsTranslation where
  qualify namespace (ContextsTranslation obj) =
    ContextsTranslation $ fromFoldable
      ( ( toUnfoldable obj <#> \(Tuple unqualifiedName ctxt) ->
            let
              qualifiedName = namespace <> "$" <> unqualifiedName
            in
              Tuple qualifiedName (qualify qualifiedName ctxt)
        ) :: Array (Tuple String ContextTranslation)
      )
  writeKeys (ContextsTranslation contextTranslations) = forWithIndex_ contextTranslations
    \contextName (ContextTranslation { translations, external, users, things, contextroles, contexts, notifications, indexedName }) -> do
      void $ modify \allTranslations -> insert contextName translations allTranslations

      -- We have to handle the external role here, because we must construct its name from the contextName.
      RoleTranslation { translations: etranslations, properties: eproperties, actions: eactions, notifications: enotifications, markdowns: emarkdowns, titles: etitles } <- pure external
      void $ modify \allTranslations -> insert (contextName <> "$External") etranslations allTranslations
      writeKeys eproperties
      writeKeys eactions
      writeKeys enotifications
      writeKeys emarkdowns
      writeKeys etitles

      writeKeys users
      writeKeys things
      writeKeys contextroles
      writeKeys contexts
      writeKeys notifications
      for indexedName writeKeys
  writeYaml indent (ContextsTranslation ctxts) = void $ for (object2array ctxts)
    \(Tuple contextName contextTranslation) -> do
      tell (i indent (typeUri2LocalName_ contextName) colonNl)
      writeYaml (indent <> tab) contextTranslation
  addTranslations tbl@(TranslationTable table) (ContextsTranslation a) = ContextsTranslation $ flip mapWithIndex a
    \contextName r@(ContextTranslation rec) -> case lookup contextName table of
      Nothing -> addTranslations tbl r
      Just ts -> addTranslations tbl (ContextTranslation rec { translations = ts })

instance Translation ModelTranslation where
  qualify namespace (ModelTranslation { contexts, roles, version }) =
    let
      contexts' = (qualify namespace) <$> contexts
      roles' = (qualify namespace) <$> roles
    in
      ModelTranslation { namespace, contexts: contexts', roles: roles', version }
  writeKeys (ModelTranslation { contexts, roles }) = do
    case contexts of
      Just cs -> writeKeys cs
      Nothing -> pure unit
    case roles of
      Just rs -> writeKeys rs
      Nothing -> pure unit
  writeYaml indent (ModelTranslation { namespace, contexts, roles, version }) = do
    tell (namespace <> colonNl)
    tell (i tab "version" colonSpace "\"" version "\"" nl)
    for_ contexts \contexts'@(ContextsTranslation cs) -> unless (isEmpty cs) do
      tell (i tab "contexts" colonNl)
      writeYaml (tab <> tab) contexts'
    for_ roles \roles'@(RolesTranslation rs) -> unless (isEmpty rs) do
      tell (i tab "roles" colonNl)
      writeYaml (tab <> tab) roles'
  addTranslations table (ModelTranslation { namespace, contexts, roles, version }) = ModelTranslation
    { namespace
    , contexts: addTranslations table <$> contexts
    , roles: addTranslations table <$> roles
    , version
    }

qualifyObjectKeys :: forall a. Translation a => String -> Object a -> Object a
qualifyObjectKeys namespace obj = fromFoldable
  ( ((toUnfoldable obj) :: Array (Tuple String a)) <#>
      \(Tuple key val) -> Tuple (i namespace "$" key) (qualify (i namespace "$" key) val)
  )

object2array :: forall a. Object a -> Array (Tuple String a)
object2array = toUnfoldable

-------------------------------------------------------------------------------
---- GENERATE YAML FROM TRANSLATION
-------------------------------------------------------------------------------
tab :: String
tab = "  "

colonSpace :: String
colonSpace = ": "

colonNl :: String
colonNl = ":\n"

doubleQuote :: String
doubleQuote = "\""

nl :: String
nl = "\n"

writeTranslationYaml :: ModelTranslation -> String
writeTranslationYaml modeltranslation = execWriter (writeYaml "" modeltranslation)

-- | Produce a YAML representation with human-readable (modeller) identifiers instead of
-- | stable CUID composed identifiers.
-- | Use this when exporting a translation file for editing by a modeller.
-- | Pass the same StableIdMapping that was used to normalize (readable -> stable)
-- | so we can invert back (stable -> readable) deterministically. If a reverse
-- | lookup fails for a key, the stable id key is left as-is.
writeReadableTranslationYaml :: StableIdMapping -> ModelTranslation -> MonadPerspectives String
writeReadableTranslationYaml mapping mt = do
  readable <- toReadableModelTranslation mt
  pure $ execWriter (writeYaml "" readable)

-------------------------------------------------------------------------------
---- QUALIFY NAMES IN TRANSLATION
-------------------------------------------------------------------------------
qualifyModelTranslation :: ModelTranslation -> ModelTranslation
qualifyModelTranslation m@(ModelTranslation { namespace }) = qualify namespace m

-------------------------------------------------------------------------------
---- CONSTRUCT TRANSLATIONTABLE FROM TRANSLATION
-------------------------------------------------------------------------------
-- Each key is the string representation of a type (ContextType, EnumeratedRoleType,
-- CalculatedRoleType, EnumeratedPropertyType, CalculatedPropertyType) or of the combination 
-- of a (user) EnumeratedRoleType or CalculatedRoleType and an Action name (separated by '$').

generateTranslationTable :: ModelTranslation -> TranslationTable
generateTranslationTable modeltranslation = TranslationTable $ execState (writeKeys modeltranslation) empty

emptyTranslationTable :: TranslationTable
emptyTranslationTable = TranslationTable empty

-------------------------------------------------------------------------------
---- PARSE YAML TO TRANSLATION
-------------------------------------------------------------------------------
-- | Parse a YAML translation file into a ModelTranslation whose keys are local names
-- | (qualify them with `qualifyModelTranslation` or `parseTranslation_pass2`).
-- | The file must be a mapping with a single key (the model namespace) whose value holds
-- | `version` and optionally `contexts` and `roles`.
parseTranslation_pass1 :: String -> Effect (Either Error ModelTranslation)
parseTranslation_pass1 source = do
  parseResult :: Either Error Foreign <- load source
  pure case parseResult of
    Left e -> Left (error $ "Could not parse the translation file: " <> message e)
    Right yaml -> case toModelTranslation_ yaml of
      Nothing -> Left (error "Could not parse the translation file: expected a mapping from the model namespace to its version, contexts and roles.")
      Just mt -> Right (hydrate mt)
  where
  toModelTranslation_ :: Foreign -> Maybe ModelTranslation_
  toModelTranslation_ yaml =
    if isMapping yaml then do
      let (topLevel :: Object Foreign) = unsafeFromForeign yaml
      namespace <- head $ keys topLevel
      body <- lookup namespace topLevel
      if isMapping body then
        let
          (fields :: Object Foreign) = unsafeFromForeign body
        in
          Just $ ModelTranslation_
            { namespace: NULL.notNull namespace
            , version: NULL.toNullable (lookup "version" fields >>= scalarToString)
            , contexts: NULL.toNullable (unsafeFromForeign <$> lookup "contexts" fields)
            , roles: NULL.toNullable (unsafeFromForeign <$> lookup "roles" fields)
            }
      else Nothing
    else Nothing

parseTranslation_pass2 :: ModelTranslation -> ModelUri Readable -> MonadPerspectives ModelTranslation
parseTranslation_pass2 m@(ModelTranslation modelTranslation) (ModelUri someFqn) =
  fromReadableModelTranslation (qualifyModelTranslation (ModelTranslation modelTranslation { namespace = typeUri2typeNameSpace_ someFqn }))

-------------------------------------------------------------------------------
---- ADD TRANSLATIONS
-- Add existing translations from a TranslationTable to a new Translations derived from a DomeinFile
-------------------------------------------------------------------------------
augmentModelTranslation :: TranslationTable -> ModelTranslation -> ModelTranslation
augmentModelTranslation = addTranslations

-------------------------------------------------------------------------------
---- RUNTIME TRANSLATION FUNCTIONS
-------------------------------------------------------------------------------
-- | Look up the translation of `text` (a qualified type name or an original text) in the
-- | translation table of `domain` (an unversioned model URI), for the current language.
-- | Falls back to `text` itself if there is no table, no entry, no translation for the language,
-- | or if the translation is still a "MISSING <lang>" placeholder.
translationOf :: String -> String -> MonadPerspectives String
translationOf domain text = do
  language <- getCurrentLanguage
  mtable <- getTranslationTable domain
  case mtable of
    Nothing -> pure text
    Just (TranslationTable table) -> case lookup text table of
      Nothing -> pure text
      Just (Translations translations) -> case lookup language translations of
        Nothing -> pure text
        Just translation ->
          if isMissingPlaceholder translation then pure text
          else pure translation

-- | The placeholder written for languages that have not been translated yet (see addDefaultLanguages).
isMissingPlaceholder :: String -> Boolean
isMissingPlaceholder = Regex.test (unsafeRegex "^MISSING [A-Za-z_-]+$" noFlags)

translateTypeString :: String -> MonadPerspectives String
translateTypeString typeName = case typeUri2ModelUri typeName of
  Nothing ->
    if isExternalRole typeName then pure "External"
    else pure typeName
  Just domain -> translationOf domain (deconstructBuitenRol typeName)

-----------------------------------------------------------
-- CLASS ADDPERSPECTIVES
----------------------------------------------------------- 
class CollectMarkdowns a where
  collectMarkdowns :: a -> Array String
  collectTitles :: a -> Array String

instance addPerspectivesScreenDefinition :: CollectMarkdowns ScreenDefinition where
  collectMarkdowns (ScreenDefinition r) = concat $ map concat $ catMaybes
    [ map collectMarkdowns <$> r.tabs, map collectMarkdowns <$> r.rows, map collectMarkdowns <$> r.columns, (\a -> [ a ]) <<< collectMarkdowns <$> r.whoWhatWhereScreen ]
  collectTitles (ScreenDefinition r) =
    let
      titles =
        ( concat $ map concat $ catMaybes
            [ map collectTitles <$> r.tabs, map collectTitles <$> r.rows, map collectTitles <$> r.columns, (\a -> [ a ]) <<< collectTitles <$> r.whoWhatWhereScreen ]
        )
    in
      case r.title of
        Nothing -> titles
        Just title -> cons title titles

instance addPerspectivesScreenElementDef :: CollectMarkdowns ScreenElementDef where
  collectMarkdowns (RowElementD (RowDef re)) = concat $ collectMarkdowns <$> re
  collectMarkdowns (ColumnElementD (ColumnDef re)) = concat $ collectMarkdowns <$> re
  collectMarkdowns (TableElementD (TableDef re)) = []
  collectMarkdowns (FormElementD (FormDef re)) = []
  collectMarkdowns (MarkDownElementD re) = collectMarkdowns re
  collectMarkdowns (ChatElementD re) = []
  collectMarkdowns (TypeAheadFillerElementD re) = []
  collectMarkdowns (TypeAheadFormElementD re) = []
  collectMarkdowns (WhenElementD (WhenDef { elements })) = concat $ collectMarkdowns <$> elements
  collectTitles (RowElementD (RowDef re)) = concat $ collectTitles <$> re
  collectTitles (ColumnElementD (ColumnDef re)) = concat $ collectTitles <$> re
  collectTitles (TableElementD re) = collectTitles re
  collectTitles (FormElementD re) = collectTitles re
  collectTitles (MarkDownElementD re) = []
  collectTitles (ChatElementD re) = []
  collectTitles (TypeAheadFillerElementD re) = collectTitles re
  collectTitles (TypeAheadFormElementD re) = collectTitles re
  collectTitles (WhenElementD (WhenDef { elements })) = concat $ collectTitles <$> elements

instance CollectMarkdowns WhoWhatWhereScreenDef where
  collectMarkdowns (WhoWhatWhereScreenDef { who, what, whereto }) = collectMarkdowns who <> collectMarkdowns what <> collectMarkdowns whereto
  collectTitles (WhoWhatWhereScreenDef { who, what, whereto }) = collectTitles who <> collectTitles what <> collectTitles whereto

instance CollectMarkdowns Who where
  collectMarkdowns (Who { markdown, userRoles }) = (concat $ collectMarkdowns <$> markdown) <> (concat $ collectMarkdowns <$> userRoles)
  collectTitles (Who { userRoles }) = concat $ collectTitles <$> userRoles

instance CollectMarkdowns What where
  collectMarkdowns (TableForms { markdown, tableForms }) = (concat $ collectMarkdowns <$> markdown) <> (concat $ collectMarkdowns <$> tableForms)
  collectMarkdowns (FreeFormScreen r) = concat $ map concat $ catMaybes
    [ map collectMarkdowns <$> r.tabs, map collectMarkdowns <$> r.rows, map collectMarkdowns <$> r.columns ]
  collectTitles (TableForms { tableForms }) = concat $ collectTitles <$> tableForms
  collectTitles (FreeFormScreen r) =
    ( concat $ map concat $ catMaybes
        [ map collectTitles <$> r.tabs, map collectTitles <$> r.rows, map collectTitles <$> r.columns ]
    )

instance CollectMarkdowns WhereTo where
  collectMarkdowns (WhereTo { markdown, contextRoles }) = (concat $ collectMarkdowns <$> markdown) <> (concat $ collectMarkdowns <$> contextRoles)
  collectTitles (WhereTo { contextRoles }) = concat $ collectTitles <$> contextRoles

instance CollectMarkdowns TableFormDef where
  collectMarkdowns (TableFormDef { markdown, table, form }) = concat (collectMarkdowns <$> markdown) <> collectMarkdowns table <> collectMarkdowns form
  collectTitles (TableFormDef { table, form }) = (collectTitles table) <> (collectTitles form)

instance CollectMarkdowns WhenTableFormDef where
  collectMarkdowns (WhenTableFormDef { tableForms }) = concat $ collectMarkdowns <$> tableForms
  collectTitles (WhenTableFormDef { tableForms }) = concat $ collectTitles <$> tableForms

instance CollectMarkdowns TableFormOrWhenDef where
  collectMarkdowns (PlainTableFormDef tfd) = collectMarkdowns tfd
  collectMarkdowns (WhenTableFormItemDef wtfd) = collectMarkdowns wtfd
  collectTitles (PlainTableFormDef tfd) = collectTitles tfd
  collectTitles (WhenTableFormItemDef wtfd) = collectTitles wtfd

instance addPerspectivesTabDef :: CollectMarkdowns TabDef where
  collectMarkdowns (TabDef { elements }) = concat (collectMarkdowns <$> elements)
  collectTitles (TabDef { title, elements }) = cons title (concat (collectTitles <$> elements))

instance addPerspectivesColumnDef :: CollectMarkdowns ColumnDef where
  collectMarkdowns (ColumnDef cols) = concat (collectMarkdowns <$> cols)
  collectTitles (ColumnDef cols) = concat (collectTitles <$> cols)

instance addPerspectivesRowDef :: CollectMarkdowns RowDef where
  collectMarkdowns (RowDef rows) = concat (collectMarkdowns <$> rows)
  collectTitles (RowDef rows) = concat (collectTitles <$> rows)

instance addPerspectivesTableDef :: CollectMarkdowns TableDef where
  collectMarkdowns (TableDef { markdown }) = concat (collectMarkdowns <$> markdown)
  collectTitles (TableDef { widgetCommonFields }) = maybe [] (\title -> [ title ]) widgetCommonFields.title

instance addPerspectivesFormDef :: CollectMarkdowns FormDef where
  collectMarkdowns (FormDef { markdown }) = concat (collectMarkdowns <$> markdown)
  collectTitles (FormDef { widgetCommonFields }) = maybe [] (\title -> [ title ]) widgetCommonFields.title

instance CollectMarkdowns TypeAheadFillerDef where
  collectMarkdowns _ = []
  collectTitles (TypeAheadFillerDef { widgetCommonFields }) = maybe [] (\title -> [ title ]) widgetCommonFields.title

instance CollectMarkdowns TypeAheadFormDef where
  collectMarkdowns _ = []
  collectTitles (TypeAheadFormDef { widgetCommonFields }) = maybe [] (\title -> [ title ]) widgetCommonFields.title

instance CollectMarkdowns MarkDownDef where
  collectMarkdowns (MarkDownConstantDef { text }) = [ text ]
  collectMarkdowns (MarkDownExpressionDef _) = []
  collectMarkdowns (MarkDownPerspectiveDef _) = []
  collectTitles _ = []

instance CollectMarkdowns ChatDef where
  collectMarkdowns (ChatDef _) = []
  collectTitles _ = []

getCurrentLanguageFromIDB :: Aff String
getCurrentLanguageFromIDB = do
  mforeign <- idbGet "currentLanguage"
  case mforeign of
    Nothing -> pure "en"
    Just mlang -> case read_ mlang of
      Just lang -> pure lang
      Nothing -> pure "en"