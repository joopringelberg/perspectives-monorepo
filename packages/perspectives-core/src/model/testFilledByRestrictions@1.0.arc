-- Copyright Joop Ringelberg and Cor Baars, 2026
-- A model to test all variants of the filledBy restriction syntax:
--
--   1. Single restriction:
--        filledBy TraitAlpha
--        Accepts any filler that is (an aspect of) TraitAlpha.
--
--   2. Combination (AND):
--        filledBy (TraitAlpha + TraitBeta)
--        Accepts only fillers that satisfy BOTH TraitAlpha AND TraitBeta.
--
--   3. Alternatives (OR):
--        filledBy (TraitAlpha, TraitBeta)
--        Accepts fillers that satisfy TraitAlpha OR TraitBeta.
--
--   4. Disjunction of conjunctions (OR of ANDs):
--        filledBy ((TraitAlpha + TraitBeta), TraitGamma)
--        Accepts fillers that satisfy (TraitAlpha AND TraitBeta) OR TraitGamma.
--
-- Concrete filler roles and the restrictions they satisfy:
--
--   Alpha     → TraitAlpha
--   Beta      → TraitBeta
--   AlphaBeta → TraitAlpha AND TraitBeta
--   Gamma     → TraitGamma
--
-- Expected filler compatibility:
--
--   SlotSingle       (filledBy TraitAlpha):                 Alpha ✓  Beta ✗  AlphaBeta ✓  Gamma ✗
--   SlotCombination  (filledBy (TraitAlpha + TraitBeta)):   Alpha ✗  Beta ✗  AlphaBeta ✓  Gamma ✗
--   SlotAlternatives (filledBy (TraitAlpha, TraitBeta)):    Alpha ✓  Beta ✓  AlphaBeta ✓  Gamma ✗
--   SlotDisjunction  (filledBy ((TraitAlpha + TraitBeta), TraitGamma)):
--                                                           Alpha ✗  Beta ✗  AlphaBeta ✓  Gamma ✓

domain model://joopringelberg.nl#TestFilledByRestrictions@1.0
  use sys for model://perspectives.domains#System
  use mm for model://joopringelberg.nl#TestFilledByRestrictions

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          -- This is to add an entry to the Start Contexts in System.
          app <- create context TestFilledByRestrictionsApp
          start <- create role StartContexts in sys:MySystem
        in
          -- Being a RootContext, too, Installer can fill a new instance
          -- of StartContexts with it.
          bind_ app >> extern to start
          Name = "Test FilledBy Restrictions" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (mm:TestFilledByRestrictionsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (mm:TestFilledByRestrictionsApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestFilledByRestrictionsApp
    indexed mm:TestFilledByRestrictionsApp
    aspect sys:RootContext
    external

    user Manager = sys:Me
      -- Perspectives on the concrete filler roles (create instances to use as fillers)
      perspective on Alpha
        only (Create, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Beta
        only (Create, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on AlphaBeta
        only (Create, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      perspective on Gamma
        only (Create, Remove)
        props (Name) verbs (Consult, SetPropertyValue)
      -- Perspectives on the slot roles (create slots and fill them with the filler instances above)
      perspective on SlotSingle
        only (Create, Fill, Remove)
        props (Label) verbs (Consult, SetPropertyValue)
      perspective on SlotCombination
        only (Create, Fill, Remove)
        props (Label) verbs (Consult, SetPropertyValue)
      perspective on SlotAlternatives
        only (Create, Fill, Remove)
        props (Label) verbs (Consult, SetPropertyValue)
      perspective on SlotDisjunction
        only (Create, Fill, Remove)
        props (Label) verbs (Consult, SetPropertyValue)

    ---- ABSTRACT TRAIT ROLES ----
    -- These are the types used in filledBy restrictions.
    -- Concrete filler roles inherit them via `aspect`.

    thing TraitAlpha
      aspect sys:RoleWithName

    thing TraitBeta
      aspect sys:RoleWithName

    thing TraitGamma
      aspect sys:RoleWithName

    ---- CONCRETE FILLER ROLES ----

    -- Alpha satisfies TraitAlpha only.
    thing Alpha (relational)
      aspect mm:TestFilledByRestrictionsApp$TraitAlpha

    -- Beta satisfies TraitBeta only.
    thing Beta (relational)
      aspect mm:TestFilledByRestrictionsApp$TraitBeta

    -- AlphaBeta satisfies both TraitAlpha AND TraitBeta simultaneously.
    thing AlphaBeta (relational)
      aspect mm:TestFilledByRestrictionsApp$TraitAlpha
      aspect mm:TestFilledByRestrictionsApp$TraitBeta

    -- Gamma satisfies TraitGamma only.
    thing Gamma (relational)
      aspect mm:TestFilledByRestrictionsApp$TraitGamma

    ---- SLOT ROLES WITH FILLEDBY RESTRICTIONS ----

    -- Case 1: Single restriction.
    -- Accepts: Alpha, AlphaBeta.  Rejects: Beta, Gamma.
    thing SlotSingle (relational) filledBy TraitAlpha
      property Label (String)

    -- Case 2: Combination (AND) restriction.
    -- The filler must satisfy BOTH TraitAlpha AND TraitBeta.
    -- Accepts: AlphaBeta only.  Rejects: Alpha, Beta, Gamma.
    thing SlotCombination (relational) filledBy (TraitAlpha + TraitBeta)
      property Label (String)

    -- Case 3: Alternatives (OR) restriction.
    -- The filler must satisfy TraitAlpha OR TraitBeta.
    -- Accepts: Alpha, Beta, AlphaBeta.  Rejects: Gamma.
    thing SlotAlternatives (relational) filledBy (TraitAlpha, TraitBeta)
      property Label (String)

    -- Case 4: Disjunction of conjunctions (OR of ANDs).
    -- The filler must satisfy (TraitAlpha AND TraitBeta) OR TraitGamma.
    -- Accepts: AlphaBeta, Gamma.  Rejects: Alpha, Beta.
    thing SlotDisjunction (relational) filledBy ((TraitAlpha + TraitBeta), TraitGamma)
      property Label (String)
