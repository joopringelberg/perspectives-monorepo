-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

-- Special-perspectives regression test model.
-- Tests selfOnly properties, authorOnly properties, selfOnly perspectives and
-- authorOnly perspectives using a three-PDR setup:
--   pdrA  = Alice  (Manager / Leader)
--   pdrB  = Bob    (Follower 1 – the primary / positive Follower)
--   pdrC  = Charlie (Follower 2 – the secondary / negative Follower)

domain model://joopringelberg.nl#SpecialPerspectivesTestModel@1.0
  use sys for model://perspectives.domains#System
  use sp for model://joopringelberg.nl#SpecialPerspectivesTestModel

  -------------------------------------------------------------------------------
  ---- SETTING UP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context TestApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Special Perspectives Test App" for start
          IsSystemModel = true for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (sp:SpecialPerspectivesTestApp >> extern)
      in
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- INDEXED CONTEXT
  -------------------------------------------------------------------------------
  case TestApp
    indexed sp:SpecialPerspectivesTestApp
    aspect sys:RootContext
    external

    user Manager = sys:Me
      perspective on Tests
        only (CreateAndFill, RemoveContext)
      perspective on Tests >> binding >> context >> Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)
        props (FirstName) verbs (Consult)

    -- Two Followers are used in the three-PDR test setup (Bob and Charlie).
    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)

    context Tests (relational) filledBy Test

  -------------------------------------------------------------------------------
  ---- BASE TEST CONTEXT
  ---- All specific test contexts use this as an aspect.
  -------------------------------------------------------------------------------
  case Test

    -- The state machine fires once the test instance is linked to TestApp$Tests,
    -- giving us a stable AppFollower reference to work with.
    state AppFollowersReachable = exists extern >> binder Tests
      on entry
        do for Initializer
          -- Bind all TestApp Followers (Bob AND Charlie) into this test context.
          bind AppFollower >> binding to Follower
          bind me to Leader

    external
      property TestName (String)
      property TestSucceeded (Boolean)

    -- Relational: both Bob and Charlie appear as AppFollower instances.
    user AppFollower (relational) = extern >> binder Tests >> context >> Follower

    user Initializer = me
      perspective on Leader
        only (Create, Fill)
      perspective on Follower
        only (Create, Fill)

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on extern
        props (TestName) verbs (SetPropertyValue, Consult)

    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)
      perspective on Leader
        props (FirstName) verbs (Consult)
      perspective on extern
        props (TestName) verbs (Consult)
        props (TestSucceeded) verbs (Consult, SetPropertyValue)

  -------------------------------------------------------------------------------
  ---- TEST: selfOnly property
  ---- Leader sets P on every Follower instance.
  ---- Because P is selfonly, each Follower can see the value only on their own
  ---- instance.  Charlie (Follower 2) must NOT see P on Bob's (Follower 1) instance.
  -------------------------------------------------------------------------------
  case Test_SelfOnly_Property
    aspect sp:Test
    external
      -- Leader evaluates this condition; Leader sees all P values (no selfonly
      -- restriction on Leader's perspective).
      state TestSucceeded = exists context >> Follower >> P
        on entry
          do for Leader
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Leader
      perspective on Follower
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        P = 1 for Follower
        TestName = "selfOnly property: each Follower sees only their own P" for extern

    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Follower
      property P (selfonly, Number)
      perspective on Follower
        selfonly
        props (P) verbs (Consult)

  -------------------------------------------------------------------------------
  ---- TEST: authorOnly property
  ---- Bob (Follower 1) creates TestRole1 and sets P (authorOnly).
  ---- Charlie (Follower 2) must NOT see P on TestRole1.
  -------------------------------------------------------------------------------
  case Test_AuthorOnly_Property
    aspect sp:Test
    external
      state TestSucceeded = exists context >> TestRole1 >> P
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Leader

    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Follower
      perspective on TestRole1
        only (Create)
        props (P) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          tr <- create role TestRole1
        in
          P = 1 for tr
          TestName = "authorOnly property: only the author sees P" for extern

    thing TestRole1
      property P (authoronly, Number)

  -------------------------------------------------------------------------------
  ---- TEST: selfOnly perspective
  ---- Leader sets Q on every Follower instance.
  ---- Because the Follower perspective on Follower is selfonly, Charlie (Follower 2)
  ---- does not receive Bob's (Follower 1) role instance at all.
  -------------------------------------------------------------------------------
  case Test_SelfOnly_Perspective
    aspect sp:Test
    external
      state TestSucceeded = exists context >> Follower >> Q
        on entry
          do for Leader
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Leader
      perspective on Follower
        props (Q) verbs (SetPropertyValue, Consult)
      action RunTest
        Q = 1 for Follower
        TestName = "selfOnly perspective: Follower cannot see the other Follower's instance" for extern

    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Follower
      property Q (Number)
      -- selfonly perspective: each Follower sees only their own instance.
      perspective on Follower
        selfonly
        props (Q) verbs (Consult)

  -------------------------------------------------------------------------------
  ---- TEST: authorOnly perspective
  ---- Bob (Follower 1) creates PrivateRole and sets R.
  ---- Because the perspective is authoronly, Charlie (Follower 2) never receives
  ---- PrivateRole or R at all.
  -------------------------------------------------------------------------------
  case Test_AuthorOnly_Perspective
    aspect sp:Test
    external
      state TestSucceeded = exists context >> PrivateRole
        on entry
          do for Follower
            TestSucceeded = true

    user Leader filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Leader

    user Follower (relational) filledBy (sys:TheWorld$PerspectivesUsers)
      aspect sp:Test$Follower
      perspective on PrivateRole
        authoronly
        only (Create)
        props (R) verbs (SetPropertyValue, Consult)
      action RunTest
        letA
          pr <- create role PrivateRole
        in
          R = 1 for pr
          TestName = "authorOnly perspective: only the author sees PrivateRole" for extern

    thing PrivateRole
      property R (Number)
