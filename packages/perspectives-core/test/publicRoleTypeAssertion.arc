domain model://perspectives.domains#PublicRoleTypeAssertionTest
  case Box
    thing Repository (mandatory)
    thing ClaimedRepository = publicrole pub:cw_test#repository (Repository)
