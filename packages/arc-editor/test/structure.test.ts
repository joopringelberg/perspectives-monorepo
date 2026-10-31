import assert from "node:assert/strict";
import { test } from "node:test";
import { enclosingContextOfRole } from "../src/structure.ts";

const source = `domain model://perspectives.domains#Test
  use sys for model://perspectives.domains#System

  case Meeting
    aspect thing sys:Chats$Chat
    -- comment
    user Chair
      property Name (String)

      perspective on Agenda
        props (Title) verbs (Consult)

    thing Agenda
      property Title (String)

    case SubMeeting
      external
        property Date (Date)
  perspective on Meeting`.split("\n");

test("Jump to the lexical context of a role", () => {
  assert.equal(enclosingContextOfRole(source, 4), 3);    // aspect role: the context it is added to
  assert.equal(enclosingContextOfRole(source, 6), 3);    // role declaration line
  assert.equal(enclosingContextOfRole(source, 10), 3);   // deep inside a perspective of the role
  assert.equal(enclosingContextOfRole(source, 13), 3);
  assert.equal(enclosingContextOfRole(source, 17), 15);  // external role of a nested context
  assert.equal(enclosingContextOfRole(source, 3), undefined);
  assert.equal(enclosingContextOfRole(source, 18), undefined);
});
