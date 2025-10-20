// NOTE DEPENDENCIES. Code in this section is adapted from module Perspectives.Identifiers.

import { PropertyType, RoleInstanceT } from "perspectives-proxy";

// deconstructSegments :: String -> String
// For type identifiers: from model://some.domain#System$any$segment, take any$segment.
interface DeconstructSegmentsResult {
  segments: string;
}

export function deconstructSegments(s: string): string {
  const typePattern = "^(model://[^/]+#[^\\$]+)\\$?(.*)$";
  try {
    return s.match(typePattern)![2];
  } catch (e) {
    throw "deconstructSegments: no local name in '" + s + "'.";
  }
}

// From "model://some.authority#Modelname$Sometype$Subtype" return "Subtype";
interface DeconstructLocalNameResult {
  localName: string;
}

export function deconstructLocalName(s: string): string {
  const localNameRegEx = new RegExp("^model://[^/]+#[^#/]+\\$([^\\$/]*)$");
  try {
    return s.match(localNameRegEx)![1];
  } catch (e) {
    throw "deconstructLocalName: no local name in '" + s + "'.";
  }
}

// A Namespace has the form "model:Name"
export function externalRole( s : string) : RoleInstanceT
{
  return s + "$External" as RoleInstanceT;
}

export function isExternalRole( s : string) : boolean
{
  const r = new RegExp("External$");
  return !!s.match(r);
}

export function deconstructContext( s : string) : string
{
  const matches = s.match(/(.*?)(?:\$External)/);
  if ( matches )
  {
    return matches[1];
  }
  else
  {
    return s;
  }
}

// TODO!
// From a type identifier, retrieve the name of the file of the local version of a model.
// "model://some.authority#Modelname$Sometype" should result in some_authority-Modelname.json.
// We achieve this by first capturing the part following "model://" and before the first "$",
// then replacing "." by "_" and "#" by "-".
export function deconstructModelName( s : string) : string
{
  const typeRegex = new RegExp( "^model://([^/]+#[^\\$]+)\\$?.*$" );
  const m = s.match(typeRegex);
  if ( m )
  {
    return m[1].replace(".", "_").replace("#", "-");
  }
  else {
    throw "deconstructModelName: the string '" + s + "' is not a well-formed domeinURI";
  }
}

export function getQualifiedPropertyName (localName : string, qualifiedNames : PropertyType[]) : PropertyType
{
    // Match the local propertyname given as a prop with the qualified names in context.
    const r = new RegExp(".*" + localName + "$");
    const n = qualifiedNames.filter( qn => qn.match(r));
    if (qualifiedNames.indexOf(localName as PropertyType) > -1)
    {
      // The local name is a qualified name.
      return localName as PropertyType;
    }
    if (n.length > 1) {
      throw "PROGRAMMER WARNING: '" + localName + "' does not uniquely identify a property. Choose one of: " + n;
      }
    else if (n == undefined || n.length == 0) {
      throw "PROGRAMMER WARNING: '" + localName + "' does not match of the properties " + qualifiedNames;
    }
    else {
      return n[0];
    }
}

// Is this a type identifier?
export function isQualifiedName(s : string) : boolean
{
  const typeRegEx = new RegExp( "^(model://[^/]+#[^\\$]+)\\$?(.*)$" );
  return s.match(typeRegEx) !== null;
}

// Is this a resource identifier?
export function isSchemedResourceIdentifier(s : string) : boolean
{
  const schemedResourceRegEx = new RegExp( "^(def|loc|rem|pub|model|):(.*)$" );
  return s.match(schemedResourceRegEx) !== null;
}

export function takeCUID(s : string) : string
{
  const r = new RegExp("^[^#$]+#(.+)");
  const result = s.match(r);
  if (result)
  {
    return result[1];
  }
  else
  {
    throw "takeCUID: no CUID in '" + s + "'.";
  }
}