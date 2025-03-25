import { FIREANDFORGET, PDRproxy, RoleInstanceT, ContextInstanceT } from "perspectives-proxy";
import { ContextInstance, externalRole, isExternalRole, RoleInstance } from "perspectives-react";

type ExternalRoleMatches = { tag: "RoleInstance", value: RoleInstanceT } | {tag : "Choices", value: Record<string, ContextInstanceT>};

/**
 * Tries to map the provided input string with a valid external role. It need not be a qualified role identifier.
 * 
 * This function performs the following steps:
 * - If the role identifier is already an external role, it resolves immediately.
 * - If the role identifier is not an external role, it attempts to match the context name.
 * - If a match is found, it resolves with the external role.
 * - If no match is found, it attempts to get the binding of the role identifier.
 * - If a binding is found and it is an external role, it resolves with the binding.
 * - If the binding is not an external role, it attempts to get the context of the binding.
 * - If the context is found, it resolves with the external role of the context.
 * - If no binding or context is found, it rejects with an error.
 * 
 * @param s - The role identifier to ensure as an external role.
 * @returns A promise that resolves to the external role instance.
 * @throws An error if the role identifier cannot be resolved to an external role.
 */
export default function ensureExternalRole(s : string) : Promise<ExternalRoleMatches>
{
  if ( isExternalRole( s ) )
  {
    return Promise.resolve( { tag: "RoleInstance", value: s as RoleInstanceT} );
  }
  else
  {
    // Request the binding and then its context.
    return PDRproxy.then( proxy =>
      new Promise( function( resolve, reject )
        {
          proxy.matchContextName( s, 
            function (serialisedtables : Record<string, ContextInstanceT>[])
              {
                const table = serialisedtables[0];
                if ( Object.values( table ).length > 0 )
                {
                  if ( table[s] !== undefined )
                  {
                    resolve( { tag: "RoleInstance", value: externalRole( table[s] ) } );
                  }
                  else
                  {
                    resolve( { tag: "Choices", value: table } );
                  }
                }
                else
                {
                  // No match. Try to get the filler.
                  proxy.getBinding( s as RoleInstanceT,
                    function (bindingIds)
                      {
                        if (bindingIds.length > 0)
                        {
                          if ( isExternalRole (bindingIds[0]))
                          {
                            // If the filler is an external role, resolve with the filler.
                            resolve( {tag: "RoleInstance", value: bindingIds[0] } );
                          }
                          else
                          {

                            proxy.getRolContext( bindingIds[0] ).then(
                              contextArr => resolve( {tag: "RoleInstance", value: externalRole( contextArr[0] )} ),
                            )
                          }
                        }
                        else
                        {
                          // Otherwise, either not a context role after all, or no binding. Fail.
                          return reject( new Error( "This role is not an external role and has no filler either, so cannot open a context for role: " + s ));
                        }
                      },
                    FIREANDFORGET,
                    function (e)
                    {
                      return reject(e);
                    });
                }
              }
            , FIREANDFORGET );
        }));
  }
}

