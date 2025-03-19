import { FIREANDFORGET, PDRproxy, RoleInstanceT, ValueT } from "perspectives-proxy";
import { externalRole, isExternalRole, RoleInstance } from "perspectives-react";

/**
 * Ensures that the provided role identifier is an external role.
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
export default function ensureExternalRole(s : string) : Promise<RoleInstanceT>
{
  if ( isExternalRole( s ) )
  {
    return Promise.resolve( s as RoleInstanceT);
  }
  else
  {
    // Request the binding and then its context.
    return PDRproxy.then( proxy =>
      new Promise( function( resolve, reject )
        {
          proxy.matchContextName( s, 
            function (serialisedtables : { [key: string]: string }[])
              {
                const table = serialisedtables[0];
                if (table[s])
                {
                  resolve( externalRole( table[s] ))
                }
                else if (Object.values(table).length == 1)
                {
                  resolve( externalRole( Object.values(table)[0]) );
                }
                else
                {
                  proxy.getBinding( s as RoleInstanceT,
                    function (bindingIds)
                      {
                        if (bindingIds.length > 0)
                        {
                          if ( isExternalRole (bindingIds[0]))
                          {
                            resolve( bindingIds[0] );
                          }
                          else
                          {
                            proxy.getRolContext( bindingIds[0] ).then(
                              contextArr => resolve( externalRole( contextArr[0] ))
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
