import {PDRproxy} from "perspectives-proxy";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

export default function importContexts(fileList)
{
  function isContextSerialisation (t)
  {
    const firstContext = t[0];
    if (firstContext)
    {
      return firstContext.rollen && firstContext.id && firstContext.ctype;
    }
  }
  const theFile = fileList.item(0)
  if (theFile)
  {
    if (theFile.type == "application/json")
    {
      theFile.text().then( function(t)
      {
        const json = JSON.parse(t);
        if (isContextSerialisation( json ))
        {
          // Now send to the PDR.
          PDRproxy
            .then( pproxy => pproxy.importContexts( json ) )
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("importContexts_title", { ns: 'preact' }) 
                , message: i18next.t("importContexts_message", {ns: 'preact'})
                , error: e.toString()
                })));
        }
        else
        {
          alert("Not a valid Perspectives serialisation!")
        }
      });
    }
    else
    {
      alert("Not JSON!")
    }
  }
  else
  {
    alert("No file present!")
  }
}
