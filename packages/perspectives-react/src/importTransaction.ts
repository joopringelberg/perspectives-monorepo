import {PDRproxy} from "perspectives-proxy";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

// Sends a parsed transaction object to the PDR proxy.
export function sendTransactionToProxy(transaction: any): void
{
  PDRproxy
    .then( pproxy => pproxy.importTransaction( transaction ) )
    .catch(e => UserMessagingPromise.then( um => 
      um.addMessageForEndUser(
        { title: i18next.t("importTransaction_title", { ns: 'preact' }) 
        , message: i18next.t("importTransaction_message", {ns: 'preact'})
        , error: e.toString()
        })));
}

export default function importTransaction(theFile : File)
{
  // A slight check.
  interface Transaction {
    timeStamp: string;
    deltas: any;
  }

  function isTransaction(t: any): t is Transaction {
    if (!t.timeStamp) { return false; }
    if (!t.deltas) { return false; }
    return true;
  }
  if (theFile)
  {
    if (theFile.type == "application/json")
    {
      theFile.text().then( function(t)
      {
        const json = JSON.parse(t);
        if (isTransaction( json ))
        {
          // debugger;
          // Now send to the PDR.
          PDRproxy
            .then( pproxy => pproxy.importTransaction( json ) )
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("importTransaction_title", { ns: 'preact' }) 
                , message: i18next.t("importTransaction_message", {ns: 'preact'})
                , error: e.toString()
                })));
        }
        else
        {
          alert("Not a valid Perspectives serialisation!");
        }
      });
    }
    else
    {
      alert("Not JSON!");
    }
  }
  else
  {
    alert("No file present!");
  }
}
