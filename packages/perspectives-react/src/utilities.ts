// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE

import { InputType } from "perspectives-proxy/dist/perspectivesshape";

// Use this function to open a page relative to the origin of the application.
// Don't rely on the website root (/), as we may serve the app from a subdirectory like "/remotetest/"
export function thisAppsLocation()
{
  const segments = location.pathname.split("/");
  segments.pop();
  return location.origin +  segments.join("/") + "/";
}

export function formatPropertyValue(propertyValues: string[], inputType: InputType) : string
{
  function epoch_to_datetime_local (epoch : string)
  {
    const dt = new Date( parseInt( epoch ) );
    dt.setMinutes(dt.getMinutes() - dt.getTimezoneOffset());
    return dt.toISOString().slice(0,16);
  }

  // returns either "hh:mm" or "hh:mm.ss"
  function milliseconds_to_time (millis : string)
  {
    // If I don't subtract 
    const tm = new Date( parseInt( millis ) );
    return pad(tm.getUTCHours()) + ":" + pad(tm.getUTCMinutes()) + (tm.getUTCSeconds() ? ":" + pad(tm.getUTCMilliseconds()) : "");
  }

  // returns "yyyy-mm-dd"
  function epoch_to_date( epoch : string )
  {
    const dt = new Date( parseInt( epoch ) );
    return dt.getFullYear() + "-" + pad( (dt.getMonth() + 1) ) + "-" + pad( dt.getDate() );
  }
  function pad(n : number)
  {
    if (n < 10)
    {
      return "0" + n;
    }
    else
    {
      return n + "";
    }
  }

    if (propertyValues[0])
    {
      switch (inputType) {
        case "datetime-local":
          // a datetime is represented as a timestamp (milliseconds sinds epoch).
          return epoch_to_datetime_local( propertyValues[0] );  
        case "date":
          // A date is represented as a timestamp, but without a time component.
          return epoch_to_date ( propertyValues[0] );
        case "time":
          // a time is represented in terms of milliseconds.
          // We have to provide a string in the form "hh:mm"
          return milliseconds_to_time ( propertyValues[0] );
        default:
          return propertyValues[0];
      }
    }
    else 
    {
      return "";
    }
}
