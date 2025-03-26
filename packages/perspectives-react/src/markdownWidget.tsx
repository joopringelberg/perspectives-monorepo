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

////////////////////////////////////////
//// MARKDOWNWIDGET
////////////////////////////////////////
// This module provides a markdown widget that can be used in a Perspectives application.
// It uses markdown-it to render markdown text.

///// TODO: HANDLE THE OPTIONAL 'OPEN' PROP.


import React, { Component } from "react";
import { string } from "prop-types";
import {ContextInstanceT, PDRproxy, RoleInstanceT, RoleType} from "perspectives-proxy";

import markdownit from 'markdown-it'
const md = markdownit();

import {default as markdownPerspectives} from "./perspectives-markdown.js";
// https://www.npmjs.com/package/markdown-it-link-attributes
import mila from "markdown-it-link-attributes";
import { UserMessagingPromise } from "./userMessaging.js";
import i18next from "i18next";

md.use(markdownPerspectives);
// All hyperlinks will open in a new tab or window.
md.use(mila, {
  attrs: {
    target: "_blank"
  },
});

interface PerspectivesEntryPoint {
  opencontext: (event: Event, roleIdentifier: RoleInstanceT) => void;
  runaction: (event: Event, actionName: string, contextid: ContextInstanceT, myroletype: RoleType) => void;
}

declare global {
  interface Window {
    $perspectives_entry_point_for_markdown$: PerspectivesEntryPoint;
  }
}


if (!window.$perspectives_entry_point_for_markdown$)
{
  window.$perspectives_entry_point_for_markdown$ = {
    opencontext: function(event: Event, roleIdentifier: string) {
      event.preventDefault();
      event.stopPropagation();
      // NOTICE dependency on MyContexts App.js! Instead, put handlers in the screen component.
      (document.getElementById(__MyContextsContainer__) as HTMLElement).dispatchEvent(new CustomEvent('OpenContext', { detail: roleIdentifier, bubbles: true }));
    },
    runaction: function(event: Event, actionName: string, contextid: ContextInstanceT, myroletype: RoleType) {
      event.preventDefault();
      event.stopPropagation();

      PDRproxy.then(function(pproxy) {
        pproxy.contextAction(contextid, myroletype, actionName).catch(e =>
          UserMessagingPromise .then(um =>
            um.addMessageForEndUser({
              title: i18next.t("action_title", { ns: 'preact' }),
              message: i18next.t("action_message", { ns: 'preact', action: actionName }),
              error: e.toString()
            })
          )
        );
      });
    }
  };
}

interface MarkDownWidgetProps {
  markdown: string;
  contextid: ContextInstanceT;
  myroletype: RoleType;
  open?: boolean;
}

export class MarkDownWidget extends Component<MarkDownWidgetProps>
{
  constructor(props: MarkDownWidgetProps)
  {
    super(props);
  }
  render()
  {
    const component = this;
    const htmlString = md.render( component.props.markdown );
    const htmlString_ = htmlString.replace(/__contextid__/g, component.props.contextid).replace( /__myroletype__/g, component.props.myroletype);

    return <div dangerouslySetInnerHTML={{ __html: htmlString_ }} />
  }

}

interface UnboundMarkDownWidgetProps {
  markdown: string;
  open?: boolean;
}

// Use this component to create html from markdown that is provided as a property of the component itself, in the page.
// It may use the link instruction, but cannot use the action instruction.
export class UnboundMarkDownWidget extends Component<UnboundMarkDownWidgetProps>
{
  constructor(props: MarkDownWidgetProps)
  {
    super(props);
  }
  render()
  {
    const component = this;
    return <div dangerouslySetInnerHTML={{ __html: md.render( component.props.markdown ) }} />
  }
}
