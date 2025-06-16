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

import { Component } from "react";
import {render} from 'react-dom';
import styles from '@chatscope/chat-ui-kit-styles/dist/default/styles.min.css';
import ChatComponent from "./chatcomponent";

class App extends Component
{
    constructor(props)
    {
        super(props);
    }

    render()
    {
        return <ChatComponent roleinstance="testrole" roletype="testroletype" messagesproperty="messageprop" mediaproperty="mediaprop" externalrole="testexternalrole"/>
    }

}

render(<App />, document.getElementById('root'))
