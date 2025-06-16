
import { PureComponent } from "react";

import {PDRproxy, PropertyType, RoleInstanceT} from "perspectives-proxy";
const Component = PureComponent;
import i18next from "i18next";
import {UserMessagingPromise} from "./userMessaging.js";
import { Image } from 'react-bootstrap';

interface AsyncImageProps {
  propId: PropertyType;
  roleId: RoleInstanceT;
  fileName: string;
}

interface AsyncImageState {
  fileUrl: string | undefined;
  file: File | undefined;
}

export class AsyncImage extends Component<AsyncImageProps, AsyncImageState>
{
  constructor(props : AsyncImageProps)
  {
    super(props);
    this.state = 
      { fileUrl: undefined
      , file: undefined
      };
  }

  componentDidMount()
  {
    const component = this;
    component.retrieveFile()
      .then( file => component.changeUrl(file));

    // Couchdb might not yet have finished synchronizing the write- and read database, so try again.
    setTimeout(() => 
      component.retrieveFile()
      .then( file => component.changeUrl(file))
      .catch(e => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("retrieveFile_title", { ns: 'preact' }) 
          , message: i18next.t("retrieveFile_message", {ns: 'preact'})
          , error: e.toString()
          }))), 400)
  }

  changeUrl(file : File)
  {
    if (this.state.file != file)
    {
      this.setState({fileUrl: window.URL.createObjectURL(file), file});
    }
  }

  componentWillUnmount ()
  {
    // No need to call super.componentWillUnmount() as it does not exist in PureComponent
    if ( this.state.fileUrl ) {
      URL.revokeObjectURL( this.state.fileUrl );
    }
  }

  retrieveFile()
  {
    const component = this; 
    return PDRproxy
      .then( pproxy => pproxy.getFile( component.props.roleId, component.props.propId ) )
  }

  render()
  {
    if (this.state.fileUrl)
    {
      return <Image src={this.state.fileUrl} fluid/>
    }
    else 
    {
      return null;
    }
  }
}
