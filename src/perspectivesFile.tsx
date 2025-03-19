import React from 'react';

import {Form, Row} from 'react-bootstrap';

import {PDRproxy, RoleInstanceT, RoleType, PerspectivesFile as PerspectivesFileType, ValueT} from "perspectives-proxy";
// const PDRproxy = new Promise( function(resolve)
//   {
//     resolve(
//       { setProperty: function(i1, i2, value){ alert("Stub: saving property value: " + value); return new Promise(resolver => resolver(true));}
//       , saveFile: function(){ alert("Stub: saving file."); return new Promise(resolver => resolver(true));}
//       }
//     )
//   })
import { shape, string } from "prop-types";
import PerspectivesComponent from "./perspectivesComponent";
import i18next from "i18next";
import { Col } from 'react-bootstrap';
import { UploadIcon, DownloadIcon} from '@primer/octicons-react';
import {UserMessagingPromise} from "./userMessaging.js";
import {AsyncImage} from "./asyncImage.js";
import {ArcViewer} from "./arcViewer.js";
import {PropertyValues, SerialisedProperty} from "perspectives-proxy";

// As the real action happens in handleFile as it is presented on the props of FileDropZone,
// an error boundary is of no good here.

type selectedField = "fileName" | "mimeType" | "upload" | "download";
const UPLOAD = "upload";
const DOWNLOAD = "download";
const FILENAME = "fileName";
const MIMETYPE = "mimeType";

type PerspectivesFileProps = 
{ 
  serialisedProperty: SerialisedProperty;
  myRoletype: RoleType;
  propertyValues?: PropertyValues;
  roleId?: RoleInstanceT;
}

interface PerspectivesFileState
{
  fileName?: string
  mimeType?: string
  database?: string
  roleFileName?: string

  state: state
  previousState: state | undefined
  selectedField: selectedField | undefined
}

type state = "empty" | "filled" | "readonly" | "editable";
// In state "empty" we have no roleId on the props.
const EMPTY = "empty";
// In state "readonly" there may be a roleId.
const READONLY = "readonly";
// In state "filled" we have roleId on the props.
const FILLED = "filled";
// In state "editable" we have a roleId on the props.
const EDITABLE = "editable";

export class PerspectivesFile extends PerspectivesComponent<PerspectivesFileProps, PerspectivesFileState>
{
  constructor(props : PerspectivesFileProps)
  {
    super(props);
    if (props.propertyValues && props.propertyValues.values.length > 0)
    {
      // We must, by construction of the props, have both the roleId and the propertyValues.
      const fileProp = JSON.parse(props.propertyValues!.values[0]) as PerspectivesFileType;
      this.state = 
      { fileName: fileProp.fileName
      , mimeType: fileProp.mimeType
      , database: fileProp.database
      , roleFileName: fileProp.roleFileName
      , state: this.propertyOnlyConsultable() ? READONLY : FILLED
      , previousState: EMPTY
      , selectedField: undefined
      }      
    }
    else {
      this.state = 
      { state: this.propertyOnlyConsultable() ? READONLY : EMPTY
      , previousState: EMPTY
      , selectedField: undefined
      }
    }
  }

  // An update may show a roleId, and it may also show propertyValues.
  componentDidUpdate(prevProps : PerspectivesFileProps)
  {
    let fileProp : PerspectivesFileType | null = null
      , prevFileProp  : PerspectivesFileType | null = null, 
      updater : { fileName: string, mimeType: string, database?: string, roleFileName: string, state: state};
    if ( prevProps.propertyValues?.values[0] )
    {
      prevFileProp = JSON.parse( prevProps.propertyValues.values[0] );
    }
    if (this.props.propertyValues?.values[0])
    {
      fileProp = JSON.parse( this.props.propertyValues.values[0] ) as PerspectivesFileType;
    }
    if ( prevFileProp?.fileName != fileProp?.fileName )
    {
      updater = 
        { fileName: fileProp!.fileName || ""
        , mimeType: fileProp!.mimeType || ""
        , database: fileProp!.database
        , roleFileName: fileProp!.roleFileName
        , state: !prevProps.roleId && this.props.roleId ? EMPTY : FILLED
        };
      this.setState(updater);
    }
    // this is the first time we have a roleId on the props.
    // I think this situation never co-uccurs with the previous one.
    if (!prevProps.roleId && this.props.roleId)
    {
      this.setState({state: EMPTY});
    }
  }

  // The property is only consultable when it just has the verb Consult,
  // or when it is calculated. It will be shown disabled as a consequence.
  propertyOnlyConsultable()
  {
    if (this.props.propertyValues)
    {
      const propertyVerbs = this.props.propertyValues.propertyVerbs;
      const property = this.props.serialisedProperty;
      return (propertyVerbs.indexOf("Consult") > -1 
        && propertyVerbs.length == 1)
        || property.isCalculated;
    }
    else
    {
      return false;
    }
  }

  handleKeyDownInReadOnly(event : React.KeyboardEvent)
  {
    const component = this;
    switch(event.keyCode){

      case 39: // right arrow
        // If we have a file (as can be seen from the url property), we may move to the download button.
        if (component.state.roleFileName)
        {
          event.preventDefault();
          event.stopPropagation();      
          component.focusDownload();
          break;
        }
    }
  }

  handleKeyDownInEmpty(event : React.KeyboardEvent)
  {
    const component = this;
    let fileName, mimeType, newFile : File;
    switch(event.keyCode){
      // case 9: // horizontal tab; vertical tab is 11
      // case 37: // left arrow
      case 39: // right arrow
        if (component.state.selectedField == FILENAME)
        {
          event.preventDefault();
          event.stopPropagation();      
          // the fileName field saves its value on losing focus; move focus to the other field
          component.focusMimeType();
        }
        else if (component.state.selectedField == MIMETYPE)
        {
          event.preventDefault();
          event.stopPropagation();      
          // the mimeType field saves its value on losing focus; move focus to the other field
          component.focusUpload();
        }
        else if (component.state.selectedField == UPLOAD)
        {
          event.preventDefault();
          event.stopPropagation();      
          component.focusFileName();
        }
        break;

      case 13: // Enter
        // Use values to create a file.
        // Both fields must be filled.
        event.preventDefault();
        event.stopPropagation();

        fileName = document.getElementById(this.props.serialisedProperty.id + '_fileName') as HTMLInputElement;
        mimeType = document.getElementById(this.props.serialisedProperty.id + '_mimeType') as HTMLInputElement;
        if  ( this.reportValidity(fileName!, i18next.t("fileName_invalid", { ns: 'preact' })) && 
              this.reportValidity(mimeType, 
                (component.props.serialisedProperty.constrainingFacets.pattern && component.props.serialisedProperty.constrainingFacets.pattern.label || i18next.t("mimeType_invalid", { ns: 'preact' })))
            )
        {
          // Create the file and save it and the property value itself.
          if (component.state.fileName && component.state.mimeType)
          {
          newFile = new File([""], component.state.fileName, {type: component.state.mimeType});
          component.saveFileAndProperty(newFile).then(() => 
            // Change state (previousState is still EMPTY).
            component.setState({state: FILLED}) );
          }
        }
      
        break;

      case 27: // Escape
        // Discard changes.
        event.preventDefault();
        event.stopPropagation();   
        // previousState is still EMPTY.
        component.setState({state: EMPTY, fileName: "", mimeType: ""});

        break;
      }
  }

  handleKeyDownInFilled(event : React.KeyboardEvent)
  {
    const component = this;
    switch(event.keyCode){
      case 13: // Enter
        event.preventDefault();
        event.stopPropagation();      

        // previousState is still EMPTY.
        component.setState({state: EDITABLE, selectedField: FILENAME})
        break;

      case 39: // right arrow
        // If we have a file (as can be seen from the url property), we may move to the download button.
        if (component.state.roleFileName)
        {
          event.preventDefault();
          event.stopPropagation();      
          component.focusDownload();
          break;
        }
    }
  }

  handleKeyDownInEditable(event : React.KeyboardEvent)
  {
    const component = this;
    let parsedPropertyValue = undefined;
    let fileNameOnProps = undefined;
    switch(event.keyCode){
      // case 9: // horizontal tab; vertical tab is 11
      case 39: // right arrow
        if (component.state.selectedField == FILENAME)
        {
          event.preventDefault();
          event.stopPropagation();      
          // the fileName field saves its value on losing focus; move focus to the other field
          if (component.state.roleFileName)
          {
            component.focusDownload();
          }
          else
          {
            component.focusUpload();
          }
        }
        else if (component.state.selectedField == DOWNLOAD)
        {
          event.preventDefault();
          event.stopPropagation();      
          component.focusUpload();
        }
        else if (component.state.selectedField == UPLOAD)
        {
          event.preventDefault();
          event.stopPropagation();      
          component.focusFileName();
        }
        break;

      case 13: // Enter
        event.preventDefault();
        event.stopPropagation();      

        if (component.props.propertyValues?.values[0] && component.props.roleId)
        {
          parsedPropertyValue = JSON.parse(component.props.propertyValues.values[0]);
          fileNameOnProps = parsedPropertyValue.fileName
        
          if (component.state.fileName !== fileNameOnProps && 
              this.reportValidity(event.target as HTMLInputElement, i18next.t("fileName_invalid", {ns: 'preact'})) )
          {
            // If state.fileName is not equal to the value stored in the property value, just set the property value.
            // We know the file has not been uploaded AFTER changing the fileName, because we save the file and property
            // immediately on uploading.
            return PDRproxy.then( pproxy => 
              {
                pproxy
                  // Construct and save the property's compound value.
                  .setProperty(
                    component.props.roleId!,
                    component.props.serialisedProperty.id,
                    JSON.stringify(
                      { fileName: component.state.fileName
                      , propertyType: component.props.serialisedProperty.id
                      , mimeType: component.state.mimeType
                      , database: component.state.database
                      , roleFileName: component.state.roleFileName
                      }) as ValueT,
                    component.props.myRoletype )
                  .then( () => 
                    // Change state (previousState is still EMPTY). 
                    // If a new file had been uploaded, it is registered in state already.
                    component.setState({state: FILLED}) )
                  .catch(e => UserMessagingPromise.then( um => 
                    um.addMessageForEndUser(
                      { title: i18next.t("saveFile_title", { ns: 'preact' }) 
                      , message: i18next.t("saveFile_message", {ns: 'preact'})
                      , error: e.toString()
                      })))
              } );
            }
        }
        break;

      case 27: // Escape
        // Discard changes (revert to saved values, if any)
        event.preventDefault();
        event.stopPropagation();   
        // previousState is still EMPTY.
        if (component.props.propertyValues?.values[0])
        {
          parsedPropertyValue = JSON.parse(component.props.propertyValues.values[0]);
          component.setState(
            { state: FILLED
            , fileName: parsedPropertyValue.fileName || ""
            , mimeType: parsedPropertyValue.mimeType || ""
            , database: parsedPropertyValue.database
            , roleFileName: parsedPropertyValue.roleFileName
          });  
        }

        break;
      }
  }

  // Called on dropping a file or uploading it.
  // Sends the file to the PDR.
  // Stores the property value.
  // STATECHANGES: sets state.database
  // Returns a promise.
  saveFileAndProperty(theFile : File)
  {
    const component = this;
    return PDRproxy.then( pproxy => 
      {
        return pproxy
          // Save a serialised PerspectivesFile shape and the file in a single call:
          .saveFile(
            { fileName: theFile.name
            , propertyType: component.props.serialisedProperty.id
            , mimeType: component.mapMimeType( theFile.type, theFile.name )
            , database: undefined
            // By construction we know that the roleId is present.
            , roleFileName: component.props.roleId!
            },
            theFile,
            component.props.myRoletype
          )
          .then( perspectivesFile => 
            {
              return component.setState({ database: perspectivesFile.database });
            })
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("saveFile_title", { ns: 'preact' }) 
              , message: i18next.t("saveFile_message", {ns: 'preact'})
              , error: e.toString()
              })))
      } );
  }

  // Returns a promise for the file.
  // Doesn't change state.
  // Only call when we have a value in propertyValues that includes a value for database!
  retrieveFile()
  {
    const component = this; 
    return PDRproxy
      .then( pproxy => pproxy.getFile( component.props.roleId!, component.props.serialisedProperty.id ) )
      .catch(e => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("retrieveFile_title", { ns: 'preact' }) 
          , message: i18next.t("retrieveFile_message", {ns: 'preact'})
          , error: e.toString()
          })));
  }

  // Focus on the fileName field. The mimeType field will save its value automatically in state.
  focusFileName()
  {
    this.setState({selectedField: FILENAME});
    const fileNameElement = document.getElementById(this.props.serialisedProperty.id + '_fileName');
    if (fileNameElement) {
      fileNameElement.focus();
    }
  }
  
  // Focus on the mimeType field. The fileName field will save its value automatically in state.
  focusMimeType()
  {
    this.setState({selectedField: MIMETYPE});
    const mimeTypeElement = document.getElementById(this.props.serialisedProperty.id + '_mimeType');
    if (mimeTypeElement) {
      mimeTypeElement.focus();
    }
  }

  // Focus on the upload button.
  focusUpload()
  {
    this.setState({selectedField: UPLOAD});
    const uploadButton =
      document.getElementById(this.props.serialisedProperty.id + '_upload');
    if (uploadButton) {
      uploadButton.focus();
    }
  }

  // Focus on the download button.
  focusDownload()
  {
    this.setState({selectedField: DOWNLOAD});
    const downloadButton =
      document.getElementById(this.props.serialisedProperty.id + '_download');  
    if (downloadButton) {
      downloadButton.focus();
    }
  }

  // Triggers handlFile; doesn't change state by itself.
  upload(event: React.KeyboardEvent | React.MouseEvent)
  {
    const component = this;
    function doit()
    {
      event.preventDefault();
      event.stopPropagation();
      const selectedFile = document.getElementById(component.props.serialisedProperty.id + '_selectedFile');
      if (selectedFile)
      {
        selectedFile.click();   
      }
    }
    if (event instanceof KeyboardEvent && event.code === "Space") {
      doit();
    }
    if (event.type == "click")
    {
      doit();
    }
  }

  // STATECHANGES: sets fileName, mimeType and state.
  handleFile(theFile : File | null)
  {
    const component = this;
    if ( theFile )
    {
      this.saveFileAndProperty( theFile ).then( () => 
        this.setState(
          { fileName: theFile.name
          , mimeType: component.mapMimeType( theFile.type, theFile.name )
          , state: FILLED}));
    }
  }

  mapMimeType( mime : string, fileName : string) : string
  {
    if ( fileName.match(/\.arc/))
    {
      return "text/arc";
    }
    else 
    {
      return mime;
    }
  }

  // This function doesn't change state.
  download(event : React.KeyboardEvent | React.MouseEvent)
  {
    const component = this;
    function doit()
    {
      event.stopPropagation();
      event.preventDefault();
      component.retrieveFile()
        .then( file => 
          {
            if (file)
            {
              const element = document.createElement('a');
              const url = window.URL.createObjectURL(file);
              element.style.display = 'none';
              element.href = url;
              element.setAttribute('download', component.state.fileName!);
              document.body.appendChild(element);  
              element.click();
              document.body.removeChild(element);
              window.URL.revokeObjectURL(url);
            }
          });
    }
    if (event instanceof KeyboardEvent && event.code === "Space") {
      doit();
    }
    if ( this.state.roleFileName && event.type == "click")
    {
      doit();
    }
  }

  // The event should have the input element as target. Returns true iff no constraints are violated.
  reportValidity(el : HTMLInputElement, message : string)
  {
    // A ValidityState object. See: https://developer.mozilla.org/en-US/docs/Web/API/ValidityState
    const validity = el.validity;
    if (validity.patternMismatch)
    {
      // We now expect a pattern in the perspective.
      el.setCustomValidity( message );
    }
    else
    {
      el.setCustomValidity( "" );
    }
    return el.reportValidity();
  }

  fileIsImage()
  {
    return this.state.mimeType && this.state.mimeType.match(/image/);
  }

  fileIsArc()
  {
    return this.state.mimeType && this.state.mimeType.match(/text\/arc/);
  }

  render()
  {
    const component = this;
    // KEEP THIS CODE ALIGNED WITH smartfieldcontrol.js.
    // `patternFacet` will be an object of this shape (or undefined):
    // { regex: PropTypes.string.isRequired
    // , label: PropTypes.string.isRequired}
    // `pattern` is the string that represents just the regex, no flags.
    // label has the shape /regex/flags.
    // flags will be ignored.
    const patternFacet = component.props.serialisedProperty.constrainingFacets.pattern;
    let pattern = "[^\./]+\/[^\./]+" // default pattern;
    if ( patternFacet )
    {
      const match = patternFacet.regex.match(/\/(.*)\//);
      pattern = match ? match[1] : "";
    }

    switch (component.state.state) {
      case READONLY:
        return (
          <div onKeyDown={e => component.handleKeyDownInReadOnly(e)} tabIndex={component.state.roleFileName ? -1 : 0}>
            <Row>
              {
                  component.fileIsImage() && component.state.roleFileName ?
                    <AsyncImage roleId={ component.props.roleId! } propId={ component.props.serialisedProperty.id } fileName={component.state.fileName!}/>
                    :
                    <>
                      <Col lg="3">
                        <Form.Control readOnly value={ component.state.fileName } tabIndex={-1} size="sm"/>
                      </Col>
                      <Col lg="2">
                        <Form.Control readOnly value={ component.state.mimeType } tabIndex={-1} size="sm"/>
                      </Col>
                    </>
                }
              <Col lg="1">
              <div style={{display: "flex"}}>
                <div 
                  style={{flexShrink: 1, opacity: component.state.roleFileName ? 1 : 0.5}}
                  id={component.props.serialisedProperty.id + '_download'}
                  onClick={ e => component.download(e) }
                  onKeyDown={ e => component.download(e) }
                  tabIndex={component.state.roleFileName ? 0 : -1}
                  >
                  <DownloadIcon 
                    aria-label={ i18next.t("perspectivesFile_download", { ns: 'preact' }) }
                    size='medium'
                  />
                </div>
              </div>
            </Col>
            </Row>
          </div>);

      case FILLED:
        return (
          <div onKeyDown={e => component.handleKeyDownInFilled(e)} tabIndex={component.state.roleFileName ? -1 : 0}>
            <Row>
              {
                component.fileIsImage() && component.state.fileName ?
                  <AsyncImage roleId={ component.props.roleId! } propId={ component.props.serialisedProperty.id } fileName={component.state.fileName}/>
                  :
                  component.fileIsArc() && component.state.fileName ?
                  <ArcViewer roleId={ component.props.roleId! } propId={ component.props.serialisedProperty.id } fileName={component.state.fileName}/>
                  :
                  <>
                    <Col lg="3">
                      <Form.Control readOnly value={ component.state.fileName } tabIndex={-1} size="sm"/>
                    </Col>
                    <Col lg="2">
                      <Form.Control readOnly value={ component.state.mimeType } tabIndex={-1} size="sm"/>
                    </Col>
                  </>
              }
              {/* If the file is an image, display that instead of the next two columns. */}
              <Col lg="1">
              <div style={{display: "flex"}}>
                <div 
                  style={{flexShrink: 1, opacity: component.state.roleFileName ? 1 : 0.5}}
                  id={component.props.serialisedProperty.id + '_download'}
                  onClick={ e => component.download(e) }
                  onKeyDown={ e => component.download(e) }
                  tabIndex={component.state.roleFileName ? 0 : -1}
                  >
                  <DownloadIcon 
                    aria-label={ i18next.t("perspectivesFile_download", { ns: 'preact' }) }
                    size='medium'
                  />
                </div>
              </div>
            </Col>
            </Row>
          </div>);
    
      case EDITABLE:
        return (
          <div 
          >
            <Row
                onKeyDown={e => component.handleKeyDownInEditable(e)}
                onDragOver={ev => ev.preventDefault()}
                onDragEnter={ev => (ev.target as HTMLElement).classList.add("border-primary", "border") }
                onDragLeave={ev => (ev.target as HTMLElement).classList.remove("border-primary", "border")}
                onDrop={ (ev) => 
                  {
                    component.handleFile( ev.dataTransfer.files.item(0) );
                    (ev.target as HTMLElement).classList.remove("border-primary", "border");
                    ev.preventDefault();
                    ev.stopPropagation();
                  }}
            >
              <Col lg="3">
                <Form.Control
                id={component.props.serialisedProperty.id + '_fileName'}
                tabIndex={0}
                aria-label={ i18next.t("perspectivesFile_fileName", {ns: 'preact'}) }
                value={ component.state.fileName }
                onFocus={ () => component.setState({selectedField: FILENAME})}
                onChange={e => component.setState({fileName: e.target.value}) }
                onBlur={e => component.setState({fileName: e.target.value}) }
                type="text"
                required={true}
                pattern="[^\./]+\.[^\./]+"
                placeholder={ i18next.t("perspectivesFile_fileName_label", { ns: 'preact' }) }
                size="sm"
                  />
            </Col>
            <Col lg="2">
              <Form.Control 
                readOnly 
                value={ component.state.mimeType } 
                tabIndex={-1}
                size="sm"
                />
            </Col>
            <Col lg="1">
              <div style={{display: "flex"}}>
                <div 
                  style={{flexShrink: 1, opacity: component.state.roleFileName ? 1 : 0.5}}
                  id={component.props.serialisedProperty.id + '_download'}
                  onClick={ e => component.download(e) }
                  onKeyDown={ e => component.download(e) }
                  tabIndex={-1}
                  >
                  <DownloadIcon 
                    aria-label={ i18next.t("perspectivesFile_download", { ns: 'preact' }) }
                    size='medium'
                  />
                </div>
                <div 
                  style={{flexShrink: 1}}
                  id={component.props.serialisedProperty.id + '_upload'}
                  onClick={ e => component.upload(e) }
                  onKeyDown={ e => component.upload(e) }
                  tabIndex={-1}
                  >
                  <UploadIcon 
                      aria-label={ i18next.t("perspectivesFile_upload", { ns: 'preact' }) }
                      size='medium'/>
                </div>
              </div>
            </Col>
            </Row>
            <input 
              type="file" 
              id={component.props.serialisedProperty.id + '_selectedFile'} 
              style={{display: "none"}} 
              onChange={ev => {
                if (ev.target.files) {
                  component.handleFile(ev.target.files.item(0));
                }
              }}
            />
          </div>);

      // EMPTY is the default.
      case EMPTY:
      default:
        return (
          <div>
            <Row
              onKeyDown={e => component.handleKeyDownInEmpty(e)}
              onDragOver={ev => ev.preventDefault()}
              onDragEnter={ev => (ev.target as HTMLElement).classList.add("border-primary", "border") }
              onDragLeave={ev => (ev.target as HTMLElement).classList.remove("border-primary", "border")}
              onDrop={ ev => 
                {
                  component.handleFile( ev.dataTransfer.files.item(0) );
                  (ev.target as HTMLElement).classList.remove("border-primary", "border");
                  ev.preventDefault();
                  ev.stopPropagation();
                }}
            >
              <Col lg="3">
                <Form.Control
                id={component.props.serialisedProperty.id + '_fileName'}
                aria-label={ i18next.t("perspectivesFile_fileName", {ns: 'preact'}) }
                value={ component.state.fileName }
                onFocus={ () => component.setState({selectedField: FILENAME})}
                onChange={e => component.setState({fileName: e.target.value}) }
                type="text"
                required={true}
                pattern="[^\./]+\.[^\./]+"
                size="sm"
                placeholder={ i18next.t("perspectivesFile_fileName_label", { ns: 'preact' }) }
                tabIndex={0}
                />
              </Col>
              <Col lg="2">
                <Form.Control
                  id={component.props.serialisedProperty.id + '_mimeType'}
                  aria-label={ i18next.t("perspectivesFile_mimeType", {ns: 'preact'}) }
                  value={ component.state.mimeType }
                  onFocus={ () => component.setState({selectedField: MIMETYPE})}
                  onChange={e => component.setState({mimeType: e.target.value}) }
                  type="text"
                  required={true}
                  pattern={ pattern }
                  size="sm"
                  placeholder={ i18next.t("perspectivesFile_mimeType_label", { ns: 'preact' }) }
                  tabIndex={-1}
                  />
              </Col>
              <Col lg="1">
                <div style={{display: "flex"}}>
                  <div 
                    style={{flexShrink: 1}}
                    id={component.props.serialisedProperty.id + '_upload'}
                    onClick={ e => component.upload(e) }
                    onKeyDown={ e => component.upload(e) }
                    tabIndex={-1}
                    >
                    <UploadIcon 
                        aria-label={ i18next.t("perspectivesFile_upload", { ns: 'preact' }) }
                        size='medium'/>
                  </div>
                </div>
              </Col>
            </Row>
            <input 
              type="file" 
              id={component.props.serialisedProperty.id + '_selectedFile'} 
              style={{display: "none"}} 
              onChange={ ev => 
              {
                if (ev.target.files)
                {
                  component.handleFile(ev.target.files.item(0))
                }
              }}
            />
          </div>);
    }
  }
}
