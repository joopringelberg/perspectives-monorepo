// BEGIN LICENSE
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

import {FileShareCredentials, PDRproxy, PropertyType, PStorageType, RoleInstanceT, RoleType, ValueT} from "perspectives-proxy";

import React from "react";

import { MainContainer, ChatContainer, MessageList, MessageInput, ConversationHeader, Avatar, VoiceCallButton, VideoCallButton, InfoButton, TypingIndicator, MessageSeparator, SendButton, AttachmentButton, AvatarGroup, ExpansionPanel, Message as MessageComponent, InputToolbox } from '@chatscope/chat-ui-kit-react';


import { createAvatar } from '@dicebear/core';
import { initials } from '@dicebear/collection';

import { File as MFile, Storage } from 'megajs'
import { MicrophoneButton } from "./microphoneButton.js";
import { file2PsharedFile } from "./PSharedFile";
import { cuid2 } from "./cuid";
import { OverlayTrigger, Tooltip } from "react-bootstrap";
import i18next from "i18next";
import PPStorage from "./ppsharedfilestorage";
import { UnboundMarkDownWidget } from "./markdownWidget.js";
import { UserMessagingPromise} from "./userMessaging";
import modelDependencies, { default as ModelDependencies } from "./modelDependencies.js";
import PerspectivesComponent from "./perspectivesComponent";
import '@chatscope/chat-ui-kit-styles/dist/default/styles.min.css';
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';

const ppStorageLimit = __PPSTORAGELIMIT__;
const ppWarningLevel = __PPWARNINGLEVEL__

interface ChatComponentProps {
  externalrole: RoleInstanceT;
  roleinstance: RoleInstanceT;
  roletype: RoleType;
  messagesproperty: PropertyType;
  mediaproperty: PropertyType;
  myroletype: RoleType;
}

interface ChatComponentState {
  messages: any[];
  me: RoleInstanceT;
  storage: any;
  storageType: PStorageType;
  sharedStorageId: RoleInstanceT;
  sharedFileServerKey: string | undefined;
  participants: Participant[];
  warningLevelReached: boolean;
  limitReached: boolean;
  nrOfUploadedFiles: number | undefined;
  mediaRecorder: MediaRecorder | undefined;
}

interface Participant {
  avatar?: ObjectUrl | PSharedFile;
  firstname?: ValueT;
  lastname?: ValueT;
  roleInstance: RoleInstanceT;
}

interface PSharedFile {
  url: string;
  type: string;
  storageType: string;
}

type ObjectUrl = string & { readonly brand: unique symbol };

type Message = 
{ sender: RoleInstanceT
, sentTime: string
, direction: 'incoming' | 'outgoing' | 0 | 1
, position: 'single' | 'first' | 'normal' | 'last' | 0 | 1 | 2 | 3
, payload: string | PSharedFilePayload };

interface PSharedFilePayload { 
  src: PSharedFile | ObjectUrl;
  alt: string;
  width: string;
  mimeType: string };

interface MessagePayload {
  payload: string | { src: PSharedFile, alt: string, width: string };
  sender: any;
  sentTime: number;
}

export class ChatComponent extends PerspectivesComponent<ChatComponentProps, ChatComponentState>
{
  sharedFileStore: { [key: string]: ObjectUrl };
  constructor(props: any)
  {
    super(props);
    let storageType_: PStorageType | undefined, sharedStorageId_: RoleInstanceT | undefined;
    this.sharedFileStore = {};
    const component = this;
    component.state = 
      { messages: []
      , me: "" as RoleInstanceT
      // storage is either the Mega Storage object, or an instance of PPStorage.
      , storage: undefined
      , storageType: "ppstorage"
      // The identifier of the role that represents the storage. Either the DefaultFileServer, or MySharedFileService,
      , sharedStorageId: "DefaultFileServer" as RoleInstanceT
      , sharedFileServerKey: undefined
      , participants: []
      , warningLevelReached: false
      , limitReached: false
      , nrOfUploadedFiles: undefined
      , mediaRecorder: undefined
      };
    PDRproxy
      .then( pproxy => pproxy.getFileShareCredentials() )
      // Mega storage.
      // Notice that for now we assume all storage is Mega. This might change in the future.
      .then( (credentials : FileShareCredentials) => 
        {
          if (credentials)
          {
            switch (credentials.storageType) {
              case "ppstorage":
                // DefaultFileServer
                PDRproxy.then( pproxy => {
                  component.addUnsubscriber(
                    pproxy.getProperty( 
                      credentials.sharedStorageId,
                      // Should be initialised to zero.
                      ModelDependencies.nrOfUploadedFiles,
                      component.props.roletype,
                      values => component.guardUploadLimit( values )
                    ))});
                // Notice that password is overloaded for ppstorage.
                component.setState({storage: new PPStorage( credentials.password ), storageType: credentials.storageType, sharedStorageId: credentials.sharedStorageId} );
                component.initializeAudio();
                break;
            
              // MySharedFileService.
              case "mega":
                // {accountName, password, storageType, sharedStorageId}
                storageType_ = credentials.storageType;
                sharedStorageId_ = credentials.sharedStorageId;
                new Storage({email: credentials.accountName, password: credentials.password, userAgent: "Perspectives", keepalive: false}).ready
                  .then( storage => {
                    if (storageType_ && sharedStorageId_) {
                      component.setState({storage, storageType: storageType_, sharedStorageId: sharedStorageId_});
                    }
                  });
                component.initializeAudio();
                break;

              default:
                component.setState({limitReached: true});
                UserMessagingPromise.then( um => 
                  um.addMessageForEndUser(
                    { title: i18next.t("chatcomponent_nostorageservice_title", { ns: 'preact' }) 
                    , message: i18next.t("chatcomponent_nostorageservice", {ns: 'preact'})
                    , error: ""
                    }))
                break;
            }
          }
        })
      .catch( e => UserMessagingPromise.then( um => 
        um.addMessageForEndUser(
          { title: i18next.t("chatcomponent_nostorageservice_title", { ns: 'preact' }) 
          , message: i18next.t("chatcomponent_nostorageservice", {ns: 'preact'})
          , error: e.toString()
          })))
    // Participants might change during the conversation.
    PDRproxy.then( pproxy => component.addUnsubscriber (
        pproxy.getChatParticipants(
          component.props.roleinstance,
          component.props.messagesproperty,
          (participants => component.augmentParticipants(participants)))));
    component.sharedFileStore = {};
  }

  componentDidUpdate(prevProps: Readonly<ChatComponentProps>, prevState: Readonly<ChatComponentState>)
  {
    const component = this;
    if (component.state.storage && ! prevState.storage)
    {
      // Only initialise the audio when we have storage!
      component.initializeAudio();
    }
  }

  componentDidMount()
  {
      const component = this;
      PDRproxy.then( proxy => 
        {
          component.addUnsubscriber(
            proxy.getProperty( 
              component.props.roleinstance,
              component.props.messagesproperty,
              component.props.roletype,
              values => component.augmentMessages( values ).then( augmentedMessages => component.setState({messages: augmentedMessages}))
            ));
          proxy.getMeInContext( component.props.externalrole ).then( me => me.length > 0 ? component.setState({me: me[0] as RoleInstanceT}) : undefined);
        });
  }

  componentWillUnmount ()
  {
    super.componentWillUnmount();
    Object.values( this.sharedFileStore ).forEach( objectUrl => URL.revokeObjectURL( objectUrl ));
  }

  guardUploadLimit(nrArr : string[])
  {
    const component = this;
    // The property is initialised on 0.
    const nrOfUploadedFiles = parseInt( nrArr[0] );
    
    if ( nrOfUploadedFiles >= ppWarningLevel && nrOfUploadedFiles < ppStorageLimit)
    {
      component.setState({warningLevelReached: true, nrOfUploadedFiles});
    }
    if (nrOfUploadedFiles >= ppStorageLimit )
    {
      component.disablePPStorage(nrOfUploadedFiles);
    }
    else 
    {
      component.setState({nrOfUploadedFiles});
    }
  }

  disablePPStorage(nrOfUploadedFiles : number)
  {
    const component = this;
    PDRproxy.then( pproxy => pproxy.setProperty(
      component.state.sharedStorageId,
      modelDependencies.disabled,
      "true" as ValueT,
      component.props.myroletype
    ));
    component.setState({storage: undefined, storageType: "ppstorage", limitReached: true, nrOfUploadedFiles, mediaRecorder: undefined});
  }

  augmentParticipants(participants: Participant[]): void {
    const component = this;
    Promise.all(
      participants.map(participant => {
        if (participant.avatar) {
          return component.getPsharedFile(participant.avatar as PSharedFile)
            .then(objectUrl => {
              participant.avatar = objectUrl as ObjectUrl;
              return participant;
            });
        } else {
          return new Promise<Participant>((resolve) => {
            participant.avatar = createAvatar(initials, {
              seed: participant.firstname + " " + participant.lastname,
              size: 128
            }).toDataUri() as ObjectUrl;
            resolve(participant);
          });
        }
      })
    ).then(participantsWithAvatars => component.setState({ participants: participantsWithAvatars }));
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////
  ////            SHARING FILES
  /////////////////////////////////////////////////////////////////////////////////////////////////

  handleFileUpload (fileList : FileList) {
    const theFile = fileList.item(0);
    const component = this;
    if (theFile) {
      if (component.state.storage) {
        component.uploadMediaFile(theFile);
      }
      // No storage. By now the client will have received an answer from the PDR, so we can safely assume
      // the user has no private storage and has exceeded the complimentary storage limit.
    }
  };

  uploadMediaFile(theFile: File) {
    const component = this;
    switch (component.state.storageType) {
      case "mega":
        const reader = new FileReader();
        reader.onload = function (event) {
          const arrayBuffer = event.target?.result as ArrayBuffer; // ArrayBuffer from FileReader
          if (arrayBuffer) {
            const uint8Array = new Uint8Array(arrayBuffer); // Create Uint8Array from ArrayBuffer
            component.state.storage.upload({ name: theFile.name, size: theFile.size }, uint8Array).complete
              .then((file: any) => file.link())
              // then add to the map with the mega url as index
              .then((megaUrl: string) => component.cacheAndSendMessage(theFile, megaUrl))
              // TODO: meldt in een dialog.
              .catch((message: string) => alert(message));
          }
        };
        reader.readAsArrayBuffer(theFile);
        break;

      case "ppstorage":
        component.state.storage.upload(theFile)
          .then((megaUrl: string) => component.cacheAndSendMessage(theFile, megaUrl))
          .then(() => PDRproxy.then(pproxy => pproxy.setProperty(
            component.state.sharedStorageId,
            modelDependencies.nrOfUploadedFiles,
            (component.state.nrOfUploadedFiles! + 1).toString() as ValueT,
            component.props.myroletype
          )))
          .catch(({ error, message }: { error: number, message: string }) => {
            switch (error) {
              case MAXFILESREACHED:
                component.disablePPStorage(ppStorageLimit);
                break;

              default:
                UserMessagingPromise.then(um =>
                  um.addMessageForEndUser(
                    {
                      title: i18next.t("ppsharedfilestorage_serviceerror", { ns: 'preact' }),
                      message: message,
                      error: "ppstorage errored with code: " + error
                    }))
                break;
            }
          });
        break;

      default:
        break;
    }
  }

  cacheAndSendMessage(theFile : File, megaUrl : string): void {
    const component = this;
    // then save the mega url in the 'media' PDR property
    PDRproxy.then(pproxy =>
      pproxy.addProperty(
        component.props.roleinstance,
        component.props.mediaproperty,
        JSON.stringify(file2PsharedFile(theFile, component.state.sharedStorageId, component.state.storageType, megaUrl)) as ValueT,
        component.props.myroletype
      )
    );
    // then create an object url and add to the map with the mega url as index
    component.sharedFileStore[megaUrl] = URL.createObjectURL(theFile) as ObjectUrl;
    // then create and send a message with the mega url.
    component.handleSend({
      src: file2PsharedFile(theFile, component.state.sharedStorageId, component.state.storageType, megaUrl),
      alt: theFile.name,
      width: "100%"
    });
  }

  // returns a promise for an object url.
  // Invariant: the sharedFileStore will contain an entry for psharedFile.url holding the object url.
  getPsharedFile(psharedFile: PSharedFile): Promise<ObjectUrl> {
    const component = this;
    if (component.sharedFileStore[psharedFile.url]) {
      return new Promise((resolve) => resolve(component.sharedFileStore[psharedFile.url]));
    } else {
      return component.retrieveFileFromStorage(psharedFile);
    }
  }
  // returns a promise for an object url.
  // Adds the object url to sharedFileStore under key psharedFile.url.
  retrieveFileFromStorage (psharedFile : PSharedFile) : Promise<ObjectUrl> {
    switch (psharedFile.storageType) {
      case "ppstorage":
      case "mega":
        return this.retrieveFileFromMega(psharedFile);

      default:
        // TODO: possibly handle in another way, like provide a default image?
        return new Promise((_, reject) => reject("Unknown storage type"));
    }
  };

  // Adds the object url to sharedFileStore under key psharedFile.url.
  retrieveFileFromMega( {url, type} : PSharedFile) : Promise<ObjectUrl>
  { 
    const component = this;
    // Get the file object from the URL
    const file = MFile.fromURL(url)
    // Load file attributes
    return file.loadAttributes()
      .then(() => file.downloadBuffer({}))
      .then( data => 
        {
          const f = new Blob([data], {type})
          // Now create an object url:
          const objectUrl =  URL.createObjectURL(f)
          component.sharedFileStore[url] = objectUrl as ObjectUrl
          return objectUrl as ObjectUrl;  
        }          
      );
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  ////            AUDIO
  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Creates the mediaRecorder in component state.
  // When the recorder is started and then stopped:
  //    Saves the audio file to Mega.
  //    Adds a megaUrl - objectUrl mapping to the local sharedFileStore.
  //    Saves a PSharedFile.
  //    Sends the recording as a message.
  initializeAudio()
  {
    const component = this;

    // Request access to the user's microphone
    navigator.mediaDevices.getUserMedia({ audio: true })
      .then( stream => 
        {
          // Initialize the chunks.
            let audioChunks: BlobPart[] = [];
          // Initialize MediaRecorder with the stream
          const mediaRecorder = new MediaRecorder(stream);

          // Event listener to collect audio data when available
          mediaRecorder.ondataavailable = (event) => {
              audioChunks.push(event.data);
          };

          // Event listener to handle when recording stops
          mediaRecorder.onstop = () => {
              // Create a Blob from the audio data
              const audioBlob = new Blob(audioChunks, { type: mediaRecorder.mimeType });
              // Clear the audioChunks array for future recordings
              audioChunks = [];
              // Save the audio file
              const audioFile = new File([audioBlob], cuid2(), { type: mediaRecorder.mimeType }); 
              component.uploadMediaFile( audioFile );
          };
          component.setState({mediaRecorder});
        });
  }

  startRecording(ev: React.MouseEvent<HTMLButtonElement>)
  {
    const component = this;
    ev.stopPropagation();
    ev.preventDefault();
    // console.log("Start recording.");
    if (component.state.mediaRecorder){
      component.state.mediaRecorder.start();
    }
  }

  stopRecording(ev : React.MouseEvent<HTMLButtonElement>)
  {
    const component = this;
    ev.stopPropagation();
    ev.preventDefault();
    // console.log("Stop recording.");
    if (component.state.mediaRecorder){
      component.state.mediaRecorder.stop();
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  ////            SEND MESSAGE
  /////////////////////////////////////////////////////////////////////////////////////////////////
  // payload is either a string, or a {src, alt, width} structure where src is a PSharedFile.
  handleSend(payload: string | { src: PSharedFile, alt: string, width: string }) {
    const component = this;
    PDRproxy.then(proxy =>
      proxy.addProperty(
        component.props.roleinstance,
        component.props.messagesproperty,
        JSON.stringify({
          payload,
          sender: component.state.me,
          sentTime: Date.now()
        } as MessagePayload) as ValueT,
        component.props.myroletype
      )
    );
  }
  

  /////////////////////////////////////////////////////////////////////////////////////////////////
  ////            CONSTRUCTING MESSAGES
  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Returns a promise for augmented messages.
  // Add `direction`, `position` and if the payload is an object whose src is a PSharedFile, set the payload src to an object url.
  // {
  //   message: string, WE DON'T USE THIS BUT PAYLOAD INSTEAD!
  //   sentTime: string,
  //   sender: string,
  //   direction: 'incoming' | 'outgoing' | 0 | 1,
  //   position: 'single' | 'first' | 'normal' | 'last' | 0 | 1 | 2 | 3,
  //   type: 'html' | 'text' | 'image' | 'custom',
  //   payload: string | object | allowedChildren([MessageCustomContent])
  // }
  // If payload is not a string, it is an image object {src, alt, width} where src is a PSharedFile object.
  augmentMessages( messages : string[] ) : Promise<any[]>
  {
    const component = this;
    function isPSharedFile(message: Message): boolean {
      return typeof message.payload === 'object' && 'src' in message.payload && 'type' in message.payload.src;
    }
    return Promise.all( messages.map( (messageString, i) => 
    {
      const message = JSON.parse(messageString) as Message;
      const previousMessage = messages[i-1] ? JSON.parse (messages[i-1]) as Message : undefined;
      const nextMessage = messages[i+1] ? JSON.parse (messages[i+1]) as Message : undefined;
      // Make time readable
      message.sentTime = new Date(message.sentTime).toLocaleTimeString();
      // Set direction.
      if (message.sender == component.state.me)
      {
        message.direction = 'outgoing';
      }
      else
      {
        message.direction = 'incoming';
      }
      // Set position.
      if (previousMessage && previousMessage.sender == message.sender)
      {
        // a A
        if (nextMessage && nextMessage.sender == message.sender)  
        {
          // a A a
          message.position = 'normal';
        }
        else
        {
          // a A b
          message.position = 'last';
        }
      }
      // b A
      else if (nextMessage && message.sender == nextMessage.sender)
      {
        // b A a
        message.position = 'first';
      }
      else
      {
        // b A b
        message.position = 'single';
      }
      // Create an image payload where the src is an object url:
      if (isPSharedFile(message))
      {
        const payload = message.payload as PSharedFilePayload;
        const source = payload.src as PSharedFile;
        return component.getPsharedFile( source )
          .then( objectUrl => 
            {
              payload.mimeType = source.type;
              payload.src = objectUrl
              return message
            } )
      }
      else
      {
        return new Promise((resolve) => resolve(message));
      }
    }));
  }

  // message types according to https://chatscope.io/storybook/react/?path=/docs/components-message--docs:
  // type: 'html' | 'text' | 'image' | 'custom'
  // Notice that here we work with payload.mimeType (a mime-type valued property we add to the payload structure that chatscope exclusively uses for images).
  // payload = {type, ...}
  buildMessage( message : Message, i : number)
  {
    const component = this;
    if (typeof message.payload == 'string')
    {
      return (<MessageComponent key={i} model={message} type='text'>
          <MessageComponent.Header sentTime={message.sentTime}/>
          <Avatar name={message.sender} size="sm" src={component.getAvatar(message.sender) as string}/>
        </MessageComponent>);
    }
    if (typeof message.payload == 'object')
    {
      // Now payload.mimeType **MUST** exist.
      if (message.payload.mimeType.match(/^audio/))
      {
        return (<MessageComponent key= {i} model={{
                  direction: message.direction,
                  type: "custom",
                  position: message.position,
                }}>
                  <Avatar name={message.sender} size="sm" src={component.getAvatar(message.sender) as string}/>
                  <MessageComponent.CustomContent>
                    <audio controls>
                      <source src={message.payload.src as string} type={message.payload.mimeType}/>
                    </audio>
                  </MessageComponent.CustomContent>
                </MessageComponent>)
      }
      else if (message.payload.mimeType.match(/^image/))
      {
        return (<MessageComponent key={i} model={message} type='image'>
            <Avatar name={message.sender} size="sm" src={component.getAvatar(message.sender) as string}/>
          </MessageComponent>);
      }
    }
  }

  buildMessageList()
  {
    const component = this;
    const messageList = [];
    for (let index = 0; index < component.state.messages.length; index++) {
      const previousMessage = component.state.messages[index-1];
      const currentMessage = component.state.messages[index];
      if (previousMessage && new Date(previousMessage.sentTime).getDate() < new Date(currentMessage.sentTime).getDate())
      {
        messageList.push( <MessageSeparator content={new Date(currentMessage.sentTime).toLocaleDateString()} /> )
      }
      messageList.push( component.buildMessage(currentMessage, messageList.length));
    }
    return messageList;
  }

  getAvatar( userId : RoleInstanceT)
  {
    return this.state.participants.find( ({roleInstance}) => roleInstance == userId )?.avatar;
  }

  render()
  {
    const renderTooltip = (props : OverlayInjectedProps) => (
      <Tooltip id="recordAudio-tooltip" {...props} show={
        props.show}>
        { i18next.t("chatComponent_recordAudio", {ns: "preact"}) }
      </Tooltip> );
  
    const component = this;
    let audioAvailabe = !!component.state.mediaRecorder;
    return (
      <MainContainer responsive style={{flexDirection: 'column'}}>
           <ChatContainer
            style={{
                height: '100%'
            }}
            >
            <ConversationHeader>
                <ConversationHeader.Content>
                  <AvatarGroup>
                    {
                      component.state.participants.map( ({firstname, avatar}, i) => <Avatar key={i} name={firstname} src={avatar as string} />)
                    }
                  </AvatarGroup>
                </ConversationHeader.Content>
                <ConversationHeader.Actions>
                </ConversationHeader.Actions>
            </ConversationHeader>
            {/* The MessageList should have a fixed height to enable scrolling behaviour. 
            This can be achieved by giving the entire ChatComponent a fixed height.*/}
            <MessageList>{ component.buildMessageList()}</MessageList>
            <MessageInput
              placeholder="Type message here" 
              sendButton={true}
              attachButton={false}
              onSend={text => component.handleSend(text)}
              style={{ flexGrow: 1, borderTop: 0, flexShrink:"initial",  }} />
            <InputToolbox style={{display:"flex", flexDirection:"row", borderTop: "1px dashed #d1dbe4"}}>
              {/* <SendButton style={{fontSize:"1.2em", marginLeft:0, paddingLeft: "0.2em", paddingRight:"0.2em"}} /> */}
                <AttachmentButton 
                  disabled={!audioAvailabe}
                  onClick={ () => document.getElementById('__fileupload__')!.click()} style={{fontSize:"1.2em", paddingLeft: "0.2em", paddingRight:"0.2em"}} 
                />
                <OverlayTrigger
                    placement="left"
                    delay={{ show: 250, hide: 400 }}
                    overlay={renderTooltip}
                  >
                  <MicrophoneButton 
                    disabled={!audioAvailabe}
                    onMouseDown={ (ev : React.MouseEvent<HTMLButtonElement>) => component.startRecording(ev)}
                    onMouseUp={ (ev : React.MouseEvent<HTMLButtonElement>) => component.stopRecording(ev)}
                    style={{fontSize:"1.2em", paddingLeft: "0.2em", paddingRight:"0.2em"}}  
                    />
                </OverlayTrigger>
            </InputToolbox>
          </ChatContainer>
          {
            (component.state.warningLevelReached && !component.state.limitReached) ? 
            <ExpansionPanel title={i18next.t("chatComponent_instruction_title", { ns: 'preact' })} open={true}>
              <UnboundMarkDownWidget markdown={i18next.t("chatComponent_instruction_body1", { ns: 'preact', remainingUploads: ppStorageLimit-ppWarningLevel})} open={true}/>
            </ExpansionPanel>
            : 
            null
          }
          {
            component.state.limitReached ? 
            <ExpansionPanel title={i18next.t("chatComponent_instruction_title", { ns: 'preact' })} open={true} >
              <UnboundMarkDownWidget markdown={i18next.t("chatComponent_instruction_body2", { ns: 'preact' })} open={true}/>
            </ExpansionPanel>
            : 
            null
          }
        <input 
            type="file" 
            id="__fileupload__"
            style={{display: "none"}} 
            onChange={ ev => 
            {
              ev.target.files &&
                component.handleFileUpload(ev.target.files)
            }}
          />
      </MainContainer>
    );
  }
}

  const MAXFILESREACHED = 4;
