declare module "sharedworker/perspectives-handleclientrequest.js" {
  export default function handleClientRequest(): void;
}

declare let PDRPromise: Promise<void>;

declare const channels: { [key: number]: MessagePort };
declare let channelIndex: number;

/**
 * Function to handle the port to the page that hosts the PDR.
 * This function is passed on by the client in the call configurePDRProxy({pageHostingPDRPort: pageHostingPDRPort})
 * This function returns a MessagePort as documented here: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort.
 * 
 * @param pdr - The PDR object.
 * @returns {MessagePort} - The MessagePort object.
 */
export default function pageHostingPDRPort(pdr: any): MessagePort;