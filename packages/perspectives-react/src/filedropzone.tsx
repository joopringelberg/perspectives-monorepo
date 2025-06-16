
import { ReactNode } from 'react';
import { Tooltip, OverlayTrigger} from 'react-bootstrap';
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';

// As the real action happens in handleFile as it is presented on the props of FileDropZone,
// an error boundary is of no good here.

interface FileDropZoneProps {
  tooltiptext?: string;
  handlefile: (file: File) => void;
  extension: string;
  collapsenavbar: () => void;
  children: ReactNode;
}

export default function FileDropZone(props: FileDropZoneProps) {
  const renderTooltip = (tooltipProps: OverlayInjectedProps) => (
    <Tooltip id="download-tooltip" {...tooltipProps}>
      {props.tooltiptext}
    </Tooltip>
  );

  function handleKeyDown(event: React.KeyboardEvent<HTMLDivElement>) {
    switch (event.keyCode) {
      case 13: // Enter
      case 32: // space
        document.getElementById('selectedFile')?.click();
        event.preventDefault();
        break;
    }
  }

  function handleClick(event: React.MouseEvent<HTMLDivElement>) {
    document.getElementById('selectedFile')?.click();
  }

  function handleFileSelect(event: React.ChangeEvent<HTMLInputElement>) {
    const fileList = event.target.files;
    if (fileList && fileList.length > 0) {
      handleFileWithExtension(fileList);
    }
  }

  function handleFileWithExtension(fileList: FileList) {
    const theFile = fileList.item(0);
    const r = props.extension.match(/\./) ? new RegExp("\\" + props.extension) : new RegExp("\." + props.extension);
    if (theFile && theFile.name.match(r)) {
      props.handlefile(theFile);
    }
    if (props.collapsenavbar) {
      props.collapsenavbar();
    }
  }

  if (props.tooltiptext) {
    return (
      <OverlayTrigger
        placement="left"
        delay={{ show: 250, hide: 400 }}
        overlay={renderTooltip}
      >
        <div
          onDragOver={ev => ev.preventDefault()}
          aria-describedby="Select a file"
          tabIndex={0}
          className="dropzone text-secondary"
          onDrop={ev => {
            handleFileWithExtension(ev.dataTransfer.files);
            ev.currentTarget.classList.remove("border", "p-3", "border-primary");
            ev.preventDefault();
            ev.stopPropagation();
          }}
          onClick={ev => handleClick(ev)}
          onDragEnter={ev => ev.currentTarget.classList.add("border", "border-primary")}
          onDragLeave={ev => ev.currentTarget.classList.remove("border", "border-primary")}
          onKeyDown={ev => handleKeyDown(ev)}
        >
          <input type="file" id="selectedFile" style={{ display: "none" }} onChange={ev => handleFileSelect(ev)} />
          {props.children}
        </div>
      </OverlayTrigger>
    );
  } else {
    return (
      <div
        onDragOver={ev => ev.preventDefault()}
        aria-describedby="Select a file"
        tabIndex={0}
        className="dropzone text-secondary"
        onDrop={ev => {
          handleFileWithExtension(ev.dataTransfer.files);
          ev.currentTarget.classList.remove("border", "p-3", "border-primary");
          ev.preventDefault();
          ev.stopPropagation();
        }}
        onClick={ev => handleClick(ev)}
        onDragEnter={ev => ev.currentTarget.classList.add("border", "border-primary")}
        onDragLeave={ev => ev.currentTarget.classList.remove("border", "border-primary")}
        onKeyDown={ev => handleKeyDown(ev)}
      >
        <input type="file" id="selectedFile" style={{ display: "none" }} onChange={ev => handleFileSelect(ev)} />
        {props.children}
      </div>
    );
  }
}
