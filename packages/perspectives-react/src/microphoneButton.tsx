import React from "react";
import { forwardRef } from 'react';
import classNames from "classnames";
import Button from "./button.js";

import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faMicrophone } from "@fortawesome/free-solid-svg-icons";
import { IconProp } from "@fortawesome/fontawesome-svg-core";

interface MicrophoneButtonProps {
  children?: React.ReactNode;
  className?: string;
  [key: string]: any;
}

export const MicrophoneButton = forwardRef<HTMLElement, MicrophoneButtonProps>((props, ref) => {
    const cName = `cs-button--voicecall`;
    const { children, className, ...restProps } = props;

  return (
    <Button
      {...restProps}
      className={classNames(cName, props.className ? props.className : "")}
      icon={<FontAwesomeIcon icon={faMicrophone as IconProp} />}
    >
      {props.children}
    </Button>
  );
});

export default MicrophoneButton;