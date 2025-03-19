import React, { Children } from "react";
import { bool, node, oneOf, string } from "prop-types";
import classNames from "classnames";
import { FC, ReactNode, ButtonHTMLAttributes } from "react";

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  children?: ReactNode;
  className?: string;
  icon?: ReactNode;
  border?: boolean;
  labelPosition?: "left" | "right";
}
export const Button: FC<ButtonProps> = ({
  children = undefined,
  className = "",
  icon = undefined,
  border = false,
  labelPosition = undefined,
  ...rest
} : ButtonProps) => {
  const cName = `cs-button`;

  const lPos = typeof labelPosition !== "undefined" ? labelPosition : "right";
  const labelPositionClassName =
    Children.count(children) > 0 ? `${cName}--${lPos}` : "";
  const borderClassName = border === true ? `${cName}--border` : "";
  return (
    <button
      {...rest}
      className={classNames(
        cName,
        labelPositionClassName,
        borderClassName,
        className
      )}
    >
      {lPos === "left" && children}
      {icon}
      {lPos === "right" && children}
    </button>
  );
};

export default Button;