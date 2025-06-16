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
/**
 * Alert component to display a modal with a message.
 *
 * @param {Object} props - The properties object.
 * @param {string} props.title - The title of the alert.
 * @param {string} props.message - The message to display in the alert.
 * @param {boolean} props.show - Whether the alert is visible or not.
 * @param {function} props.close - Function to call when closing the alert.
 * @returns {JSX.Element} The rendered Alert component.
 */
import { Button, Modal } from "react-bootstrap";


interface AlertProps {
  title: string;
  message: string;
  show: boolean;
  close: () => void;
}

const Alert: React.FC<AlertProps> = ({ title, message, show, close }) => {
  function handleClose() {
    close();
  }

  return (
    <Modal show={show} onHide={handleClose} backdrop="static">
      <Modal.Header closeButton>
        <Modal.Title>{title}</Modal.Title>
      </Modal.Header>
      <Modal.Body>{message}</Modal.Body>
      <Modal.Footer>
        <Button variant="primary" onClick={handleClose}>
          Close
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default Alert;
