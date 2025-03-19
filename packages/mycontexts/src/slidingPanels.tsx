import React, { useState } from "react";
import { Button, Container } from "react-bootstrap";

import { ReactNode } from "react";

import "./slidingPanels.css";

interface SlidingPanelsProps {
  children: [ReactNode, ReactNode];
}

const SlidingPanels = ({ children }: SlidingPanelsProps) => {
  const [isOpen, setIsOpen] = useState(false);

  if (!children || children.length !== 2) {
    console.error("SlidingPanels requires exactly two children: <MainContent /> and <SlidingPanelContent />");
    return null;
  }

  const [mainContent, slidingContent] = children;

  return (
    <Container fluid className="position-relative p-3" style={{ height: "100%" }}>
      {/* Main Panel */}
      <div className="main-panel">
        {mainContent}
        <Button onClick={() => setIsOpen(true)}>Open Cover Panel</Button>
      </div>

      {/* Cover Panel */}
      <div className={`cover-panel ${isOpen ? "open" : ""}`}>
        <div className="content">
          {slidingContent}
          <Button variant="danger" onClick={() => setIsOpen(false)}>Close</Button>
        </div>
      </div>
    </Container>
  );
};

export default SlidingPanels;
