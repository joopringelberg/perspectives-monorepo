export function feedbackHeightLimits(available: number) {
  const min = Math.min(80, Math.max(0, available) / 2);
  return { min, max: Math.max(min, available - 80) };
}

export function setupFeedbackSplitter(panes: HTMLElement, splitter: HTMLElement, panel: HTMLElement): void {
  let drag: { pointerId: number; y: number; height: number } | undefined;

  function resize(height: number) {
    const available = panes.clientHeight - splitter.offsetHeight;
    const { min, max } = feedbackHeightLimits(available);
    const value = Math.min(max, Math.max(min, height));
    panel.style.flexBasis = `${value}px`;
    splitter.setAttribute("aria-valuemin", String(Math.round(min)));
    splitter.setAttribute("aria-valuemax", String(Math.round(max)));
    splitter.setAttribute("aria-valuenow", String(Math.round(value)));
    splitter.setAttribute("aria-valuetext", `Feedback height ${Math.round(value)} pixels`);
  }

  splitter.addEventListener("pointerdown", event => {
    if (!event.isPrimary || event.button !== 0) return;
    event.preventDefault();
    splitter.focus();
    drag = { pointerId: event.pointerId, y: event.clientY, height: panel.getBoundingClientRect().height };
    splitter.setPointerCapture(event.pointerId);
    panes.classList.add("resizing");
  });
  splitter.addEventListener("pointermove", event => {
    if (drag?.pointerId === event.pointerId) resize(drag.height + drag.y - event.clientY);
  });
  const stop = (event: PointerEvent) => {
    if (drag?.pointerId !== event.pointerId) return;
    drag = undefined;
    panes.classList.remove("resizing");
    if (splitter.hasPointerCapture(event.pointerId)) splitter.releasePointerCapture(event.pointerId);
  };
  splitter.addEventListener("pointerup", stop);
  splitter.addEventListener("pointercancel", stop);
  splitter.addEventListener("lostpointercapture", stop);
  splitter.addEventListener("keydown", event => {
    const height = panel.getBoundingClientRect().height;
    const { min, max } = feedbackHeightLimits(panes.clientHeight - splitter.offsetHeight);
    switch (event.key) {
      case "ArrowUp": resize(height + 20); break;
      case "ArrowDown": resize(height - 20); break;
      case "Home": resize(min); break;
      case "End": resize(max); break;
      default: return;
    }
    event.preventDefault();
  });
  new ResizeObserver(() => resize(panel.getBoundingClientRect().height)).observe(panes);
}
