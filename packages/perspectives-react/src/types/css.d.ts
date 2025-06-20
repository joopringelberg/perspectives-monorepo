declare module '*.css' {
  const content: { [className: string]: string };
  export default content;
}

// Specifically handle chatscope styles
declare module '@chatscope/chat-ui-kit-styles/dist/default/styles.min.css' {
  const content: any;
  export default content;
}