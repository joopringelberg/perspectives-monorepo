declare module 'react-bootstrap' {
  export * from 'react-bootstrap';
}

declare module 'react-bootstrap/*' {
  const component: any;
  export default component;
}