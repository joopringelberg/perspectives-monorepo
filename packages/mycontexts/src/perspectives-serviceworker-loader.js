if ("serviceWorker" in navigator) {
  // Register a service worker hosted at the root of the
  // site using the default scope.
  navigator.serviceWorker.register(`perspectives-serviceworker${__BUILD__}.js`).then(
    (registration) => {
      console.log("Perspectives-service worker registration succeeded:", registration);
    },
    (error) => {
      console.error(`Perspectives-service worker registration failed: ${error}`);
    },
  );
} else {
  console.error("Service workers are not supported.");
}
