import { promises as fs } from 'fs';

// Create build.json if it doesn't exist
try {
  await fs.access("./build.json");
} catch (err) {
  await fs.writeFile("./build.json", JSON.stringify({build: 0}));
}

const {build, buildPath} = JSON.parse(await fs.readFile("./build.json", { encoding: "utf-8" }));

// See: https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/How_to/Define_app_icons#create_the_necessary_icon_sizes
const macIcons = ["512.png", "256.png", "128.png", "32.png", "16.png"].map( 
  function(icon) 
  {
    const size = icon.replace(".png", "");
    return {
      "src": `${buildPath}/AppImages/ios/` + icon, // Update to match your base URL
      "sizes": size + "x" + size,
      "type": "image/png"
    }
  });

const manifest = {
  "name": "MyContexts",
  "short_name": "MyContexts",
  "icons": macIcons,
  "start_url": buildPath,
  "display": "standalone",
  "background_color": "#ffffff",
  "theme_color": "#0275d8"
}

// Write manifest directly to the public directory
await fs.writeFile('./public/perspectives.webmanifest', JSON.stringify(manifest, null, 2))
  .then(() => {
    console.log("Manifest written to public directory");
  })
  .catch(err => {
    console.error("Error writing manifest:", err);
  });

// Update build number
await fs.writeFile("./build.json", JSON.stringify({build: build + 1, buildPath: buildPath}))
  .then(() => {
    console.log(`Build increased to ${build + 1}`);
  })
  .catch(err => {
    console.error("Error updating build number:", err);
  });