import { promises as fs } from 'fs';

import { ensureBuildMeta } from './buildMeta.js';

const { buildId, buildPath } = await ensureBuildMeta({ refresh: true });

// See: https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/How_to/Define_app_icons#create_the_necessary_icon_sizes
const macIcons = ["512.png", "256.png", "128.png", "32.png", "16.png"].map( 
  function(icon) 
  {
    const size = icon.replace(".png", "");
    return {
      "src": `${buildPath}AppImages/ios/` + icon, // Update to match your base URL
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

console.log(`Build id: ${buildId}`);