// scripts/update-spago-comments.js
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');

// Find all spago.yaml files
const findSpagoFiles = (dir) => {
  const results = [];
  const files = fs.readdirSync(dir);
  
  for (const file of files) {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);
    
    if (stat.isDirectory()) {
      results.push(...findSpagoFiles(filePath));
    } else if (file === 'spago.yaml') {
      results.push(filePath);
    }
  }
  
  return results;
};

// Get package versions from package.json files
const getPackageVersions = () => {
  const versions = {};
  const packagesDir = path.join(__dirname, '../packages');
  const packageDirs = fs.readdirSync(packagesDir);
  
  for (const dir of packageDirs) {
    const packageJsonPath = path.join(packagesDir, dir, 'package.json');
    if (fs.existsSync(packageJsonPath)) {
      const { name, version } = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
      versions[name] = version;
      versions[dir] = version; // Also store by directory name
    }
  }
  
  return versions;
};

// Update commented git refs in spago.yaml files
const updateSpagoComments = (spagoFile, versions) => {
  console.log(`Updating: ${spagoFile}`);
  
  const content = fs.readFileSync(spagoFile, 'utf8');
  const data = yaml.load(content);
  let updated = false;
  
  if (data.workspace?.extraPackages) {
    for (const [name, config] of Object.entries(data.workspace.extraPackages)) {
      // Handle both styles: full package name or local directory name
      const version = versions[name] || versions[`purescript-${name}`] || versions[`perspectives-${name}`];
      
      if (version && config.path && config.git) {
        // Found a package with path and commented git
        const versionPrefix = version.startsWith('v') ? '' : 'v';
        const newRef = `${versionPrefix}${version}`;
        
        if (config.ref !== newRef) {
          console.log(`  Updating ${name} ref from ${config.ref || 'none'} to ${newRef}`);
          config.ref = newRef;
          updated = true;
        }
      }
    }
  }
  
  if (updated) {
    fs.writeFileSync(spagoFile, yaml.dump(data, { lineWidth: -1 }));
    console.log(`  Updated ${spagoFile}`);
  } else {
    console.log(`  No changes needed for ${spagoFile}`);
  }
};

// Main
const main = () => {
  const versions = getPackageVersions();
  const spagoFiles = findSpagoFiles(path.join(__dirname, '../packages'));
  
  for (const file of spagoFiles) {
    updateSpagoComments(file, versions);
  }
};

main();