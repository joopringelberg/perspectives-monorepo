const fs = require('fs');
const path = require('path');

// Your custom message to add to all changelogs
const RELEASE_NOTE = `
## Major Changes

This release represents a significant milestone for the Perspectives project:

* Completely new GUI, based on 'who' (plays a role in this context), 'what' (is it about) and 'where' (can you navigate from it to).
* All code is now in a monorepo structure
* Client side code is now in typescript
* Unified versioning across all packages
* Complete integration of incremental changes
* Improved module resolution and dependency handling
* Major architectural improvements for better performance and maintainability

This release is estimated to be at Technology Readiness Level (TRL) 6, meaning it is a fully functional prototype that has been tested in a relevant environment. It is ready for further testing and validation in real-world scenarios.

For more details, see the [release documentation](https://github.com/joopringelberg/perspectives-monorepo/blob/master/RELEASES.md#v300).
`;

// Process all package directories
const packagesDir = path.join(__dirname, '..', 'packages');
const packageDirs = fs.readdirSync(packagesDir);

packageDirs.forEach(packageName => {
  const changelogPath = path.join(packagesDir, packageName, 'CHANGELOG.md');
  
  if (fs.existsSync(changelogPath)) {
    let content = fs.readFileSync(changelogPath, 'utf8');
    
    // Find the position after the version header
    const versionHeaderRegex = /(# 3\.0\.0 \(\d{4}-\d{2}-\d{2}\)\n\n)/;
    const match = content.match(versionHeaderRegex);
    
    if (match) {
      // Insert our custom note after the version header
      const updatedContent = content.replace(
        versionHeaderRegex, 
        `$1${RELEASE_NOTE}\n`
      );
      
      // Write updated content back to file
      fs.writeFileSync(changelogPath, updatedContent);
      console.log(`✅ Updated changelog for ${packageName}`);
    } else {
      console.log(`⚠️ Could not find version header in ${packageName} changelog`);
    }
  } else {
    console.log(`⚠️ No changelog found for ${packageName}`);
  }
});

console.log('All changelogs updated successfully!');