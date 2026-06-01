import { promises as fs } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const packageDir = path.resolve(__dirname, '..');
const sharedWorkerDir = path.resolve(packageDir, '../perspectives-sharedworker');
const publicAssetsDir = path.join(packageDir, 'public/assets');

const sharedWorkerFiles = [
  'perspectives-sharedworker.js',
  'perspectives-sharedworker.js.map',
];

async function syncSharedWorkerAsset(fileName) {
  const sourcePath = path.join(sharedWorkerDir, 'dist', fileName);
  const targetPath = path.join(publicAssetsDir, fileName);
  await fs.copyFile(sourcePath, targetPath);
}

await fs.mkdir(publicAssetsDir, { recursive: true });
await Promise.all(sharedWorkerFiles.map(syncSharedWorkerAsset));

console.log('Synced shared worker assets to public/assets');