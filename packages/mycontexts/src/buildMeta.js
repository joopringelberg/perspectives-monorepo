import { promises as fs } from 'fs';
import path from 'path';
import { execSync } from 'child_process';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const packageDir = path.resolve(__dirname, '..');
const repoRoot = path.resolve(packageDir, '../..');
const buildConfigPath = path.join(packageDir, 'build.json');
const buildMetaPath = path.join(packageDir, '.build-meta.json');

function timestampId(date = new Date()) {
  return date.toISOString().replace(/[-:TZ.]/g, '').slice(0, 14);
}

function sanitizeLabel(label) {
  return label
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .slice(0, 40);
}

function gitShortSha() {
  try {
    return execSync('git rev-parse --short HEAD', {
      cwd: repoRoot,
      stdio: ['ignore', 'pipe', 'ignore'],
    }).toString().trim();
  } catch {
    return 'nogit';
  }
}

function gitBranchName() {
  try {
    return execSync('git rev-parse --abbrev-ref HEAD', {
      cwd: repoRoot,
      stdio: ['ignore', 'pipe', 'ignore'],
    }).toString().trim();
  } catch {
    return 'detached';
  }
}

function createBuildId() {
  const externalBuildId = process.env.BUILD_ID?.trim();
  if (externalBuildId) {
    return externalBuildId;
  }

  const label = sanitizeLabel(process.env.BUILD_LABEL?.trim() || gitBranchName()) || 'local';
  return `${label}-${timestampId()}-${gitShortSha()}`;
}

export async function readBuildConfig() {
  try {
    const fileContents = await fs.readFile(buildConfigPath, { encoding: 'utf-8' });
    const { buildPath = '/www/' } = JSON.parse(fileContents);
    return { buildPath };
  } catch (error) {
    if (error.code !== 'ENOENT') {
      throw error;
    }

    const defaultConfig = { buildPath: '/www/' };
    await fs.writeFile(buildConfigPath, JSON.stringify(defaultConfig));
    return defaultConfig;
  }
}

export async function ensureBuildMeta({ refresh = false } = {}) {
  const { buildPath } = await readBuildConfig();

  if (!refresh) {
    try {
      const fileContents = await fs.readFile(buildMetaPath, { encoding: 'utf-8' });
      const { buildId } = JSON.parse(fileContents);
      if (buildId) {
        return { buildId, buildPath };
      }
    } catch (error) {
      if (error.code !== 'ENOENT') {
        throw error;
      }
    }
  }

  const buildMeta = { buildId: createBuildId() };
  await fs.writeFile(buildMetaPath, JSON.stringify(buildMeta));
  return { ...buildMeta, buildPath };
}