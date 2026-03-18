#!/usr/bin/env node
// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import * as path from 'path';
import { loadConfig, validateConfig } from './config';
import { setLogLevel } from './logger';
import { logger } from './logger';
import { TCP } from './tcp';

// ---------------------------------------------------------------------------
// CLI entry point
// ---------------------------------------------------------------------------

const USAGE = `
perspectives-tcp – Transaction Collection Point for the Perspectives Distributed Runtime

Usage:
  perspectives-tcp [OPTIONS]

Options:
  --config <path>   Path to the JSON configuration file (default: ./config.json)
  --schema          Print the SQL CREATE TABLE script and exit (does not start the consumer)
  --help            Show this help text

Examples:
  perspectives-tcp --config /etc/tcp/config.json
  perspectives-tcp --schema --config ./my-config.json
`.trim();

async function main(): Promise<void> {
  const args = process.argv.slice(2);

  if (args.includes('--help') || args.includes('-h')) {
    console.log(USAGE);
    process.exit(0);
  }

  // Resolve config path
  const configIndex = args.indexOf('--config');
  const configPath =
    configIndex !== -1 && args[configIndex + 1]
      ? args[configIndex + 1]
      : path.join(process.cwd(), 'config.json');

  const printSchemaOnly = args.includes('--schema');

  // Load and validate configuration
  let config;
  try {
    config = loadConfig(configPath);
    validateConfig(config);
  } catch (err) {
    console.error(`Failed to load configuration from "${configPath}":`, err);
    process.exit(1);
  }

  // Apply log level
  setLogLevel(config.logLevel ?? 'info');
  logger.info(`Loaded configuration from "${configPath}"`);

  const tcp = new TCP(config);

  if (printSchemaOnly) {
    const sql = await tcp.printSchema();
    console.log(sql);
    process.exit(0);
  }

  // Graceful shutdown
  const shutdown = async (): Promise<void> => {
    logger.info('Received shutdown signal – stopping …');
    try {
      await tcp.stop();
    } catch (err) {
      logger.error('Error during shutdown:', err);
    }
    process.exit(0);
  };
  process.on('SIGINT', () => {
    shutdown().catch((err) => {
      console.error('Unhandled error during shutdown:', err);
      process.exit(1);
    });
  });
  process.on('SIGTERM', () => {
    shutdown().catch((err) => {
      console.error('Unhandled error during shutdown:', err);
      process.exit(1);
    });
  });

  // Start
  await tcp.start();
}

main().catch((err) => {
  console.error('Unhandled error:', err);
  process.exit(1);
});
