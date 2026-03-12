// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

export type LogLevel = 'error' | 'warn' | 'info' | 'debug';

const LEVELS: Record<LogLevel, number> = {
  error: 0,
  warn: 1,
  info: 2,
  debug: 3,
};

let currentLevel: LogLevel = 'info';

export function setLogLevel(level: LogLevel): void {
  currentLevel = level;
}

function shouldLog(level: LogLevel): boolean {
  return LEVELS[level] <= LEVELS[currentLevel];
}

function timestamp(): string {
  return new Date().toISOString();
}

export const logger = {
  error(message: string, ...args: unknown[]): void {
    if (shouldLog('error')) {
      console.error(`[${timestamp()}] ERROR ${message}`, ...args);
    }
  },
  warn(message: string, ...args: unknown[]): void {
    if (shouldLog('warn')) {
      console.warn(`[${timestamp()}] WARN  ${message}`, ...args);
    }
  },
  info(message: string, ...args: unknown[]): void {
    if (shouldLog('info')) {
      console.info(`[${timestamp()}] INFO  ${message}`, ...args);
    }
  },
  debug(message: string, ...args: unknown[]): void {
    if (shouldLog('debug')) {
      console.debug(`[${timestamp()}] DEBUG ${message}`, ...args);
    }
  },
};
