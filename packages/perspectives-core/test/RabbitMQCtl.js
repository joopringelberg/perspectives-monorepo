// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { execFile } from 'child_process';

// Purges a queue on the local RabbitMQ node through rabbitmqctl.
// Returns an Effect (Promise String) with rabbitmqctl's output.
export function purgeQueueImpl(vhost) {
  return function (queueName) {
    return function () {
      return new Promise(function (resolve, reject) {
        execFile('rabbitmqctl', ['purge_queue', '-p', vhost, queueName], function (err, stdout, stderr) {
          if (err) {
            reject(new Error('rabbitmqctl purge_queue ' + queueName + ' failed: ' + (stderr || err.message)));
          } else {
            resolve(stdout.trim());
          }
        });
      });
    };
  };
}

// Runs rabbitmqctl with the given arguments. Returns an Effect (Promise String) with its output.
export function rabbitmqctlImpl(args) {
  return function () {
    return new Promise(function (resolve, reject) {
      execFile('rabbitmqctl', args, function (err, stdout, stderr) {
        if (err) {
          reject(new Error('rabbitmqctl ' + args.join(' ') + ' failed: ' + (stderr || err.message)));
        } else {
          resolve(stdout.trim());
        }
      });
    });
  };
}
