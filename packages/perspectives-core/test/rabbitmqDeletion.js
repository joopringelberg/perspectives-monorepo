// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { createServer } from "node:http";

export const startServer = () => new Promise((resolve, reject) => {
  const server = createServer((request, response) => {
    const match = /^\/(\d+)\/api\/(?:users\/test-delete-user|queues\/(?:test|mycontexts)\/test-delete-queue)$/.exec(request.url);
    const authorization = `Basic ${Buffer.from("test-admin:test-password").toString("base64")}`;
    if (!match || request.method !== "DELETE" || request.headers.authorization !== authorization) {
      response.writeHead(400);
      response.end();
      return;
    }
    const status = Number(match[1]);
    if (status === 0) {
      request.socket.destroy();
      return;
    }
    response.writeHead(status);
    response.end();
  });
  server.once("error", reject);
  server.listen(0, "127.0.0.1", () => {
    resolve({ server, url: `http://127.0.0.1:${server.address().port}/` });
  });
});

export const stopServer = server => () => new Promise((resolve, reject) => {
  server.close(error => error ? reject(error) : resolve());
});
