// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { createServer } from "node:http";

export const startRepositoryMock = status => () => new Promise((resolve, reject) => {
  const requests = [];
  const server = createServer((request, response) => {
    let body = "";
    request.setEncoding("utf8");
    request.on("data", chunk => { body += chunk; });
    request.on("end", () => {
      requests.push({
        method: request.method,
        body,
        authorization: request.headers.authorization ?? "",
      });
      response.writeHead(status, { "Content-Type": "application/json" });
      response.end(JSON.stringify({ ok: status < 400 }));
    });
  });
  server.once("error", reject);
  server.listen(0, "127.0.0.1", () => {
    resolve({
      url: `http://127.0.0.1:${server.address().port}/models_test`,
      requests: () => [...requests],
      close: () => new Promise((done, failed) => {
        server.close(error => error ? failed(error) : done());
      }),
    });
  });
});
