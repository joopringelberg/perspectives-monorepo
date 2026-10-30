// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { Buffer } from "node:buffer";

export const utf8Base64 = text => Buffer.from(text, "utf8").toString("base64");
