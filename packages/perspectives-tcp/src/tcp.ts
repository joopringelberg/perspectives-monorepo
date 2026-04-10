// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import { TCPConfig } from './config';
import { KnexAdapter } from './database/knex-adapter';
import { DatabaseAdapter } from './database/adapter';
import { AMQPClient } from './amqp/client';
import { SchemaGenerator } from './schema/generator';
import { processTransaction } from './transaction/processor';
import { TransactionForPeer } from './types';
import { logger } from './logger';

// ---------------------------------------------------------------------------
// TCP – main orchestrator
// ---------------------------------------------------------------------------

/**
 * The Transaction Collection Point.
 *
 * Responsibilities:
 *   1. Apply the relational schema to the database (create tables if absent).
 *   2. Connect to RabbitMQ and subscribe to the configured queue.
 *   3. For each incoming message:
 *        a. Parse the JSON as a `TransactionForPeer`.
 *        b. Process each `SignedDelta` and persist the data in the database.
 */
export class TCP {
  private config: TCPConfig;
  private db: DatabaseAdapter;
  private amqp: AMQPClient;
  private schemaGenerator: SchemaGenerator;

  constructor(config: TCPConfig) {
    this.config = config;
    this.db = new KnexAdapter(config.database);
    this.schemaGenerator = new SchemaGenerator(this.db);
    this.amqp = new AMQPClient(config.cuid, config.rabbitmq, (msg) =>
      this.onMessage(msg),
    );
  }

  // -------------------------------------------------------------------------
  // Lifecycle
  // -------------------------------------------------------------------------

  /** Initialise the database schema and start the RabbitMQ consumer. */
  async start(): Promise<void> {
    logger.info(`Starting TCP with CUID "${this.config.cuid}"`);

    await this.schemaGenerator.applySchema(this.config.schema.tables);
    await this.schemaGenerator.applyViews(
      this.config.schema.tables,
      this.config.schema.nameMap ?? {},
    );
    await this.amqp.start();

    logger.info('TCP is running and waiting for transactions');
  }

  /** Stop the RabbitMQ consumer and close the database connection. */
  async stop(): Promise<void> {
    logger.info('Stopping TCP …');
    await this.amqp.stop();
    await this.db.close();
    logger.info('TCP stopped');
  }

  // -------------------------------------------------------------------------
  // Schema utilities (callable independently of the consumer loop)
  // -------------------------------------------------------------------------

  /**
   * Print the SQL CREATE TABLE and CREATE VIEW statements for the configured schema.
   * Does NOT apply them to the database.
   */
  async printSchema(): Promise<string> {
    const tableSQL = await this.schemaGenerator.generateSQL(this.config.schema.tables);
    const viewSQL = this.schemaGenerator.generateViewsSQL(
      this.config.schema.tables,
      this.config.schema.nameMap ?? {},
    );
    return viewSQL ? `${tableSQL}\n\n${viewSQL}` : tableSQL;
  }

  // -------------------------------------------------------------------------
  // Message handling
  // -------------------------------------------------------------------------

  private async onMessage(rawJson: string): Promise<void> {
    let transaction: TransactionForPeer;

    try {
      transaction = JSON.parse(rawJson) as TransactionForPeer;
    } catch (err) {
      logger.error('Failed to parse incoming message as JSON:', err);
      throw err; // propagate so the AMQP client can nack
    }

    await processTransaction(transaction, this.db, {
      verifySignatures: this.config.verifySignatures ?? false,
      tables: this.config.schema.tables,
      nameMap: this.config.schema.nameMap ?? {},
    });
  }
}
