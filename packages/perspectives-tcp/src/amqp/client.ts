// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import amqp, { ChannelModel, Channel, ConsumeMessage } from 'amqplib';
import { RabbitMQConfig } from '../config';
import { logger } from '../logger';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export type MessageHandler = (content: string) => Promise<void>;

// ---------------------------------------------------------------------------
// AMQPClient
// ---------------------------------------------------------------------------

/**
 * Wraps an amqplib connection and provides:
 *   - Auto-reconnect with exponential back-off
 *   - Queue assertion and exchange binding on startup
 *   - Per-message acknowledgement / rejection
 */
export class AMQPClient {
  private config: RabbitMQConfig;
  private cuid: string;
  private handler: MessageHandler;
  private connection: ChannelModel | null = null;
  private channel: Channel | null = null;
  private stopped = false;

  constructor(cuid: string, config: RabbitMQConfig, handler: MessageHandler) {
    this.cuid = cuid;
    this.config = config;
    this.handler = handler;
  }

  // -------------------------------------------------------------------------
  // Public interface
  // -------------------------------------------------------------------------

  /** Connect to RabbitMQ and start consuming messages. */
  async start(): Promise<void> {
    await this.connect();
  }

  /** Gracefully stop consuming and close the connection. */
  async stop(): Promise<void> {
    this.stopped = true;
    try {
      if (this.channel) {
        await this.channel.close();
        this.channel = null;
      }
      if (this.connection) {
        await this.connection.close();
        this.connection = null;
      }
      logger.info('AMQP connection closed');
    } catch (err) {
      logger.warn('Error while closing AMQP connection:', err);
    }
  }

  // -------------------------------------------------------------------------
  // Internal – connection lifecycle
  // -------------------------------------------------------------------------

  private async connect(): Promise<void> {
    const { url, exchange = 'amq.topic', prefetch = 10, reconnectDelayMs = 5000 } = this.config;
    const queueName = this.config.queue ?? this.cuid;
    const routingKey = this.config.routingKey ?? this.cuid;

    while (!this.stopped) {
      try {
        logger.info(`Connecting to RabbitMQ at ${redactUrl(url)} …`);
        const conn = await amqp.connect(url);
        this.connection = conn;
        logger.info('RabbitMQ connection established');

        conn.on('error', (err) => {
          logger.error('RabbitMQ connection error:', err);
        });

        conn.on('close', () => {
          if (!this.stopped) {
            logger.warn('RabbitMQ connection closed unexpectedly – reconnecting …');
            this.channel = null;
            this.connection = null;
            void this.scheduleReconnect(reconnectDelayMs);
          }
        });

        const ch = await conn.createChannel();
        this.channel = ch;
        await ch.prefetch(prefetch);

        // Assert a durable queue named after the TCP's CUID
        await ch.assertQueue(queueName, { durable: true });

        // Bind to the topic exchange (the RabbitMQ STOMP plugin routes Perspectives
        // transactions to `amq.topic` with the recipient's CUID as routing key)
        // NOTE commented this out because the default exchange is sufficient for direct routing to queues named after the CUID, and it avoids the need for exchange declaration and binding permissions in RabbitMQ.
        // await ch.assertExchange(exchange, 'topic', { durable: true });
        await ch.bindQueue(queueName, exchange, routingKey);

        logger.info(
          `Listening on queue "${queueName}" (exchange "${exchange}", key "${routingKey}")`,
        );

        await ch.consume(queueName, (msg) => {
          if (msg) void this.handleMessage(msg);
        });

        // Successful connect – break the retry loop
        return;
      } catch (err) {
        logger.error('Failed to connect to RabbitMQ:', err);
        if (this.stopped) return;
        logger.info(`Retrying in ${reconnectDelayMs} ms …`);
        await sleep(reconnectDelayMs);
      }
    }
  }

  private async scheduleReconnect(delayMs: number): Promise<void> {
    await sleep(delayMs);
    if (!this.stopped) {
      await this.connect();
    }
  }

  // -------------------------------------------------------------------------
  // Internal – message handling
  // -------------------------------------------------------------------------

  private async handleMessage(msg: ConsumeMessage): Promise<void> {
    const content = msg.content.toString('utf-8');
    try {
      await this.handler(content);
      this.channel?.ack(msg);
      logger.debug(`Message acknowledged (deliveryTag=${msg.fields.deliveryTag})`);
    } catch (err) {
      logger.error('Error processing message – rejecting (no requeue):', err);
      // Reject without requeue: a poison message should not loop forever.
      // The operator can configure a Dead Letter Exchange (DLX) in RabbitMQ to
      // capture rejected messages for later inspection.
      this.channel?.nack(msg, false, false);
    }
  }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Remove credentials from an AMQP URL for safe logging.
 * amqp://user:password@host → amqp://***@host
 */
function redactUrl(url: string): string {
  return url.replace(/\/\/[^@]+@/, '//***@');
}
