# Onderzoek CloudAMQP als commerciële RabbitMQ-provider voor BrokerServices

## Scope en werkwijze

Dit document onderzoekt of `model://perspectives.domains#BrokerServices` met de huidige externe functies uit `Perspectives.Extern.RabbitMQ` kan werken op CloudAMQP.

Omdat `www.cloudamqp.com` en `docs.cloudamqp.com` vanuit deze CI-omgeving niet direct resolveerbaar waren, is de analyse gebaseerd op:

1. De huidige PDR-implementatie (`rabbitmq.purs` en `managementAPI.purs`).
2. Publieke, door CloudAMQP onderhouden documentatie in de `cloudamqp/terraform-provider-cloudamqp` repository (README en docs).

## Huidige benodigde functies in de PDR

In `externalFunctions` worden 8 functies gebruikt:

1. `PrepareAMQPaccount`
2. `SetBindingKey`
3. `SetPassword`
4. `DeleteAMQPaccount`
5. `DeleteQueue`
6. `SetPermissionsForAMQPaccount`
7. `StartListening`
8. `SelfRegisterWithRabbitMQ`

Bron: `packages/perspectives-core/src/core/computedValues/rabbitmq.purs`.

De functies 1,2,4,5,6 gebruiken RabbitMQ Management HTTP API-calls (`/api/users`, `/api/permissions`, `/api/bindings`, `/api/queues`) via `Perspectives.AMQP.RabbitMQManagement`.

Bron: `packages/perspectives-core/src/core/amqpTransport/managementAPI.purs`.

## Past dit op CloudAMQP?

### Conclusie (kort)

**Ja, grotendeels wel**, mits we op CloudAMQP-instance niveau met broker-credentials werken (de credentials van de instance) en voor self-registration een eigen service (`/rbsr/`, Rabbit Broker Self Registration endpoint) blijven aanbieden.

### Mapping op functie-niveau

| PDR functie | Nodige capaciteit | CloudAMQP status |
|---|---|---|
| `PrepareAMQPaccount` | User aanmaken + permissions zetten | **Ondersteund** via RabbitMQ broker HTTP API op de instance (provider-to-provider voorbeeld met RabbitMQ provider, incl. credentials uit `cloudamqp_instance`) |
| `SetBindingKey` | Queue binding op exchange (`amq.topic`) | **Ondersteund** via RabbitMQ broker HTTP API |
| `SetPassword` | User password update | **Niet geïmplementeerd in PDR** (functionele gap in eigen code, geen CloudAMQP-blokkade) |
| `DeleteAMQPaccount` | User verwijderen | **Ondersteund** via RabbitMQ broker HTTP API |
| `DeleteQueue` | Queue verwijderen | **Ondersteund** via RabbitMQ broker HTTP API |
| `SetPermissionsForAMQPaccount` | Rechten per user/vhost zetten | **Ondersteund** via RabbitMQ broker HTTP API |
| `StartListening` | STOMP/consume op broker | **Ondersteund** zolang plan/protocol dit toelaat; technisch geen API-gap aangetoond |
| `SelfRegisterWithRabbitMQ` | Server-side registratie-endpoint | **Niet door CloudAMQP geleverd als kant-en-klare feature.** Blijft eigen servicepatroon (`/rbsr/`). |

Belangrijke aanwijzing: CloudAMQP-documentatie vermeldt expliciet dat je voor user/vhost-beheer de onderliggende broker HTTP API gebruikt met credentials uit `cloudamqp_instance` (bijv. via de Terraform RabbitMQ provider `cyrilgdn/rabbitmq`, dus niet via alleen de CloudAMQP customer API).

## Sub-accounts / user-model

### Vraag
Kunnen we het model voortzetten waarin elke PDR-gebruiker een RabbitMQ-user is?

### Antwoord
**Ja, technisch wel**, omdat userbeheer op broker-niveau beschikbaar is via de RabbitMQ HTTP API (create/delete user, permissions).

### Ontwerpkeuze

- **Voorkeur**: behoud huidig model (één RabbitMQ user per PDR gebruiker, één queue per gebruiker).
- **Alternatief** (één technische service-user): alleen nodig als CloudAMQP-plan hard limieten oplegt die het user-per-user model blokkeren.

### Risico

Plan-afhankelijke limieten (max users/connections/channels/queues) zijn niet in deze code-analyse vastgesteld en moeten bij providerselectie contractueel bevestigd worden.

## Metering per user en/of queue

### Wat is aantoonbaar beschikbaar

- CloudAMQP ondersteunt alarms en metric-integraties.
- Alarms ondersteunen queue-gerichte voorwaarden (`queue_regex`, `vhost_regex`, `message_type`).
- Metric-integratie ondersteunt `queue_allowlist` en `vhost_allowlist`.

Daarmee is **metering op queue/vhost-niveau** praktisch haalbaar.

### Per-user metering

- In de gevonden CloudAMQP Customer/Instance API-documentatie is geen expliciete “billing/metering per RabbitMQ user”-feature aangetoond.
- Praktisch advies: fair-use baseren op queue/vhost-metrics (met ons model: 1 user : 1 queue) of zelf afleiden via broker management-statistieken.

## Europa / datalokatie

CloudAMQP ondersteunt meerdere regio’s en cloudproviders; voorbeelden bevatten Europese regio’s (zoals `amazon-web-services::eu-central-1`, `google-compute-engine::europe-west1`).

Ontwerpadvies:

1. Kies expliciet EU-regio bij instance-creatie.
2. Leg in contract/SLA vast dat productie-instances alleen in EU-regio’s draaien.
3. Bevestig backup/log/telemetrie-locatie apart (niet automatisch af te leiden uit enkel region-selectie).

## Aanbevolen ontwerp voor Perspectives

1. **Behoud huidige BrokerServices API** voor de 8 functies (geen modelwijziging nodig voor basiswerking).
2. **Implementeer `SetPassword` echt** (nu no-op) zodat lifecycle compleet is.
3. **Behoud eigen self-registration service** (`/rbsr/`) als abstraction layer boven broker admin-acties.
4. **Gebruik queue-gebaseerde fair-use** (queue alarms + metric integraties) als eerste stap.
5. **Voeg provider capability check toe** bij onboarding:
   - broker HTTP API beschikbaar?
   - STOMP toegestaan?
   - limieten op users/queues/connections passend?
   - regio = EU contractueel geborgd?

## Open punten voor besluitvorming

1. Welke CloudAMQP-plannen voldoen aan verwacht aantal users/queues/connections?
2. Is STOMP op alle beoogde plannen/tiers beschikbaar?
3. Welke metric-retentie en granulariteit is nodig voor fair-use handhaving?
4. Welke contracttekst is nodig voor EU-only data residency (incl. backups/observability)?

## Bronnen

### Perspectives codebase

- `packages/perspectives-core/src/core/computedValues/rabbitmq.purs`
- `packages/perspectives-core/src/core/amqpTransport/managementAPI.purs`

### CloudAMQP-publicaties (via GitHub, geraadpleegd op 2026-04-16)

- Terraform provider README:  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/README.md`
- Message Broker HTTP API guide (provider-to-provider, user/vhost beheer):  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/docs/guides/info_http_api.md`
- Instance resource docs (credentials, regions, dedicated/shared context):  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/docs/resources/instance.md`
- Region guide:  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/docs/guides/info_region.md`
- Alarm docs (queue/vhost-gerichte alarmen):  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/docs/resources/alarm.md`
- Metric integration docs (queue/vhost allowlist):  
  `https://github.com/cloudamqp/terraform-provider-cloudamqp/blob/main/docs/resources/integration_metric.md`
