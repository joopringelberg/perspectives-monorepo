# Een betaalservice voor Perspectives

## 1) Korte verkenning van Nederlandse betaalservices

Voor Perspectives zijn vooral deze functies relevant:

1. iDEAL/SEPA betalingen starten met vooraf ingevulde betaalgegevens;
2. een **merchant reference/metadata** meesturen;
3. een betrouwbare bevestiging van de betaalstatus ophalen;
4. liefst een ondertekend betaalbewijs dat via de koper kan worden teruggegeven.

### Kandidaten (NL/EU, gangbaar in Nederland)

| Service | Relevante mogelijkheden | Match met Perspectives-vraag |
|---|---|---|
| **Mollie** | Hosted checkout, iDEAL, metadata/reference, payment status API, webhooks | Goed bruikbaar voor flow met `paymentId` + status-verificatie; geen standaard “ondertekend bewijs naar koper” als primaire flow |
| **Adyen** | Web Components, redirect/components flow, metadata/reference, webhook + HMAC, status API | Sterk voor embedded UX; bevestiging is normaal server-gericht, niet direct koper→verkoper |
| **Buckaroo** | NL PSP, iDEAL, transaction reference, status callbacks/API | Praktisch vergelijkbaar: callback/webhook dominant, koper-proof meestal niet standaard ondertekend token |
| **MultiSafepay** | Hosted/embedded opties, order metadata, status updates/webhooks | Zelfde patroon: reference + statusverificatie werkt, “signed receipt to payer” beperkt |
| **CM.com Payments** | iDEAL ondersteuning, redirect/API flow, order reference | Bruikbaar voor standaard PSP-patroon; expliciete cryptografische koper-receipt niet kernpatroon |

**Conclusie verkenning:** voor de Perspectives-architectuur is het meest realistische patroon momenteel:

- koper ontvangt na betaling een `paymentId`/`orderId` (+ eigen reference);
- koper schrijft dat in een Perspectives-property;
- verkoper valideert de status bij de PSP via API.

Dat geeft functioneel hetzelfde resultaat als webhook-terugkoppeling, zonder publiek bereikbaar verkoperendpoint.
Als de PSP API tijdelijk niet bereikbaar is, blijft `PaymentStatus` op `Pending` staan en moet de
verificatie herhaalbaar zijn (retry/backoff), zonder dienstvrijgave.

### Keuze eerste provider: Adyen

Voor de eerste implementatie kiezen we **Adyen**.

### Is Adyen-integratie mogelijk zonder webhook?

Ja, voor een eerste versie wel:

1. de koper rondt de betaling af in de Adyen component (drop-in/card);
2. de koper-installatie slaat `PspPaymentId` en return-payload op in het betaalverzoek;
3. de verkoper valideert de status via Adyen API (`payments/details` of session-resultaatcontrole).

Daarmee kan de kernflow werken zonder publiek webhook-endpoint aan verkoperszijde.
Webhook blijft optioneel als versnelling voor snellere statusupdates, maar is geen harde voorwaarde.

### Verschil tussen `PaymentReference` en `PspPaymentId`

- `PaymentReference`: eigen business-ID van het Perspectives betaalverzoek (door Aanbieder gemaakt).
  Deze gebruik je voor idempotentie, matching en domeinlogica.
- `PspPaymentId`: transactiereferentie die door de PSP (Adyen) wordt uitgegeven.
  Deze gebruik je om de feitelijke PSP-status op te vragen/te valideren.

## 2) Functioneel ontwerp voor inbedding in Perspectives

### Doel

Volledig client-side flow (in `perspectives-react` en/of `mycontexts`) waarbij:

1. Aanbieder zet bedrag + ontvangergegevens + betalingskenmerk;
2. Afnemer betaalt via embedded PSP-component of redirect;
3. Afnemer-installatie slaat betalingsbewijs op als role-property;
4. PDR synchroniseert naar Aanbieder;
5. Aanbieder valideert en activeert automatisch de dienst.

### Voorstel datamodel (in context met koper + verkoper)

Modelleer dit als een expliciete rol, bijvoorbeeld `Payment`, met onderstaande properties
en ranges in ARC-termen:

- `Amount` (`Number`)
- `Currency` (`String`, in ARC bij voorkeur met expliciete default zoals `= "EUR"`)
- `ReceiverAccount` (`String`, IBAN of PSP merchant account id)
- `PaymentReference` (`String`, UUID/CUID door verkoper gegenereerd)
- `PspProvider` (`String`, bv. mollie/adyen)
- `PspPaymentId` (`String`)
- `PspReturnPayload` (`String`, JSON als string opgeslagen)
- `PaymentStatus` (`EnumeratedPropertyValue`, waarden:
  `Requested | Pending | Paid | Failed | Expired | Cancelled`)
- `PaidAt` (`DateTime`)
- `VerificationDetails` (`String`, JSON of samenvatting validatie)

### UX/flow

1. **Aanbieder** kiest “Breng in rekening” → zet `Amount`, `ReceiverAccount`, `PaymentReference`.
2. **Afnemer** ziet “Betaal”-sectie met PSP-knop/component.
3. Component start betaling met `amount + receiver + reference`.
4. Na terugkomst schrijft client minimaal `PspPaymentId` en eventueel `PspReturnPayload`.
5. Synchronisatie levert dit bij Aanbieder.
6. **Aanbieder** triggert “Verifieer betaling”:
   - via PSP API op `PspPaymentId` (en optioneel check op `PaymentReference`, amount, currency);
   - bij succes: `PaymentStatus = Paid`, `PaidAt = now`, service vrijgeven.

### Integratiepunten in code

- **`perspectives-react`**: nieuw herbruikbaar `PaymentWidget` component
  - als `PerspectivesComponent` met dezelfde reactieve subscription-stijl als bestaande componenten;
  - communiceert via `PDRproxy` voor lezen/schrijven van payment-properties;
  - props: `provider`, `amount`, `currency`, `reference`, `receiver`,
    callbacks voor `onPending`, `onReturn`, `onError`, `onTimeout`.
  - `onTimeout` moet de status op `Pending` laten en een expliciete herprobeer-actie aanbieden;
    afhandeling volgt dezelfde foutmeldingslijn als andere `PerspectivesComponent`-componenten
    (via bestaande user messaging patronen), met retry op componentniveau.
- **`mycontexts`**: provider-keuze en config (per deployment), plus schermcompositie.
  - provider-config bij voorkeur via bestaande globale configuratie (`perspectivesGlobals`)
    of een vergelijkbaar centraal configuratiepunt, zodat secrets en endpoints niet hardcoded raken.
    Deze globale config blijft caller-provided, net als de bestaande `host`-configuratie.
- **Geen afhankelijkheid van webhook** voor domeinlogica; webhook blijft optioneel voor versnelling.

### Veiligheids- en integriteitsregels

1. Verleen dienst **alleen** na verificatie door verkoper op PSP-status.
2. Vertrouw niet blind op ruwe `PspReturnPayload` uit de koper-client.
3. Vergelijk altijd: `amount`, `currency`, `reference`, `merchant account`.
4. Registreer idempotent op `PspPaymentId` (herhaalde sync of retries).
   - Als dezelfde `PspPaymentId` terugkomt met afwijkende `amount/reference`: markeer als
     inconsistent en blokkeer automatische vrijgave.
   - Als dezelfde `PspPaymentId` exact gelijk terugkomt: beschouw als duplicate delivery en negeer.

### Advies voor eerste implementatiefase

1. Start met één provider-adapter (bijv. Mollie of Adyen) en het generieke datamodel hierboven.
2. Bouw provider-agnostische interface (`createPayment`, `resumePayment`, `verifyPayment`).
3. Houd schermniveau in `mycontexts`; maak providerlogica herbruikbaar in `perspectives-react`.
4. Voeg daarna extra providers toe zonder wijziging van domeinmodel.
