# Technische beschrijving: GUI-styling in MyContexts

Dit document beschrijft de systematiek van de grafische stijl in de packages
`mycontexts` en `perspectives-react`. Het doel is inzicht te bieden in de
laagopbouw, zodat toekomstige stijlaanpassingen zo veel mogelijk via Bootstrap-
variabelen en Bootstrap-conventies kunnen worden doorgevoerd in plaats van via
ad-hoc CSS-aanpassingen.

---

## 1. Opbouw van de stijllagen

De stijl is opgebouwd in vier opeenvolgende lagen, elk met een specifieke
verantwoordelijkheid:

```
Laag 1  Bootstrap base stylesheet (Bootswatch-thema, CDN of npm)
Laag 2  Bootstrap CSS-variabelen overschreven in accessibility.css (:root)
Laag 3  Bootstrap React-componenten via react-bootstrap (className props)
Laag 4  Package-specifieke custom stylesheets (www.css, components.css, …)
```

### Laag 1 – Bootstrap base stylesheet

`mycontexts` importeert Bootstrap via het Bootswatch-thema **Spacelab**:

```typescript
// packages/mycontexts/src/App.tsx
import 'bootswatch/dist/spacelab/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
```

Dit levert Bootstrap 5 plus het Spacelab kleurenpalet (koele grijsblauw-tonen).
Bootstrap Icons worden als afzonderlijk lettertype-pakket ingeladen.

Er bestaat ook een `ThemeManager` (`src/themeManagement.ts`) die op runtime
alternatieve Bootswatch-thema's of externe custom-thema's dynamisch kan laden
via een `<link>`-element in `document.head`. Op dit moment wordt de
`ThemeManager` nog niet actief gebruikt in de productieflow; de statische import
in `App.tsx` is leidend.

### Laag 2 – CSS-variabelen in `accessibility.css`

In `packages/mycontexts/src/styles/accessibility.css` worden Bootstrap
5-CSS-variabelen overschreven in de `:root`-scope. Dit is de aanbevolen manier
om Bootstrap-kleuren, typografie en interactie-stijlen te veranderen zonder de
Bootstrap-broncode te wijzigen.

Overschreven variabelen (selectie):

| Bootstrap-variabele | Nieuwe waarde | Doel |
|---|---|---|
| `--bs-body-color` | `#464e57` | Hogere contrastverhouding voor standaard tekst |
| `--bs-secondary-color` | `#555555` | Betere leesbaarheid van secundaire tekst |
| `--bs-form-text-color` | `#6b6b6b` | Leesbaarheid van helptext in formulieren |
| `--bs-primary-rgb` / `--bs-primary` | `68, 110, 155` / `#446e9b` | Afwijkend primair blauw t.o.v. Spacelab-standaard |
| `--bs-secondary-bg` / `--bs-secondary-rgb` | `#626b73` | Donkerder secundaire achtergrond |
| `--bs-success` / `--bs-success-rgb` | `#007b00` | Donkerder groen voor betere zichtbaarheid |
| `--bs-link-color` / `--bs-link-hover-color` | `#043466` / `#003d7e` | Links met verhoogd contrast |
| `--bs-input-placeholder-color` | `#767676` | Placeholderkleur voldoet aan WCAG AA |
| `--bs-accordion-btn-color` | `#212529` | Donkere tekst in accordeon-knoppen |
| `--bs-code-color` | `#bd2e76` | Donkerder roze voor inline code |

Door uitsluitend Bootstrap-variabelen te overschrijven, zijn alle onderdelen van
de UI die dezelfde variabele gebruiken (knoppen, links, tabellen, …) automatisch
mee-aangepast.

### Laag 3 – Bootstrap React-componenten (`react-bootstrap`)

Zowel `mycontexts` als `perspectives-react` gebruiken de bibliotheek
`react-bootstrap` (v2.x, Bootstrap 5). React-Bootstrap vertaalt
Bootstrap-componentklassen naar React-componenten. De stijl wordt aangebracht
via de `className`-prop op deze componenten.

### Laag 4 – Custom stylesheets

Beide packages bevatten eigen CSS-bestanden voor zaken die buiten de Bootstrap-
defaults vallen.

---

## 2. Gebruik van Bootstrap-klassen via `className`

### 2.1 React-Bootstrap componenten

De volgende Bootstrap-componenten worden geïmporteerd en gebruikt:

**`packages/mycontexts`** (hoofdzakelijk `www.tsx`, `App.tsx`):

| Component | Gebruik |
|---|---|
| `Container` / `Row` / `Col` | Rasterindeling van de drie-kolomsweergave (Who / What / Where) |
| `Navbar` | Bovenste en onderste navigatiebalk |
| `NavDropdown` | Hamburgermenu (hoofdmenu) in de bovenste balk |
| `Tab` / `Tabs` | Mobiele tabbladen voor Who / What / Where-secties |
| `Accordion` / `Accordion.Item` | Notificaties, klembord, chat-kanalen |
| `Offcanvas` | Uitschuifpanelen (links: Me, Apps, Settings; beneden: notificaties) |
| `Modal` | Inspector-dialoog, transactie-importdialoog |
| `Button` | Diverse actieknoppen |
| `Form` / `Form.Group` / `Form.Control` | Bevestigingscode bij transactie-import |

**`packages/perspectives-react`** (componenten voor de PDR-schermen):

| Component | Gebruik |
|---|---|
| `Table` | Tabelweergave van roleinstanties in `perspectivetable.tsx` |
| `Accordion` / `AccordionContext` | Accordeon-rijen voor PerspectiveTable |
| `Form` | Formuliervelden in meerdere formuliercomponenten |
| `Card` | Foutmelding in `screen.tsx` |
| `Navbar` | Knoppenbalk boven tabellen (`tablecontrols.tsx`, `formcontrols.tsx`) |
| `Col` / `Row` / `Container` / `Button` | Layout en knoppen doorheen meerdere componenten |

### 2.2 Utility-klassen in `className`-props

Bootstrap-utility-klassen worden direct als string doorgegeven in de
`className`-prop. Hieronder een overzicht van de meest gebruikte klassen:

**Achtergrondkleuren (met opacity-variabele):**

```tsx
// www.tsx – drie kolommen op desktop
<Col className='bg-primary animated-column' style={{'--bs-bg-opacity': '.1'} as React.CSSProperties}>
<Col className='bg-primary animated-column' style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
<Col className='bg-primary animated-column' style={{'--bs-bg-opacity': '.3'} as React.CSSProperties}>
```

De opacity van de achtergrondkleur wordt via de Bootstrap CSS-variabele
`--bs-bg-opacity` als inline style ingesteld. Zo kunnen de kolommen elk een
lichtere tint van dezelfde primaire kleur tonen zonder extra CSS-regels.

**Tekstkleuren en typografie:**

```tsx
<h2 className='text-center text-dark column-heading'>
<i className="bi bi-list text-light fs-2">
<h3 className="text-center pt-5">
```

**Afstand (spacing):**

```tsx
<Container fluid className='px-0'>
<Row className='mx-0 px-0'>
<Navbar className="py-0 ps-2">
<Row className='pb-3'>
<div className="text-center p-4 border border-secondary rounded">
```

**Borders en ronden:**

```tsx
<div className="text-center p-4 border border-secondary rounded">
```

**Visueel verbergen (screenreader-tekst):**

```tsx
<span className="visually-hidden">{i18next.t("mainMenu")}</span>
```

**Subtiele achtergrond:**

```tsx
<p className='bg-light-subtle'>Ga ergens heen</p>
```

**Scrollgedrag (eigen klasse, zie §3):**

```tsx
<Tab className='bg-primary full-mobile-height px-2 scrollable-content'>
```

---

## 3. Custom stylesheets in `mycontexts`

### 3.1 `src/styles/accessibility.css`

**Doel:** Overschrijft Bootstrap CSS-variabelen voor betere toegankelijkheid
(WCAG-contrast).

Inhoud:
- `:root`-blok met gewijzigde `--bs-*`-variabelen (zie §1, laag 2).
- `.h2.column-heading` / `.h3.column-heading`: lichte afwijking van standaard
  Bootstrap h4-/h5-stijlen voor de kolom-headers.
- `.content-section-area:focus`: zichtbaar focus-outline voor toetsenbordnavigatie.
- `.keyboard-nav-instructions`: visueel verborgen instructies voor screenreaders.
- `.dropdown-toggle:focus`, `.navbar *:focus`: vergroot focus-outline (3 px
  witte rand + gekleurde box-shadow) op navbar-elementen.
- `*:focus-visible`: globale focus-stijl met `--bs-link-color` voor consistentie.

### 3.2 `src/styles/www.css`

**Doel:** Lay-out en animaties voor het hoofdscherm (WWWComponent).

Inhoud:

| Klasse | Beschrijving |
|---|---|
| `.full-height` | Hoogte = dvh minus boven- en onderste navbar (gebruikt `--bottom-navbar-height`, `--top-navbar-height` CSS-variabelen). |
| `.full-mobile-height` | Hoogte voor mobiel via `--mobile-content-height`. |
| `.full-www-content-height` | Hoogte minus beide navbars én de kolom-header (`--who-header-height`). |
| `.chat-height` | Halve beschikbare hoogte voor chatvenster. |
| `.slide-in-from-right` / `.slide-out-to-right` | CSS-animaties voor het inschuiven/uitschuiven van vensters. |
| `.hide-caret` | Verbergt het Bootstrap-dropdown-pijltje (used on `NavDropdown`). |
| `.scrollable-content` | `overflow-y: auto; overflow-x: hidden` voor scrolbare kolommen. |
| `.animated-column` | Vloeiende breedte-overgang bij klikken op kolom (`flex-basis`, `width`, `padding`). |
| `.accordion` | Overschrijft `--bs-accordion-body-padding-x` naar `0.5rem`. |
| `.container` | Overschrijft `--bs-gutter-x` naar `0.5rem`. |
| `.markdown` | Leesbaarheid van Markdown-blokken; gebruikt de accordion-padding-variabelen als fallback. |
| `.content-top-aligned` | Flex-kolom met `justify-content: flex-start` voor bovenlijning van content. |
| `.navbar-title` | Afgeknotte titeltekst met `text-overflow: ellipsis` in de bovenste Navbar. |
| `.skip-link` | Toegankelijkheids-skip-link, visueel verborgen tenzij gefocust. |
| `.cursor-pointer` | Handcursor bij hover voor klikbare elementen. |

Overschrijvingen van Bootstrap-defaults via variabelen of specifiekere selectors:

```css
/* Dropdown-link kleur via Bootstrap variabelen */
html body .dropdown-menu .dropdown-item {
  --bs-dropdown-link-color: var(--bs-primary);
  --bs-dropdown-link-hover-color: var(--bs-info-text);
}

/* Focus-ring kleur via Bootstrap variabele */
:focus {
  outline-color: var(--bs-warning);
}

/* Accordion-caret verbergen */
.accordion-button:after {
  display: none;
}
```

### 3.3 `src/styles/slidingPanels.css`

**Doel:** Stijl voor het `SlidingPanels`-patroon (een hoofd-paneel met een
bedek-paneel dat van rechts inschuift).

| Klasse | Beschrijving |
|---|---|
| `.sliding-panels-container` | `overflow-x: hidden`; flex-container op 100% breedte. |
| `.main-panel` | Vult beschikbare ruimte; vaste achtergrondkleur `#f8f9fa` (Bootstrap `$gray-100`). |
| `.cover-panel` | Absoluut gepositioneerd, begint op `right: -100%`, schuift naar `right: 0` via CSS-transitie. Z-index 1030 (boven de Navbar). |
| `.cover-panel.open` | Activeert de inschuif-positie. |
| `.has-open-panel .main-panel` | `pointer-events: none` zodat het achterliggende paneel niet klikbaar is. |
| `.sliding-panel-content` | Flex-grow 1 met ondermarge. |

De achtergrondkleur `#f8f9fa` is de Bootstrap 5 `$gray-100`-waarde, maar staat
hier als hardcoded hex. Bij een themawijziging verdient het aanbeveling dit te
vervangen door `var(--bs-light)` of `var(--bs-body-bg)`.

### 3.4 `src/styles/splash.css`

**Doel:** Basisstijl voor het laadscherm (`loadingScreen.tsx`).

Bevat één klasse `.introductionSplash` met `font-size: large`. De commentaar-
regels bevatten uitgeschakelde kleurinstellingen; deze zijn nog niet actief.

---

## 4. Custom stylesheets in `perspectives-react`

### 4.1 `src/styles/components.css`

**Doel:** Stijlen voor generieke PDR-schermcomponenten (tabellen,
accordeons, sleepgebieden).

Inhoud:

| Klasse / selector | Beschrijving |
|---|---|
| `::placeholder` | Hogere contrastkleur voor placeholder-tekst via `--bs-input-placeholder-color`. |
| `.failure:focus` | Rode (Bootstrap `$red` = `#dc3545`) focus-ring voor validatiefouten. |
| `.dropHere` | Cyaan (Bootstrap `$cyan` = `#0dcaf0`) ring voor actieve dropzones. |
| `.card:focus, .dropzone:focus` | Blauwe outline voor focusbare kaarten. |
| `.navbarCard` | Verminderde verticale padding voor kaartelementen in een Navbar. |
| `.widget` | `flex-grow: 1` voor widgets die beschikbare ruimte moeten opvullen. |
| `.disabledIcon` | Verminderde opacity + `pointer-events: none` voor uitgeschakelde iconen. |
| `.accordion-button::after` | Caretknop naar rechterrand (`margin-left: auto`). |
| `.accordion-button > div` | Ruimte tussen content en caret. |
| `.accordion-button .btn-link:hover/focus` | Verwijdert Bootstrap hover-stijl in accordeon-knoppen. |
| `.accordion-button:not(.collapsed)` | Overschrijft `--bs-accordion-active-bg` met `--bs-dark-bg-subtle`. |
| `.swiping` | Vloeiende transform + opacity bij swipe-gebaren. |
| `.swipe-confirm-element` / `.swipe-confirm-buttons` | Flex-layout voor swipe-bevestigingsopties. |
| `.accordion-actions` | Inline flex-rij voor accordeon-actieknoppen. |
| `.hide-caret.dropdown-toggle::after` | Verbergt Bootstrap dropdown-pijl (met `!important`). |
| `.card-selected` | Visuele markering van geselecteerde kaart via `--bs-primary` box-shadow + achtergrond. |
| `.public-role` | Lichtblauwe achtergrond voor publieke-rol-indicators. |
| `.calculated-role` | Lichtgele achtergrond voor berekende-rol-indicators. |
| `.cancelled-peer` | Lichtroze achtergrond voor verbroken peer-verbindingen. |

Opmerkingen:
- De hardcoded hexwaarden `#dc3545` en `#0dcaf0` corresponderen met Bootstrap
  5-variabelen `$red` en `$cyan`. Het commentaar in de code vermeldt dit
  expliciet. Om stijlaanpassingen via Bootstrap-variabelen mogelijk te maken
  zouden deze vervangen kunnen worden door `var(--bs-danger)` resp. `var(--bs-info)`.
- De klasse `.card-selected` gebruikt al `var(--bs-primary)` met een fallback —
  een goed voorbeeld van de gewenste aanpak.
- `.public-role`, `.calculated-role` en `.cancelled-peer` gebruiken hardcoded
  kleuren. Ook hier zijn Bootstrap-variabelen passender.

### 4.2 `src/styles/highlight.css`

**Doel:** Kleurthema voor syntaxmarkering van ARC-code (via Highlight.js).

Dit bestand bevat het **Solar Flare** thema (Base16-variant) van Highlight.js.
Het bevat geen Bootstrap-variabelen; het is volledig zelfstandig en definieert
zijn eigen achtergrond- en tekstkleur voor code-blokken. Het thema kan worden
vervangen door een ander Highlight.js-thema via het keuze-script
`choose_highlight-theme.sh` in de `perspectives-react`-root.

---

## 5. CSS-variabelen als stylingvectorpunt

De huidige implementatie volgt een gecombineerde aanpak:

1. **Bootstrap CSS-variabelen** (`--bs-*`) worden overschreven in
   `accessibility.css` (`:root`) en op component-niveau via `style={}`-props.
   Dit is de **aanbevolen** manier om Bootstrap-elementen thema-breed te
   wijzigen.

2. **Eigen CSS-variabelen** worden gedefinieerd voor layout-specifieke afmetingen
   die Bootstrap niet kent:
   - `--bottom-navbar-height`
   - `--top-navbar-height`
   - `--mobile-content-height`
   - `--who-header-height`
   - `--markdown-padding-y` / `--markdown-padding-x`

   Deze variabelen zijn gedeclareerd als impliciet gebruik (de klassen verwijzen
   ernaar, maar de initiële waarden worden elders ingesteld via JavaScript of
   zijn CSS-standaardwaarden).

3. **Hardcoded hexwaarden** komen voor in `slidingPanels.css` (`#f8f9fa`),
   `components.css` (`#dc3545`, `#0dcaf0`, `#e6f7ff`, `#fff3cd`, `#ffe0e0`) en
   `accessibility.css` (`#0056b3`). Dit zijn gebieden waar in de toekomst
   Bootstrap-variabelen kunnen worden toegepast.

---

## 6. Aanbevelingen voor stijlexperimenten

Op basis van bovenstaande analyse zijn dit de aanbevolen aanknopingspunten voor
stijlaanpassingen:

### 6.1 Kleurveranderingen (thema-breed)

Pas de `:root`-variabelen in `accessibility.css` aan:

```css
:root {
  --bs-primary:     #nieuwe-kleur;
  --bs-primary-rgb: r, g, b;
  --bs-secondary-bg: #nieuwe-kleur;
  /* enzovoort */
}
```

Alle Bootstrap-componenten die `--bs-primary` gebruiken (knoppen, navbars,
`bg-primary`, links) passen automatisch mee.

### 6.2 Thema wisselen (Bootswatch)

Vervang de import in `App.tsx`:

```typescript
// Van:
import 'bootswatch/dist/spacelab/bootstrap.min.css';
// Naar:
import 'bootswatch/dist/flatly/bootstrap.min.css';
```

Of activeer de `ThemeManager` om runtime-themawisseling mogelijk te maken.

### 6.3 Font

Bootstrap 5 gebruikt standaard de native system-font-stack. Om een ander font
te gebruiken, overschrijf:

```css
:root {
  --bs-body-font-family: 'Naam van het font', sans-serif;
  --bs-font-sans-serif: 'Naam van het font', sans-serif;
}
```

### 6.4 Kaders en borders

Bootstrap 5 gebruikt `--bs-border-color` en `--bs-border-radius`:

```css
:root {
  --bs-border-color: #gewenste-kleur;
  --bs-border-radius: 0; /* vierkante hoeken */
}
```

### 6.5 Hardcoded waarden vervangen door variabelen

Kandidaten in `components.css`:

```css
/* Huidig: */
.failure:focus { box-shadow: 0 0 0 5px #dc3545; }
.dropHere      { box-shadow: 0 0 0 5px #0dcaf0; }

/* Aanbevolen: */
.failure:focus { box-shadow: 0 0 0 5px var(--bs-danger); }
.dropHere      { box-shadow: 0 0 0 5px var(--bs-info);   }
```

In `slidingPanels.css`:

```css
/* Huidig: */
background: #f8f9fa;

/* Aanbevolen: */
background: var(--bs-body-bg);
```

---

## 7. Bestandsoverzicht

```
packages/mycontexts/
└── src/
    ├── App.tsx                          # Bootstrap-thema-import (Bootswatch Spacelab)
    ├── themeManagement.ts               # ThemeManager voor runtime-themawisseling
    └── styles/
        ├── accessibility.css            # Bootstrap CSS-variabelen overschreven (:root)
        ├── www.css                      # Layout- en animatiestijlen voor WWWComponent
        ├── slidingPanels.css            # Inschuifpanelen
        └── splash.css                   # Laadscherm

packages/perspectives-react/
└── src/
    └── styles/
        ├── components.css               # Generieke PDR-UI-stijlen (tabellen, dropzones)
        └── highlight.css                # Highlight.js thema voor ARC-syntaxmarkering
```
