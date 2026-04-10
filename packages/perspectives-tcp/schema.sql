
> perspectives-tcp@1.0.0 schema /Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-tcp
> node dist/index.js --schema

[2026-03-24T08:43:41.076Z] INFO  Loaded configuration from "/Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-tcp/config.json"
create table "bijeenkomsten" ("id" text not null, "context_id" text null, "filler_id" text null, "Geregistreerden" text null, "datum" timestamptz null, constraint "bijeenkomsten_pkey" primary key ("id"));
alter table "bijeenkomsten" add constraint "bijeenkomsten_geregistreerden_foreign" foreign key ("Geregistreerden") references "geregistreerden" ("id") on delete SET NULL;

create table "geregistreerden" ("id" text not null, "filler_id" text null, "context_id" text null, "voornaam" text null, "achternaam" text null, constraint "geregistreerden_pkey" primary key ("id"));
alter table "geregistreerden" add constraint "geregistreerden_context_id_foreign" foreign key ("context_id") references "bijeenkomsten" ("id") on delete SET NULL;
