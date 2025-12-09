#!/usr/bin/env bash
### Gebruik dit script om attachments tussen twee CouchDB databases te kopiëren.
### Eerst upload je de documenten zónder attachments; daarna kopieer je met dit script de attachments.
set -euo pipefail

#
# Gebruik:
#   ./copyAttachments.sh \
#       --src-url http(s)://host:port/source_db \
#       --dst-url http(s)://host:port/dest_db \
#       [--src-user USER --src-pass PASS] \
#       [--dst-user USER --dst-pass PASS] \
#       [--ids ids.txt] [--sleep 0.03] \
#       [--cacert /path/to/ca.crt | --insecure]
#
# Opmerking: --insecure is bedoeld voor snelle dev-uses; voorkom dit in productie.

# Defaults (te overriden via flags)
DBSRC=''
DBDST=''
SRC_USER=''
SRC_PASS=''
DST_USER=''
DST_PASS=''
IDS_FILE='ids.txt'
SLEEP_SECS='0.03'
CACERT=''
INSECURE=false

print_usage() {
  cat <<USAGE
Gebruik: $0 --src-url URL --dst-url URL [opties]

Verplicht:
  --src-url URL      Bron database URL (inclusief db-naam)
  --dst-url URL      Doel database URL (inclusief db-naam)

Optioneel:
  --src-user USER    Gebruiker voor bron
  --src-pass PASS    Wachtwoord voor bron
  --dst-user USER    Gebruiker voor doel
  --dst-pass PASS    Wachtwoord voor doel
  --ids FILE         Bestand met document-ids (default: $IDS_FILE)
  --sleep SECS       Pauze tussen uploads (default: $SLEEP_SECS)
  --cacert FILE      CA-certificaat/keten om TLS te vertrouwen
  --insecure         TLS-verificatie uitschakelen (alleen dev)
  -h, --help         Toon deze hulp
USAGE
}

# Parseer flags
while [[ $# -gt 0 ]]; do
  case "$1" in
    --src-url) DBSRC=${2:-}; shift 2 ;;
    --dst-url) DBDST=${2:-}; shift 2 ;;
    --src-user) SRC_USER=${2:-}; shift 2 ;;
    --src-pass) SRC_PASS=${2:-}; shift 2 ;;
    --dst-user) DST_USER=${2:-}; shift 2 ;;
    --dst-pass) DST_PASS=${2:-}; shift 2 ;;
    --ids) IDS_FILE=${2:-}; shift 2 ;;
    --sleep) SLEEP_SECS=${2:-}; shift 2 ;;
    --cacert) CACERT=${2:-}; shift 2 ;;
    --insecure) INSECURE=true; shift 1 ;;
    -h|--help) print_usage; exit 0 ;;
    *) echo "Onbekend argument: $1" >&2; print_usage; exit 2 ;;
  esac
done

if [[ -z "$DBSRC" || -z "$DBDST" ]]; then
  echo "Ontbrekende vereiste argumenten --src-url en/of --dst-url." >&2
  print_usage
  exit 2
fi

# Auth en TLS opties als strings (word splitting gewenst hier)
AUTH_SRC=""
AUTH_DST=""
[[ -n "$SRC_USER" ]] && AUTH_SRC="-u $SRC_USER:$SRC_PASS"
[[ -n "$DST_USER" ]] && AUTH_DST="-u $DST_USER:$DST_PASS"

TLS_OPTS=""
if [[ -n "$CACERT" ]]; then
  TLS_OPTS="--cacert $CACERT"
fi
if [[ "$INSECURE" == true ]]; then
  TLS_OPTS="$TLS_OPTS -k"
fi

# helper: URL-encode
uriencode() { jq -s -R @uri | tr -d '"'; }

# curl helpers (retourneer statuscode en body via tempfiles)
curl_json() {
  # args: auth_flags url out_json status_out
  local auth="$1" url="$2" out_json="$3" status_out="$4"
  local http
  http=$(mktemp)
  # -f zorgt dat non-2xx exit veroorzaakt; we willen status inspecteren, dus geen -f hier
  if ! curl -sS -L $auth $TLS_OPTS -w '%{http_code}' -H 'Accept: application/json' "$url" -o "$out_json" >"$http"; then
    echo "000" >"$http"
  fi
  cat "$http" >"$status_out"
  rm -f "$http"
}

curl_put_attachment() {
  # args: auth_flags url content_type file_path out_json status_out
  local auth="$1" url="$2" ctype="$3" file="$4" out_json="$5" status_out="$6"
  local http
  http=$(mktemp)
  if ! curl -sS -L $auth $TLS_OPTS -w '%{http_code}' -X PUT \
      -H "Content-Type: $ctype" \
      --data-binary @"$file" \
      "$url" -o "$out_json" >"$http"; then
    echo "000" >"$http"
  fi
  cat "$http" >"$status_out"
  rm -f "$http"
}

# check dat doel-docs bestaan (zou zo moeten zijn als je ze al zonder attachments hebt geüpload)
# anders krijg je een lege _rev en kunnen we de attachment niet PUTten
while IFS= read -r id; do
  [ -z "$id" ] && continue

  eid=$(printf '%s' "$id" | uriencode)

  # haal bron-doc op
  src_json=$(mktemp) ; src_status=$(mktemp)
  curl_json "$AUTH_SRC" "$DBSRC/$eid" "$src_json" "$src_status"
  code=$(cat "$src_status")
  if [ "$code" != "200" ]; then
    if [ "$code" = "404" ]; then
      echo "⚠️  Bron-doc niet gevonden: $id — overslaan"
    else
      echo "⚠️  Fout bij ophalen bron-doc ($code): $id — overslaan"
    fi
    rm -f "$src_json" "$src_status"
    continue
  fi

  # verzamel aantal attachments en controleer op leeg
  total=$(jq -r '._attachments? // {} | keys | length' <"$src_json")
  rm -f "$src_status"

  # geen attachments? door
  if [ "$total" -eq 0 ]; then
    rm -f "$src_json"
    continue
  fi

  # huidige rev van de doel-doc (moet bestaan als je docs eerder hebt geüpload)
  dst_json=$(mktemp) ; dst_status=$(mktemp)
  curl_json "$AUTH_DST" "$DBDST/$eid" "$dst_json" "$dst_status"
  code=$(cat "$dst_status")
  if [ "$code" != "200" ]; then
    echo "⚠️  Doeldoc ontbreekt of fout ($code) voor $id; upload eerst de doc zonder attachments. Overslaan."
    rm -f "$src_json" "$dst_json" "$dst_status"
    continue
  fi
  rev=$(jq -r '._rev // empty' <"$dst_json")
  if [ -z "$rev" ]; then
    echo "⚠️  Geen _rev gevonden voor doeldoc $id; overslaan."
    rm -f "$src_json" "$dst_json" "$dst_status"
    continue
  fi
  rm -f "$dst_json" "$dst_status"

  success=0
  # loop over alle attachments (zonder mapfile/readarray, voor compatibiliteit)
  while IFS= read -r att; do
    [ -z "$att" ] && continue
    eatt=$(printf '%s' "$att" | uriencode)

    # download bytes van bron
    attfile=$(mktemp)
    if ! curl -sS -L $AUTH_SRC $TLS_OPTS "$DBSRC/$eid/$eatt" -o "$attfile"; then
      echo "⚠️  Download mislukt voor attachment '$att' van $id — overslaan"
      rm -f "$attfile"
      continue
    fi

    # mimetype bepalen (best-effort)
    mime=$(file -b --mime-type "$attfile" 2>/dev/null || echo application/octet-stream)

    # upload attachment naar doel (PUT met ?rev=)
    put_json=$(mktemp) ; put_status=$(mktemp)
    curl_put_attachment "$AUTH_DST" "$DBDST/$eid/$eatt?rev=$rev" "$mime" "$attfile" "$put_json" "$put_status"
    code=$(cat "$put_status")
    newrev=$(jq -r '._rev // empty' <"$put_json" 2>/dev/null || true)

    if [ "$code" = "201" ] && [ -n "$newrev" ]; then
      rev="$newrev"
      success=$((success+1))
    elif [ "$code" = "409" ]; then
      # conflict: haal nieuwste rev op en probeer 1x opnieuw
      tmp_json=$(mktemp) ; tmp_status=$(mktemp)
      curl_json "$AUTH_DST" "$DBDST/$eid" "$tmp_json" "$tmp_status"
      if [ "$(cat "$tmp_status")" = "200" ]; then
        rev_retry=$(jq -r '._rev // empty' <"$tmp_json")
        if [ -n "$rev_retry" ]; then
          curl_put_attachment "$AUTH_DST" "$DBDST/$eid/$eatt?rev=$rev_retry" "$mime" "$attfile" "$put_json" "$put_status"
          code=$(cat "$put_status")
          newrev=$(jq -r '._rev // empty' <"$put_json" 2>/dev/null || true)
          if [ "$code" = "201" ] && [ -n "$newrev" ]; then
            rev="$newrev"; success=$((success+1))
          else
            echo "❌  PUT mislukt voor $id/$att (na retry), status $code"
          fi
        else
          echo "❌  Geen _rev bij retry voor $id/$att"
        fi
      else
        echo "❌  Kon nieuwste rev niet ophalen bij conflict ($id/$att), status $(cat "$tmp_status")"
      fi
      rm -f "$tmp_json" "$tmp_status"
    else
      echo "❌  PUT mislukt voor $id/$att, status $code"
    fi

    rm -f "$put_json" "$put_status" "$attfile"
    # kleine pauze kan helpen op krappe machines
    sleep "$SLEEP_SECS"
  done < <(jq -r '._attachments? // {} | keys[]' <"$src_json")

  # Controle: aantal attachments op doel verifiëren
  ver_json=$(mktemp) ; ver_status=$(mktemp)
  curl_json "$AUTH_DST" "$DBDST/$eid" "$ver_json" "$ver_status"
  if [ "$(cat "$ver_status")" = "200" ]; then
    dst_count=$(jq -r '._attachments? // {} | keys | length' <"$ver_json")
    echo "✅ $id: $success/$total attachment(s) overgezet — doel telt $dst_count attachments"
  else
    echo "⚠️  Kon doeldoc niet verifiëren voor $id (status $(cat "$ver_status")) — $success/$total overgezet"
  fi
  rm -f "$ver_json" "$ver_status" "$src_json"
done < "$IDS_FILE"

