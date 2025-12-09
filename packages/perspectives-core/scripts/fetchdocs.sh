#!/usr/bin/env bash
set -euo pipefail

# fetchdocs.sh — fetch all docs from a CouchDB database to an ndjson file
#
# Usage:
#   ./fetchdocs.sh --user USER --pass PASS --url http://host:5984/dbname \
#                  [--batch 1000] [--ids ids.txt] [--out docs.ndjson] \
#                  [--cacert /path/to/ca.crt | --insecure]
#
# Notes:
# - Authentication is passed via curl's -u option (no credentials embedded in the URL).
# - The provided --url must be the full database URL (including the db name).

# Defaults (can be overridden by flags)
USER_NAME=""
USER_PASS=""
DBSRC=""
BATCH=1000
IDS='ids.txt'
OUT='docs.ndjson'
CACERT=""
INSECURE=false

print_usage() {
  cat <<USAGE
Usage: $0 --user USER --pass PASS --url http(s)://host:port/database [options]

Required:
  --user USER        CouchDB username
  --pass PASS        CouchDB password
  --url  DB_URL      Full CouchDB database URL, e.g. http://localhost:5984/mydb

Optional:
  --batch N          Page size for _all_docs (default: $BATCH)
  --ids FILE         Output file for IDs (default: $IDS)
  --out FILE         Output ndjson file (default: $OUT)
  --cacert FILE      CA bundle or root CA cert to trust for TLS
  --insecure         Disable TLS verification (NOT recommended)
  -h, --help         Show this help
USAGE
}

# Parse flags
while [[ $# -gt 0 ]]; do
  case "$1" in
    --user)
      USER_NAME=${2:-}; shift 2 ;;
    --pass)
      USER_PASS=${2:-}; shift 2 ;;
    --url)
      DBSRC=${2:-}; shift 2 ;;
    --batch)
      BATCH=${2:-}; shift 2 ;;
    --ids)
      IDS=${2:-}; shift 2 ;;
    --out)
      OUT=${2:-}; shift 2 ;;
    --cacert)
      CACERT=${2:-}; shift 2 ;;
    --insecure)
      INSECURE=true; shift 1 ;;
    -h|--help)
      print_usage; exit 0 ;;
    *)
      echo "Unknown argument: $1" >&2
      print_usage; exit 2 ;;
  esac
done

if [[ -z "$USER_NAME" || -z "$USER_PASS" || -z "$DBSRC" ]]; then
  echo "Missing required arguments." >&2
  print_usage
  exit 2
fi

# Curl auth array (avoids word splitting issues)
CURLOPT_AUTH=(-u "$USER_NAME:$USER_PASS")
# TLS options
CURL_TLS_OPTS=()
if [[ -n "$CACERT" ]]; then
  CURL_TLS_OPTS+=(--cacert "$CACERT")
fi
if [[ "$INSECURE" == true ]]; then
  CURL_TLS_OPTS+=(-k)
fi

# leeg outputbestanden
: > "$IDS"
: > "$OUT"

# 1) IDs in pagina's ophalen (lichtgewicht)
startkey=''
startkey_docid=''
skip=''

while :; do
  url="$DBSRC/_all_docs?include_docs=false&limit=$BATCH$skip"
  if [ -n "${startkey:-}" ]; then
    esk=$(printf '%s' "$startkey" | jq -s -R @uri | tr -d '"')
    esd=$(printf '%s' "$startkey_docid" | jq -s -R @uri | tr -d '"')
    url="$url&startkey=%22$esk%22&startkey_docid=$esd"
  fi

  resp=$(curl "${CURLOPT_AUTH[@]}" "${CURL_TLS_OPTS[@]}" --fail-with-body -sS "$url") || { echo "Fout bij ophalen: $url"; exit 1; }
  count=$(printf '%s' "$resp" | jq '.rows|length')
  [ "$count" -eq 0 ] && break

  # alleen IDs opslaan (erg klein)
  printf '%s' "$resp" | jq -r '.rows[].id' >> "$IDS"

  last_id=$(printf '%s' "$resp" | jq -r '.rows[-1].id')
  startkey="$last_id"
  startkey_docid="$last_id"
  skip='&skip=1'
done

echo "IDs opgehaald: $(wc -l < "$IDS")"

# 2) Per ID het document ophalen (standaard zonder attachment bytes)
#    -> dit blijft klein; _attachments bevatten alleen stubs tenzij je ?attachments=true meegeeft
while IFS= read -r id; do
  # URL-encode id
  eid=$(printf '%s' "$id" | jq -s -R @uri | tr -d '"')
  # fetch het document compact op één regel
  curl "${CURLOPT_AUTH[@]}" "${CURL_TLS_OPTS[@]}" -sS "$DBSRC/$eid" | jq -c '.' >> "$OUT"
done < "$IDS"

echo "Klaar: $(wc -l < "$OUT") documenten in $OUT"
