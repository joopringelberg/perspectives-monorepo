#!/usr/bin/env bash
set -euo pipefail

# storedocs.sh — store (upload) ndjson docs into a CouchDB database
#
# Usage:
#   ./storedocs.sh --user USER --pass PASS --url http://host:5984/dbname \
#                  [--in docs.ndjson] [--sleep 0.05] [--cacert /path/to/ca.crt | --insecure]
#
# Notes:
# - Authentication is passed via curl's -u option (no credentials embedded in the URL required).
# - The provided --url must be the full database URL (including the db name).

# Defaults (overridable by flags)
USER_NAME=""
USER_PASS=""
DBDST=""
IN_FILE='docs.ndjson'
SLEEP_SECS='0.05'
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
  --in FILE          Input ndjson file (default: $IN_FILE)
  --sleep SECS       Sleep between docs (default: $SLEEP_SECS)
  --cacert FILE      CA bundle or root CA cert to trust for TLS
  --insecure         Disable TLS verification (NOT recommended)
  -h, --help         Show this help
USAGE
}

# Parse flags
while [[ $# > 0 ]]; do
  case "$1" in
    --user) USER_NAME=${2:-}; shift 2 ;;
    --pass) USER_PASS=${2:-}; shift 2 ;;
    --url)  DBDST=${2:-}; shift 2 ;;
    --in)   IN_FILE=${2:-}; shift 2 ;;
    --sleep) SLEEP_SECS=${2:-}; shift 2 ;;
    --cacert) CACERT=${2:-}; shift 2 ;;
    --insecure) INSECURE=true; shift 1 ;;
    -h|--help) print_usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; print_usage; exit 2 ;;
  esac
done

if [[ -z "$USER_NAME" || -z "$USER_PASS" || -z "$DBDST" ]]; then
  echo "Missing required arguments." >&2
  print_usage
  exit 2
fi

if [[ ! -f "$IN_FILE" ]]; then
  echo "Input file not found: $IN_FILE" >&2
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

# zorg dat de DB bestaat
curl -sS "${CURLOPT_AUTH[@]}" "${CURL_TLS_OPTS[@]}" -X PUT "$DBDST" || true

i=0
while IFS= read -r line; do
  # haal id en encode voor URL
  id=$(printf '%s' "$line" | jq -r '._id')
  [ -z "$id" ] && { echo "doc zonder _id overgeslagen"; continue; }
  eid=$(printf '%s' "$id" | jq -s -R @uri | tr -d '"')

  # strip rev/conflict velden én álle attachments
  printf '%s' "$line" \
    | jq 'del(._rev, ._revisions, ._conflicts, ._deleted_conflicts, ._attachments)' \
    > /tmp/doc.json

  # upload vanuit bestand (voorkomt "Argument list too long")
  curl -sS "${CURLOPT_AUTH[@]}" "${CURL_TLS_OPTS[@]}" \
    -X PUT "$DBDST/$eid" \
    -H 'Content-Type: application/json' \
    -H 'Expect:' --http1.1 \
    --data-binary @/tmp/doc.json \
  | jq -c '.'

  i=$((i+1))
  # kleine pauze helpt bij krappe CPU/RAM
  sleep "$SLEEP_SECS"
done < "$IN_FILE"

echo "Klaar: $i documenten geüpload (zonder attachments)."
