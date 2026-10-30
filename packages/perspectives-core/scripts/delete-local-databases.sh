#!/usr/bin/env bash
set -euo pipefail

USER_NAME=""
PASSWORD="${COUCHDB_PASSWORD:-}"
BASE_URL="http://127.0.0.1:5984"
EXECUTE=false

usage() {
  cat <<EOF
Usage: $0 --user USER [--url URL] [--execute]

Options:
  --user USER     CouchDB administrator username
  --url URL       Local CouchDB URL (default: $BASE_URL)
  --execute       Delete matching databases; otherwise only preview
  -h, --help      Show this help

Supply the password through COUCHDB_PASSWORD or enter it when prompted.

Examples:
  $0 --user admin
  $0 --user admin --execute
  COUCHDB_PASSWORD=secret $0 --user admin --execute
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --user) USER_NAME=${2:-}; shift 2 ;;
    --url) BASE_URL=${2:-}; shift 2 ;;
    --execute) EXECUTE=true; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 2 ;;
  esac
done

if [[ -z "$USER_NAME" ]]; then
  echo "Missing required --user argument." >&2
  usage
  exit 2
fi

BASE_URL="${BASE_URL%/}"

# This is intentionally restricted to CouchDB on this Mac.
if [[ ! "$BASE_URL" =~ ^https?://(localhost|127\.0\.0\.1|\[::1\])(:[0-9]+)?$ ]]; then
  echo "Refusing non-local CouchDB URL: $BASE_URL" >&2
  exit 2
fi

command -v curl >/dev/null || {
  echo "curl is required." >&2
  exit 2
}

command -v jq >/dev/null || {
  echo "jq is required; install it with: brew install jq" >&2
  exit 2
}

echo "Connecting to local CouchDB at $BASE_URL..."

if [[ -z "$PASSWORD" ]]; then
  printf 'CouchDB password: ' >&2
  IFS= read -r -s PASSWORD
  printf '\n' >&2
fi

AUTH=(-u "$USER_NAME:$PASSWORD")
DATABASES_FILE=$(mktemp)
trap 'rm -f "$DATABASES_FILE"' EXIT

DATABASES_JSON=$(curl --fail-with-body --silent --show-error \
  "${AUTH[@]}" "$BASE_URL/_all_dbs")

if ! jq -e 'type == "array"' >/dev/null <<< "$DATABASES_JSON"; then
  echo "Unexpected response from $BASE_URL/_all_dbs; expected a JSON array." >&2
  exit 1
fi

DATABASE_COUNT_TOTAL=$(jq 'length' <<< "$DATABASES_JSON")

jq -r '
    .[]
    | select(
        . != "_global_changes"
        and . != "_replicator"
        and . != "_users"
      )
    | select(test("(_post|_entities|_models|_invertedqueries|-recovery)$"))
  ' <<< "$DATABASES_JSON" > "$DATABASES_FILE"

DATABASE_COUNT=$(wc -l < "$DATABASES_FILE" | tr -d ' ')

echo "CouchDB returned $DATABASE_COUNT_TOTAL databases; $DATABASE_COUNT match the deletion suffixes."

if [[ "$DATABASE_COUNT" -eq 0 ]]; then
  echo "No matching databases found."
  exit 0
fi

echo "Matching databases ($DATABASE_COUNT):"
sed 's/^/  /' "$DATABASES_FILE"

if [[ "$EXECUTE" != true ]]; then
  echo
  echo "Dry run only. Add --execute to delete these databases."
  exit 0
fi

echo
read -r -p "Type DELETE $DATABASE_COUNT DATABASES to continue: " CONFIRMATION

if [[ "$CONFIRMATION" != "DELETE $DATABASE_COUNT DATABASES" ]]; then
  echo "Confirmation did not match; nothing deleted."
  exit 1
fi

DELETED=0

while IFS= read -r DATABASE_NAME; do
  ENCODED_NAME=$(
    printf '%s' "$DATABASE_NAME" |
      jq -sRr @uri
  )

  echo "Deleting $DATABASE_NAME"
  curl --fail-with-body --silent --show-error \
    "${AUTH[@]}" \
    -X DELETE \
    "$BASE_URL/$ENCODED_NAME" >/dev/null

  DELETED=$((DELETED + 1))
done < "$DATABASES_FILE"

echo "Deleted $DELETED databases."