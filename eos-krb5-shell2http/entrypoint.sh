#!/bin/sh
set -e

if [ -z "${CERN_USER}" ] || [ -z "${CERN_PASS}" ]; then
  printf "Must set both CERN_USER and CERN_PASS environment variables.\n"
  exit 1
fi

declare -p | grep -E '^CERN_' > /app/container.env || true

set -- /app/shell2http -host="0.0.0.0" "$@"

echo "$@"
exec "$@"
