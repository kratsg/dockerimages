#!/bin/sh
set -e

set -- /app/shell2http -host="0.0.0.0" "$@"

echo "$@"
exec "$@"
