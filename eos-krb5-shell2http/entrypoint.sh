#!/bin/sh
set -e

if [ -z "${CERN_USER}" ] || [ -z "${CERN_PASS}" ]; then
  printf "Must set both CERN_USER and CERN_PASS environment variables.\n"
  exit 1
fi

declare -p | grep -E '(CERN|EOS)' > /app/container.env || true

# initialize kerberos
printf "${CERN_PASS}" | kinit ${CERN_USER}@CERN.CH >> /var/log/cron.log 2>&1

crontab /etc/cron.d/kinit-cron
crond

set -- /app/shell2http -host="0.0.0.0" "$@"

echo "$@"
exec "$@"
