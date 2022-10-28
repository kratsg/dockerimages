#!/bin/bash
[ -f /app/container.env ] && source /app/container.env

STATUS=200
MESSAGE="Ok."

function print_header {
  printf "Content-Type: application/json\n"
  printf "Status: ${STATUS}\n\n"
}

if [ ! ${v_path+x} ]; then
  STATUS=400
  MESSAGE="You did not set a path."
  print_header
  printf "{\"message\": \"${MESSAGE}\"}"
  exit 0
fi

if ! klist 2> /dev/null | grep -q "principal: ${CERN_USER}@CERN.CH"
then
  STATUS=503
  MESSAGE="kerberos is not ready for ${CERN_USER}."
  print_header
  printf "{\"message\": \"${MESSAGE}\"}"
  exit 0
fi

OUTPUT=$((eos ls "${v_path}") 2>&1)

if [ $? -ne 0 ]
then
  STATUS=500
  MESSAGE="${OUTPUT}"
  OUTPUT=""
fi
CONTENT="$(printf "${OUTPUT}" | jq --raw-input . | jq -c --slurp .)"

print_header
printf "{\"content\": ${CONTENT}, \"path\": \"${v_path}\", \"message\": \"${MESSAGE}\"}"
