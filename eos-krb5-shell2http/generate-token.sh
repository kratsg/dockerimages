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

TOKEN=$(eos token --path "${v_path}")
printf "{\"token\": \"${TOKEN}\", \"path\": \"${v_path}\", \"message\": \"${MESSAGE}\"}"
