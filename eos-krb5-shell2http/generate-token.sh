#!/bin/bash
STATUS=200
MESSAGE="Ok."

function print_header {
  printf "Content-Type: application/json\n"
  printf "Status: ${STATUS}\n\n"
}

if [ ! ${v_path+x} ]; then
  STATUS=503
  MESSAGE="You did not set a path."
  print_header
  printf "{\"message\": \"${MESSAGE}\"}"
  exit 0
fi

printf "{\"path\": \"${v_path}\", \"message\": \"${MESSAGE}\"}"
