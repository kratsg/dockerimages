#!/bin/bash
STATUS=200
MESSAGE="Ok."

function print_header {
  printf "Content-Type: application/json\n"
  printf "Status: ${STATUS}\n\n"
}

if [ ! ${v_pattern+x} ]; then
  STATUS=400
  MESSAGE="You did not specify a pattern."
  print_header
  printf "{\"message\": \"${MESSAGE}\"}"
  exit 0
fi

OUTPUT=$((/app/venv/bin/pyphonebook \
  --json "surname" \
  --json "firstname" \
  --json "login" \
  --json "department" \
  --json "group" \
  --json "cerngroup" \
  --json "cernsection" \
  --json "displayname" \
  --json "phone" \
  --json "otherphone" \
  --json "mobile" \
  --json "fax" \
  --json "office" \
  --json "pobox" \
  --json "email" \
  --json "organization" \
  --json "ccid" \
  --json "homedir" \
  --json "last" \
  --json "uid" \
  --json "gid" \
  --json "uac" \
  --json "type" \
  --json "emailnick" \
  --json "company" \
  --json "shell" \
  --json "secid" \
  "${v_pattern}") 2>&1)

if [ $? -ne 0 ]
then
  STATUS=500
fi

print_header
printf "{\"pattern\": \"${v_pattern}\", \"message\": \"${MESSAGE}\", \"output\": \"${OUTPUT}\"}"
