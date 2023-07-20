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

OUTPUT=$((phonebook --all "${v_pattern}") 2>&1)

if [ $? -ne 0 ]
then
  STATUS=500
  MESSAGE="${OUTPUT}"
  CONTENT=""
else
  CONTENT=$(printf "${OUTPUT}" | grep -v '^#' | grep -v '^Login' | tr -s '\n' | jq -sR 'split("\n") | map(split(":")) | map(select(length > 0)) | map({(.[0]): (.[1:] | join(":") | sub("^[[:space:]]+"; "") | sub("[[:space:]]+$"; ""))}) | add' | jq '."Computer account(s)" = (to_entries[(to_entries | map(.key == "Computer account(s)") | index(true))+1:] | map([.key, .value] | join(":")))' | jq 'with_entries(select(.key | test("^[a-z]") | not))')
fi

print_header
printf "{\"content\": ${CONTENT}, \"path\": \"${v_path}\", \"message\": \"${MESSAGE}\"}"
