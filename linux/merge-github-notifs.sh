#! /usr/bin/env /Users/adam/.nix-profile/bin/nix-shell
#! nix-shell -i bash -p gh jq

declare -A typeLookup
typeLookup["PullRequest"]="pull"
typeLookup["Issue"]="issues"

resp=$(gh api /notifications --paginate | jq -r '.[] | select(.subject.type == "PullRequest" or .subject.type == "Issue") | "\(.repository.full_name),\(.subject.url),\(.id),\(.subject.type)"')

for x in $resp; do
  name=$(echo "${x}" | cut -d ',' -f 1)
  id=$(echo "${x}" | cut -d ',' -f 3)
  number=$(basename $(echo "${x}" | cut -d ',' -f 2))
  ofType=$(echo "${x}" | cut -d ',' -f 4)
  urlType=$(echo ${typeLookup["${ofType}"]})
  isBot="false"

  if [ "${ofType}" == "PullRequest" ]; then
    isMerged=$(gh pr view --repo "${name}" "${number}" --json closed | jq -r .closed)
    isBot=$(gh pr view --repo "${name}" "${number}" --json author | jq -r .author.is_bot)
  fi

  if [ "${ofType}" == "Issue" ]; then
    isMerged=$(gh issue view --repo "${name}" "${number}" --json closed | jq -r .closed)
  fi

  if [ "${isMerged}" == "true" ] || [ "${isBot}" == "true" ]; then
    echo "marking https://github.com/${name}/${urlType}/${number} as done..."
    gh api -X PATCH "/notifications/threads/${id}"
  fi
done
