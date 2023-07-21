#!/bin/bash

start=$(date -d "2 weeks ago" +%Y-%m-%d)
end=$(date +%Y-%m-%d)
details=$(gh api /orgs/grafana/teams/$1 | jq -r '"\(.id),\(.organization.id)"')
teamId=$(echo ${details} | cut -d , -f 1)
orgId=$(echo ${details} | cut -d , -f 2)

gh api "organizations/${orgId}/team/${teamId}/members" | \
  jq -r '.[] | .login' | \
  xargs -I % gh search prs --author=% --created="${start}..${end}" \
  --json="state,repository,url,title,updatedAt,author" --template '{{range .}}{{tablerow (.author.login | autocolor "green") (hyperlink .url .title) (.repository.name | autocolor "blue") (.state | autocolor "red") (timeago .updatedAt)}}{{end}}
{{tablerender}}'
