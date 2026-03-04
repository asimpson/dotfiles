#!/usr/bin/env bash
set -euo pipefail

token_path="/var/lib/orbit/identifier"
fleet_url="https://grafanalabs.cloud.fleetdm.com"
device_token="$(tr -d '[:space:]' < "$token_path")"
if [[ -z "$device_token" ]]; then
  echo "error: token file is empty: $token_path" >&2
  exit 1
fi

summary_url="${fleet_url%/}/api/latest/fleet/device/${device_token}/desktop"
request=$(curl -s --show-error "${summary_url}")

failing_policies=$(echo "${request}" | jq -r '.failing_policies_count' )
notif=$(echo "${request}" | jq -r '.notifications')

if [[ "$failing_policies" =~ ^[0-9]+$ ]]; then
  if (( failing_policies == 0 )); then
    device_status="green"
  else
    device_status="red"
  fi
else
  device_status="unknown"
fi

if [[ "$failing_policies" =~ ^[0-9]+$ ]]; then
  failing_policies_count_json="$failing_policies"
else
  failing_policies_count_json="null"
fi

jq -cn \
  --arg status "$device_status" \
  --argjson failing_policies_count "$failing_policies_count_json" \
  --argjson notif "$notif" \
  '{
    failing_policies_count: $failing_policies_count,
    status: $status,
    notification: $notif
  }'
