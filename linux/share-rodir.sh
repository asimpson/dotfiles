#!/bin/bash

DIR=$1
BASE=$(basename "$DIR")

virsh -c qemu:///system attach-device llm-jail --file /dev/stdin <<EOF
  <filesystem type="mount" accessmode="passthrough">
    <driver type="virtiofs"/>
    <source dir="${DIR}"/>
    <target dir="${BASE}"/>
    <readonly/>
  </filesystem>
EOF

ssh llmjail "sudo -n mkdir -p /mnt/${BASE}"
ssh llmjail "sudo -n mount -t virtiofs -o ro ${BASE} /mnt/${BASE}"
ssh llmjail "git clone /mnt/${BASE} /tmp/${BASE}"

echo "Attached ${DIR}"
