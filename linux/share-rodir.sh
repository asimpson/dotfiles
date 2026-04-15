#!/bin/bash

DIR=$1
BASE=$(basename "$DIR")

virsh attach-device llm-jail --file /dev/stdin <<EOF
  <filesystem type="mount" accessmode="passthrough">
    <driver type="virtiofs"/>
    <source dir="${DIR}"/>
    <target dir="${BASE}"/>
    <readonly/>
  </filesystem>
EOF

echo "Attached ${DIR} — mount in guest with:"
echo "sudo mkdir -p /mnt/${BASE}
sudo mount -t virtiofs -o ro ${BASE} /mnt/${BASE}"
