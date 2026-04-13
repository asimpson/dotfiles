#!/bin/bash

DIR=$1
NAME=$2

virsh attach-device llm-jail --file /dev/stdin <<EOF
  <filesystem type="mount" accessmode="passthrough">
    <driver type="virtiofs"/>
    <source dir="${DIR}"/>
    <target dir="${NAME}"/>
    <readonly/>
  </filesystem>
EOF

echo "Attached ${DIR} — mount in guest with: mount -t virtiofs -o ro ${NAME} /mnt/${NAME}"
