#!/bin/bash

VM_NAME="kolide"

# Start the VM
echo "Starting $VM_NAME..."
virsh start "$VM_NAME"

# Check if the start command was successful
if [ $? -eq 0 ]; then
    echo "$VM_NAME started successfully."

    # Open the graphical console using virt-viewer
    echo "Opening graphical console..."
    virt-viewer $VM_NAME
    sleep 2
    i3-msg '[class="Virt-viewer"] move to workspace vmwk'

else
    echo "Failed to start $VM_NAME. Please check for errors."
fi
