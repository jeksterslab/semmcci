#!/bin/bash
for filename in .sys/sys-arch/sys-arch-aur-*.sh; do
    bash $filename
done
