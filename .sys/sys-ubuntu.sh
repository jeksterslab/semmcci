#!/bin/bash

# apt
sudo apt-get update -y && sudo apt-get install -y \
        make                                      \
        tmux                                      \
        git                                       \
        vim                                       \
        curl                                      \
        wget

# docker
# https://docs.docker.com/engine/install/ubuntu/
sudo apt-get update -y && sudo apt-get install -y ca-certificates curl gnupg lsb-release
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update -y
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin
sudo service docker start
sudo docker run hello-world

# apptainer
# https://apptainer.org/docs/admin/main/installation.html
sudo apt-get update -y
sudo apt-get install -y wget
cd /tmp
wget https://github.com/apptainer/apptainer/releases/download/v1.1.2/apptainer_1.1.2_amd64.deb
sudo apt-get install -y ./apptainer_1.1.2_amd64.deb
wget https://github.com/apptainer/apptainer/releases/download/v1.1.2/apptainer-suid_1.1.2_amd64.deb
sudo dpkg -i ./apptainer-suid_1.1.2_amd64.deb
