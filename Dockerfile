# Published as msparks/devbox on Docker Hub.
from ubuntu:14.04
maintainer Matt Sparks <ms@quadpoint.org>

run apt-get update
run apt-get install -y aptitude
run apt-get install -y build-essential
run apt-get install -y cmake
run apt-get install -y curl
run apt-get install -y diffstat
run apt-get install -y git
run apt-get install -y pkg-config
run apt-get install -y python
run apt-get install -y strace
run apt-get install -y sudo
run apt-get install -y tcpdump
run apt-get install -y telnet
run apt-get install -y tmux
run apt-get install -y traceroute
run apt-get install -y vim
run apt-get install -y wget
run apt-get install -y zsh

# Install go.
run curl https://storage.googleapis.com/golang/go1.3.linux-amd64.tar.gz \
  | tar -C /usr/local -zx
env GOROOT /usr/local/go
env PATH /usr/local/go/bin:$PATH

# Install latest docker.
run wget https://get.docker.io/builds/Linux/x86_64/docker-latest \
  -O /usr/local/bin/docker
run chmod +x /usr/local/bin/docker

# Unrestricted sudo.
run echo "ALL ALL = (ALL) NOPASSWD: ALL" >> /etc/sudoers

# Timezone.
run cp /usr/share/zoneinfo/America/Los_Angeles /etc/localtime

# UTF-8.
run dpkg-reconfigure locales && \
    locale-gen en_US.UTF-8 && \
    /usr/sbin/update-locale LANG=en_US.UTF-8

# Create user.
run useradd ms
run mkdir /home/ms && chown -R ms: /home/ms

# Shared data volume.
run mkdir /var/shared/
run touch /var/shared/.placeholder
run chown -R ms:ms /var/shared
volume /var/shared

# Set up environment.
env HOME /home/ms
env PATH /home/ms/bin:$PATH
env PKG_CONFIG_PATH /home/ms/lib/pkgconfig
env LD_LIBRARY_PATH /home/ms/lib
env GOPATH /home/ms/go:$GOPATH
env LC_ALL en_US.UTF-8

# Install dotfiles.
add . /tmp/dotfiles
run chown -R ms:ms /tmp/dotfiles
run ls -l /tmp
run (cd /tmp/dotfiles && sudo -u ms ./install.sh)
run rm -rf /tmp/dotfiles

# User home directory.
user ms
run mkdir -p /home/ms/go /home/ms/bin /home/ms/lib /home/ms/include

workdir /home/ms
cmd ["/bin/zsh"]
