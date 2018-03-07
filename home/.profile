
# MacPorts Installer addition on 2015-06-15_at_22:29:12: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

export GOPATH=$HOME/go
export GRADLE_HOME=/opt/local/share/java/gradle

. /Users/ruds/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
