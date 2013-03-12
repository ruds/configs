
# MacPorts Installer addition on 2010-08-06_at_19:33:38: adding an appropriate PATH variable for use with MacPorts.
export PATH=$HOME/android/current/tools:$HOME/android/current/platform-tools:$HOME/.cabal/bin:/opt/local/bin:/opt/local/sbin:$HOME/.cljr/bin:$PATH
export CLASSPATH=".:$CLASSPATH"
#export PYTHONPATH=/opt/local/lib/python/site-packages:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/
export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/
# Finished adapting your PATH environment variable for use with MacPorts.

if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
