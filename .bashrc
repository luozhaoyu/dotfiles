#!/bin/sh
# vim /etc/bash.bashrc
# http://mywiki.wooledge.org/DotFiles
# You should therefore always have source ~/.bashrc at the end of your .bash_profile in order to force it to be read by a login shell.
# https://stackoverflow.com/questions/902946/about-bash-profile-bashrc-and-where-should-alias-be-written-in/903213#903213

# env settings should be in .profile
export GOPATH=$HOME/gocode
export PATH=$PATH:$GOPATH/bin

rsyncfolder () {
	rsync -avz $1 $2:~/$1
}

ssh_tunnel () {
	ssh -fNT -L $2:localhost:$2 $1
}

OSTYPE="$(uname -s)"
case "${OSTYPE}" in
  Darwin*)
    alias ls='ls -FG'
    alias sstlnp="netstat -anp tcp |grep LISTEN"
    ;;
  Linux*)
    alias ls='ls --color'
    ;;
  *) ;;
esac

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
