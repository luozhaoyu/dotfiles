#!/bin/sh
# vim /etc/bash.bashrc
# http://mywiki.wooledge.org/DotFiles
# You should therefore always have source ~/.bashrc at the end of your .bash_profile in order to force it to be read by a login shell.
# https://stackoverflow.com/questions/902946/about-bash-profile-bashrc-and-where-should-alias-be-written-in/903213#903213

# env settings should be in .profile
export GOPATH=$HOME/gocode
export PATH=$PATH:$GOPATH/bin

alias difff='diff -bBy --suppress-common'
alias duh1='du -h --max-depth=1'
alias findall='find / -name'
alias findh='find ./* -name'
alias findpy='find . -name '\''*.py'\'''
alias findpyg='find . -name '\''*.py'\'' | xargs grep -n --color=auto'
alias findgog='find . -name '\''*.go'\'' | xargs grep -n --color=auto'
alias yuml='yum --disableexcludes=Local'
alias xclipc='xclip -selection c'
alias openconnectuw='sudo openconnect dept-ra-cssc.vpn.wisc.edu'
rsyncfolder () {
	rsync -avz $1 $2:~/$1
}

ssh_tunnel () {
	ssh -fNT -L $2:localhost:$2 $1
}

case "$OSTYPE" in
  darwin*)
    alias ls='ls -FG'
    alias sstlnp="netstat -anp tcp |grep LISTEN"
    ;;
  linux*)
    alias ls='ls --color'
    ;;
  *) ;;
esac
alias ll='ls -l'

alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

# 重要操作前提示
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias em="emacs -nw"
alias cindexreset="cindex -reset ."
