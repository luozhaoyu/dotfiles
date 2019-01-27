#!/bin/sh
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
