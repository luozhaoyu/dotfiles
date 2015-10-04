# vim /etc/bash.bashrc ~/.bash_aliases

alias difff='diff -bBy --suppress-common'
alias duh1='du -h --max-depth=1'
alias findall='find / -name'
alias findh='find ./* -name'
alias findpy='find . -name '\''*.py'\'''
alias findpyg='find . -name '\''*.py'\'' | xargs grep -n --color=auto'
alias yuml='yum --disableexcludes=Local'
alias xclipc='xclip -selection c'
alias openconnectuw='sudo openconnect dept-ra-cssc.vpn.wisc.edu'

# shell的彩色显示
export LS_OPTIONS='--color=auto'
# eval "`dircolors`"
alias la='ls -A'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -lh'
alias l='ls $LS_OPTIONS -lA'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

# 重要操作前提示
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias emacs='emacs -nw'
