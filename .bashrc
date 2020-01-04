#!/bin/sh
# vim /etc/bash.bashrc
# http://mywiki.wooledge.org/DotFiles
# You should therefore always have source ~/.bashrc at the end of your .bash_profile in order to force it to be read by a login shell.
# https://stackoverflow.com/questions/902946/about-bash-profile-bashrc-and-where-should-alias-be-written-in/903213#903213

# env settings should be in .profile
export PATH=$PATH:$GOPATH/bin:$PATH/local/bin
export PATH="$HOME/.cargo/bin:$PATH"
export GOPATH=$HOME/go

rsyncfolder () {
	rsync -avz $1 $2:~/$1
}

ssh_tunnel () {
	ssh -fNT -L $2:localhost:$2 $1
}

git_lint_without_overrite_history () {
# https://stackoverflow.com/questions/3945382/git-commit-that-doesnt-override-original-authors-in-git-blame
# https://medium.com/millennial-falcon-technology/reformatting-your-code-base-using-prettier-or-eslint-without-destroying-git-history-35052f3d853e
    git filter-branch --subdirectory-filter lib/orm/base/ --tree-filter 'yapf' -- --all
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
