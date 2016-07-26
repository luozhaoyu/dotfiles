#!/bin/sh
# setup
ln -sf ~/codes/myvim/.bash_aliases ~
ln -sf ~/codes/myvim/.vimrc ~`
ln -sf ~/codes/myvim/UltiSnips ~/.vim/
ln -sf ~/codes/myvim/init.el ~/.emacs.d/

# company
go get -u github.com/rogpeppe/godef         # jump to function definition
go get -u golang.org/x/tools/cmd/goimports  # auto-import on format
go get -u github.com/nsf/gocode             # code completion engine
go get -u golang.org/x/tools/cmd/oracle     # it's the oracle
go get -u golang.org/x/tools/cmd/cover      # code coverage
go get -u golang.org/x/tools/cmd/vet        # detect if you're doing dumb things
go get -u github.com/golang/lint/golint     # check for style violations

# me
go get -u github.com/dougm/goflymake
go get -u github.com/kisielk/errcheck
# http://www.nongnu.org/emacsdoc-fr/manuel/shell.html
go get -u github.com/google/codesearch/cmd/...
go get -u golang.org/x/tools/cmd/gorename # rename

# handle the emacs evil ESC eval problem
echo "set -s escape-time 0" >> ~/.tmux.conf
