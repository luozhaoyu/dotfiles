clean:
	-rm ~/.vimrc
	-rm ~/.bashrc
	-rm -r ~/.vim
	-rm -r ~/.emacs.d

MYFOLDER=${HOME}/codes/dotfiles/

install:
	# setup
	echo ". ${MYFOLDER}/.bashrc" >> ${HOME}/.bashrc
	echo "source ${MYFOLDER}/.vimrc" >> ${HOME}/.vimrc
	mkdir ${HOME}/.vim
	ln -sf ${HOME}/codes/dotifles/UltiSnips ${HOME}/.vim/
	mkdir ${HOME}/.emacs.d
	ln -sf ${HOME}/codes/dotfiles/init.el ${HOME}/.emacs.d/

	# handle the emacs evil ESC eval problem
	echo "set -s escape-time 0" >> ~/.tmux.conf

install-go:
	# company
	go get -u github.com/nsf/gocode
	go get -u github.com/rogpeppe/godef
	go get -u github.com/golang/lint/golint
	go get -u github.com/lukehoban/go-outline
	go get -u sourcegraph.com/sqs/goreturns
	go get -u golang.org/x/tools/cmd/gorename
	go get -u github.com/tpng/gopkgs
	go get -u github.com/newhook/go-symbols
	go get -u golang.org/x/tools/cmd/guru
	go get -u golang.org/x/tools/cmd/goimports

	# me
	go get -u github.com/dougm/goflymake
	go get -u github.com/kisielk/errcheck
	# http://www.nongnu.org/emacsdoc-fr/manuel/shell.html
	go get -u github.com/google/codesearch/cmd/...
