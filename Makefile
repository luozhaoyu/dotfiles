default:
	@echo "try make install-go"

clean:
	-rm ~/.vimrc
	-rm ~/.bashrc
	-rm -r ~/.vim
	-rm -r ~/.emacs.d

MYFOLDER=${HOME}/repos/dotfiles/

${HOME}/.config/fish/config.fish:

fish: ${HOME}/.config/fish/config.fish
	brew install fish
	mkdir -p ${HOME}/.config/fish
	ln -sf ${PWD}/config.fish ${HOME}/.config/fish/config.fish
	ln -sf /opt/homebrew/bin/fish /usr/local/bin
	echo /usr/local/bin/fish | tee -a /etc/shells
	chsh -s /usr/local/bin/fish

install: install-links fish
	# setup
	echo ". ${MYFOLDER}/.bashrc" >> ${HOME}/.bashrc
	echo "source ${MYFOLDER}/.vimrc" >> ${HOME}/.vimrc

	# handle the emacs evil ESC eval problem
	echo "set -s escape-time 0" >> ~/.tmux.conf

install-links:
	-mkdir ${HOME}/.vim
	ln -sf ${PWD}/UltiSnips ${HOME}/.vim/
	-mkdir ${HOME}/.emacs.d
	ln -sf ${PWD}/init.el ${HOME}/.emacs.d/
	ln -sf ${PWD}/snippets ${HOME}/.emacs.d/
	ln -sf ${PWD}/customize ${HOME}/.emacs.d/

install-go:
	# company
	go get -u github.com/nsf/gocode
	go get -u github.com/rogpeppe/godef
	go get -u golang.org/x/lint/golint
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
