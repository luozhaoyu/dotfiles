if status --is-interactive
    source $HOME/codes/dotfiles/.bash_aliases
end

if status --is-interactive
    source $HOME/codes/dotfiles/company.fish
end

set PATH $PATH $HOME/go/bin/ $HOME/local/bin $HOME/.cargo/bin $HOME/Library/Python/3.8/bin
