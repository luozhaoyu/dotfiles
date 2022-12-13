if status --is-interactive
    source $HOME/repos/dotfiles/.bash_aliases
end

if status --is-interactive
    source $HOME/repos/dotfiles/company.fish
end

set -x SPARK_HOME /opt/spark
set -x JAVA_HOME /usr/local/opt/openjdk@11/
set PATH $PATH $HOME/go/bin/ $HOME/local/bin $HOME/.cargo/bin $HOME/Library/Python/3.8/bin /opt/spark/bin/
fish_add_path /usr/local/opt/openjdk/bin
fish_add_path /usr/local/opt/openjdk@11/bin
