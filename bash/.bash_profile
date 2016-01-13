if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
export PATH="$HOME/Library/Haskell/bin:$PATH"

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
export PATH="$HOME/.rbenv/bin:$PATH"

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin
