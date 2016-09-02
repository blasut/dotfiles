if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
export PATH="$HOME/Library/Haskell/bin:$PATH"

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
export PATH="$HOME/.rbenv/bin:$PATH"

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{bash_prompt}; do
  [ -r "$file" ] && source "$file"
done
unset file


# From spacemacs
#+BEGIN_SRC sh
export PATH=~/.local/bin:$PATH
#+END_SRC

export GOPATH=$HOME/code/golang
export PATH=$PATH:/usr/local/go/bin

export PATH=$PATH:~/.cache/rebar3/bin

export NVM_DIR="$HOME/.nvm"
. "$(brew --prefix nvm)/nvm.sh"

export HOMEBREW_NO_ANALYTICS=1

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
    GIT_PS1_SHOWDIRTYSTATE=true
    export PS1='[\u@mbp \w$(__git_ps1)]\$ '
fi
