#!/usr/bin/env bash
set -euo pipefail

## TODO: Make this the "real" setup script

brew install --cask firefox
brew tap homebrew/cask-versions
brew install --cask firefox-developer-edition
brew install --cask iterm2
brew install starship
brew install --cask Slack
brew install vim

brew install git

## To install emacs with native json compilation
xcode-select --install
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-native-comp
brew install stow
brew install ripgrep
brew install fd
brew install coreutils

# upgrade bash
brew install bash

# fonts
brew install --cask font-hack
brew install --cask  font-fira-code   font-fira-mono   font-fira-sans

# databases
# brew install mysql

# aws stuff
brew install --cask aws-vault
brew install awscli
brew install chamber

# misc
brew install --cask spotify
brew install --cask karabiner-elements
cd ~/dotfiles && stow karabiner && cd
brew install --cask gpg-suite
brew install --cask shiftit

## To install required LSPs

## elixir ls (https://github.com/elixir-lsp/elixir-ls)
# install requried erlang and elixir version then follow the compliation instructions in the readme

## javascript https://github.com/sourcegraph/javascript-typescript-langserver.git
brew install nvm
brew install yarn
npm i -g javascript-typescript-langserver

# ruby
brew install rbenv

# Trying out funky for local shell scripts
brew install python
pip3 install pyfunky
