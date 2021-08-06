#!/usr/bin/env bash

if test ! $(which brew); then
    echo "Homebrew not found. Please install homebrew..."
    exit 127
fi

# Make sure we’re using the latest Homebrew.
brew update

# Upgrade any already-installed formulae.
brew upgrade --all

# Install GNU core utilities (those that come with OS X are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
# brew install coreutils
# sudo ln -s /usr/local/bin/gsha256sum /usr/local/bin/sha256sum

# Install some other useful utilities like `sponge`.
brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names

###############################################################################
# Bash 4                                                                      #
###############################################################################

# Install Bash 4.
brew install bash
# brew tap homebrew/versions
brew install bash-completion2
# We installed the new shell, now we have to activate it
echo "Adding the newly installed shell to the list of allowed shells"
# Prompts for password
sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
# Change to the new shell, prompts for password
chsh -s /usr/local/bin/bash

###############################################################################
# Stuff I want                                                                #
###############################################################################

# Install more recent versions of some OS X tools.
brew install vim --with-override-system-vi
brew install grep
# brew install homebrew/dupes/openssh

# Install font tools.
brew tap bramstein/webfonttools
brew install sfnt2woff
brew install sfnt2woff-zopfli
brew install woff2

# stow for managing dotfiles
brew install stow

# git
brew install git
brew install git-calc
brew install git-lfs
brew install git-flow
brew install git-extras
brew install hub

# ag!
brew install the_silver_searcher
# pt!
brew install the_platinum_searcher

# db
brew install sqlite

# langs
brew install ruby-build
brew install rbenv
# brew install elixir
# brew install erlang
brew install leiningen
brew install boot-clj
brew install python
brew install valgrind
brew install nvm
brew install yarn
brew install cairo # ocaml
brew install clojure
brew install idris

# misc
brew install global --with-pygments --with-ctags
brew install colordiff
brew install youtube-dl
brew install pdf-tools
brew install pandoc
brew install editorconfig
## other useful stuff
brew install imagemagick --with-webp
brew install tree
brew install wget --with-iri

# Install Heroku
brew install heroku/brew/heroku
brew install watchman
heroku update

# rm to trash
brew install rmtrash

###############################################################################
# Casks                                                                       #
###############################################################################
brew tap caskroom/cask

# Editor
# brew cask install --appdir="~/Applications" emacs
brew install emacs-plus@28 --with-native-comp 

## Core casks
brew install --cask iterm2

## Nice (musts) to haves
brew install --cask caffeine
brew install --cask flux
brew install --cask shiftit
brew install --cask karabiner-elements

## Dev tools
# it's the database ui from intellij
brew install --cask datagrip
# brew install --cask sequel-pro
# brew install --cask atom
# brew install --cask visual-studio-code
# brew install --cask android-studio


## VM stuff
# brew install --cask vagrant
# brew install --cask virtualbox
# brew install --cask vagrant
# brew install --cask parallels

## Misc
# brew install --cask google-chrome
brew install --cask firefox
brew install --cask firefox-developer-edition
brew install --cask skype
brew install --cask slack
brew install --cask dropbox
brew install --cask evernote
brew install --cask 1password
brew install --cask openoffice
brew install --cask transmit
brew install --cask vlc
brew install --cask sketch
brew install --cask adium
brew install --cask calibre
brew install --cask whatsapp
# brew install --cask telegram-desktop
# LaTeX dist MacTex
# brew install --cask mactex

brew install --cask gpg-suite

## Fonts
brew tap homebrew/cask-fonts
brew install --cask font-hack
brew install --cask  font-fira-code   font-fira-mono   font-fira-sans

## Remove outdated versions from the cellar.
brew cleanup
