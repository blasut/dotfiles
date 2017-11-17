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
brew install coreutils
sudo ln -s /usr/local/bin/gsha256sum /usr/local/bin/sha256sum

# Install some other useful utilities like `sponge`.
brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names

###############################################################################
# Bash 4                                                                      #
###############################################################################

#  # Install Bash 4.
#  brew install bash
#  brew tap homebrew/versions
#  brew install bash-completion2
#  # We installed the new shell, now we have to activate it
#  echo "Adding the newly installed shell to the list of allowed shells"
#  # Prompts for password
#  sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
#  # Change to the new shell, prompts for password
#  chsh -s /usr/local/bin/bash

###############################################################################
# Stuff I want                                                                #
###############################################################################

# Install `wget` with IRI support.
brew install wget --with-iri

# Install ruby-build and rbenv
brew install ruby-build
brew install rbenv

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

# db
brew install sqlite

# langs
brew install elixir
brew install erlang
brew install leiningen
brew install boot-clj
brew install python
brew install valgrind
brew install nvm
brew install yarn
brew install cairo # ocaml

# misc
brew install global --with-pygments --with-ctags
brew install colordiff
brew install youtube-dl
brew install pdf-tools
brew install pandoc

# other useful stuff
brew install imagemagick --with-webp
brew install tree

# Install Heroku
brew install heroku-toolbelt
brew install watchman
heroku update

###############################################################################
# Casks                                                                       #
###############################################################################
brew install caskroom/cask/brew-cask
brew tap caskroom/versions

## Core casks
brew cask install --appdir="~/Applications" iterm2

## Nice (musts) to haves
brew cask install --appdir="/Applications" caffeine
brew cask install --appdir="/Applications" flux
brew cask install --appdir="/Applications" shiftit
brew cask install --appdir="/Applications" karabiner-elements

## Dev tools
# it's the database ui from intellij
# brew cask install --appdir="/Applications" datagrip
# brew cask install --appdir="/Applications" sequel-pro
# brew cask install --appdir="/Applications" atom
# brew cask install --appdir="/Applications" visual-studio-code
# brew cask install --appdir="/Applications" android-studio


## VM stuff
# brew cask install --appdir="/Applications" virtualbox
# brew cask install --appdir="/Applications" vagrant
# brew cask install --appdir="/Applications" parallels

## Misc
# brew cask install --appdir="/Applications" google-chrome
# brew cask install --appdir="/Applications" firefox
brew cask install --appdir="/Applications" skype
brew cask install --appdir="/Applications" slack
brew cask install --appdir="/Applications" dropbox
brew cask install --appdir="/Applications" evernote
brew cask install --appdir="/Applications" 1password
brew cask install --appdir="/Applications" openoffice
brew cask install --appdir="/Applications" transmit
brew cask install --appdir="/Applications" vlc
brew cask install --appdir="/Applications" sketch
brew cask install --appdir="/Applications" adium
brew cask install --appdir="/Applications" calibre
# brew cask install --appdir="/Applications" telegram-desktop
# LaTeX dist MacTex
brew cask install --appdir="/Applications" mactex

## Fonts
brew tap caskroom/fonts
brew cask install font-hack

## Remove outdated versions from the cellar.
brew cleanup
