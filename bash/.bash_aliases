alias d='date'

alias vi='vim'

alias ack='ack-grep'

alias li='li -l'

alias rspec='rspec --color'

alias prepare_db='bundle exec rake db:migrate; bundle exec rake db:test:prepare'
alias pdb='bundle exec rake db:migrate; bundle exec rake db:test:prepare'

alias prepare_assets='bundle exec rake assets:precompile'
alias pa='bundle exec rake assets:precompile'

alias coffe='coffee'
alias cofee='coffee'
alias c='coffe -wc'

alias bers='bundle exec rspec spec'
alias be='bundle exec'
alias rs='rspec spec'

alias r='rails'

alias rss='bundle exec thin start -e development --ssl --ssl-verify'

alias gti='git'
alias g='git'
alias ga='git add -all'
alias gaa='git add -all'
alias gs='git status'
alias gd='git di'
alias gm='git commit -m'
alias gcm='git commit -m'
alias gp='git push'
alias gsy='git pull --rebase'
alias gph='git push heroku master'
alias pwn='git push; git push heroku'

alias gpa='gpa --disable-x509'

alias vm='ssh vagrant@127.0.0.1 -p 2222'

alias ls='ls -la'

alias kboff='sudo kextunload /System/Library/Extensions/AppleUSBTopCase.kext/Contents/PlugIns/AppleUSBTCKeyboard.kext'
alias kbon='sudo kextload /System/Library/Extensions/AppleUSBTopCase.kext/Contents/PlugIns/AppleUSBTCKeyboard.kext'