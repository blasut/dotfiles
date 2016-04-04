# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
  colorflag="--color"
else # OS X `ls`
  colorflag="-G"
fi

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

alias ls="command ls -la ${colorflag}"
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'
