eval "$(/opt/homebrew/bin/brew shellenv)"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

# Added by Windsurf
export PATH="/Users/lau/.codeium/windsurf/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/lau/Downloads/google-cloud-sdk/path.bash.inc' ]; then . '/Users/lau/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/lau/Downloads/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/lau/Downloads/google-cloud-sdk/completion.bash.inc'; fi

# Added by Antigravity
export PATH="/Users/lau/.antigravity/antigravity/bin:$PATH"
