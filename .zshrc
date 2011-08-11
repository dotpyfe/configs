
if [[ `uname -s` == "Darwin" ]]; then
    # MacPorts
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export MANPATH=/opt/local/share/man:$MANPATH
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

#### PATH #####
export PATH=~/.cabal/bin:~/bin:$PATH
###

# Check to see if keychain is installed and start it if it is
if [[ -x /usr/bin/keychain ]]; then 
    /usr/bin/keychain -Q -q --nogui ~/.ssh/id_rsa
    [[ -f $HOME/.keychain/$HOST-sh ]] && source $HOME/.keychain/$HOST-sh
fi

autoload -U compinit 
compinit

setopt prompt_subst

autoload colors ; colors

# Set up prompt
for color in RED GREEN BLUE WHITE; do
    eval PR_$color='%{$fg[${(L)color}]%}'
done
PR_RESET="%{${reset_color}%}";

export PROMPT="${PR_GREEN}%n${PR_BLUE}@${PR_GREEN}%m${PR_RESET} %# "
export RPROMPT="%(?//${PR_RED}[%?] )${PR_BLUE}%~${PR_RESET}"
 
# Assign default editor
export EDITOR="vim"

# Set up aliases
alias -g L='| less'
alias pacman='pacman-color'

# Completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# Fuzzy matching of completions
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle -e ':completion:*:approximate:*' \
        max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
# Process kill by name
zstyle ':completion:*:*:kill:*:processes' command \
        'ps -axco pid,user,command'

# Globbing
setopt extended_glob

# Command spelling correction
setopt correct
setopt auto_cd

# Named directories
code=~/code
c=~/code/C
haskell=~/code/haskell
python=~/code/python
: ~code ~c ~haskell ~python

# Modal vi editing for cmdline
bindkey -v
# Keep ctr-r though. Vim search sucks
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward


# ZSH Functions
function mkpw () {
    if (( $# == 0 )) then
        head /dev/urandom | uuencode -m - | sed -n 2p | cut -c1-${1:-12}
    else
        head /dev/urandom | uuencode -m - | sed -n 2p | cut -c1-${1:-$1}
    fi
}
