# ----------- Oh My ZSH ----------
export ZSH="/home/alex/.oh-my-zsh"
ZSH_THEME="robbyrussell"

HYPHEN_INSENSITIVE="true"

DISABLE_UPDATE_PROMPT="true"
export UPDATE_ZSH_DAYS=5

ENABLE_CORRECTION="true"

HIST_STAMPS="yyyy-mm-dd"

plugins=(git)

source $ZSH/oh-my-zsh.sh
# --------------------------------

# ------------ Locale ------------
export LANG=en_US.UTF-8 export LC_CTYPE=ru_UA.UTF-8
# --------------------------------

# ----------- Defaults -----------
export EDITOR='emacsclient -nw -a ""'
export BROWSER='firefox'
# --------------------------------

# ------------ Aliases -----------
# Replacements
alias ls='exa'

# Configs
alias zshcf="$EDITOR $HOME/.zshrc"
alias xmocf="$EDITOR $HOME/.xmonad/xmonad.hs"
alias vimcf="$EDITOR $HOME/.config/nvim/init.vim"
alias piccf="$EDITOR $HOME/.config/picom/picom.conf"
alias alacf="$EDITOR $HOME/.config/alacritty/alacritty.yml"
alias xprcf="$EDITOR $HOME/.xprofile"

# Useful stuff
alias youtube-dl-sound='youtube-dl --ignore-errors --output "%(title)s.%(ext)s" --extract-audio --audio-format mp3'
alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

# Image preview inside vifm
alias vifm="$HOME/.config/vifm/scripts/vifmrun"

# Use emacsclient to connect to emacs daemon
emacs-gui() {
    xdo hide $(xdo id)
    emacsclient -nc $@
    exit
}
alias emacs-cli='emacsclient -nw -a ""'
# --------------------------------

# ------------ Scripts -----------
# Export .local/bin in path for scripts
export PATH="$PATH:$HOME/.local/bin/"
export PATH="$PATH:/opt/mxe/usr/bin/"

song-dl() {
    video=$(echo "$1" | sed 's/\\//g' | grep -oh 'https://www.youtube.com/watch?v=.\{11\}')
    if ! grep -Fxq "$video" "$HOME/music/from-youtube/music-links"; then
        echo "$video" >> "$HOME/music/from-youtube/music-links"
    else
        echo "This link already exists in $HOME/music/from-youtube/music-links file!"
        return
    fi

    youtube-dl --ignore-errors --output "$HOME/music/%(title)s.%(ext)s" --extract-audio --audio-format mp3 "$video"
}

run-cpp() {
    g++ -O3 "$1" -o "output.out"
    ./output.out
    rm output.out
}
# --------------------------------

# ---------- Swallowing ----------
swallow() {
    # get terminal window id
    wid=$(xdo id)

    # hide terminal
    xdo hide "$wid"

    "$@" # run program

    # show terminal again
    xdo show "$wid"
}

alias sxiv='swallow sxiv'
alias mupdf='swallow mupdf'
alias zathura='swallow zathura'
alias mpv='swallow mpv'
# --------------------------------

# ------------ SDKman ------------
export SDKMAN_DIR="/home/alex/.sdkman"
[[ -s "/home/alex/.sdkman/bin/sdkman-init.sh" ]] &&
    source "/home/alex/.sdkman/bin/sdkman-init.sh"
# --------------------------------

# Use vim bindings for zsh
bindkey -v
