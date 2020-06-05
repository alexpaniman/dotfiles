# ------- Oh My ZSH -------
export ZSH="/home/alex/.oh-my-zsh"
ZSH_THEME="gallois"

HYPHEN_INSENSITIVE="true"

DISABLE_UPDATE_PROMPT="true"
export UPDATE_ZSH_DAYS=5

ENABLE_CORRECTION="true"

HIST_STAMPS="yyyy-mm-dd"

plugins=(git)

source $ZSH/oh-my-zsh.sh
# -------------------------

# ------- Locale -------
export LANG=en_US.UTF-8
export LC_CTYPE=ru_UA.UTF-8
# ----------------------

# ------- Defaults -------
export EDITOR='nvim'
export BROWSER='firefox'
# ------------------------

# ------- Aliases -------
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
# -----------------------

# ------- Scripts -------
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
# -----------------------

# ------- SDKman -------
export SDKMAN_DIR="/home/alex/.sdkman"
[[ -s "/home/alex/.sdkman/bin/sdkman-init.sh" ]] && source "/home/alex/.sdkman/bin/sdkman-init.sh"
# ----------------------

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

bindkey -v
