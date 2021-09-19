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
export PATH="$PATH:$HOME/.gem/ruby/2.7.0/gems/lydown-0.14.0/bin/"
export PATH="$PATH:$HOME/.ghcup/bin"

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
    echo "------- COMPILER -------"
    echo "g++ ... -O2 $1 -o output.out"
    echo -ne "\n"

    compilation_beg_time="$(date +%s%N)"
    # g++ -Wall -Wextra -Wcast-align -Wcast-qual -Wctor-dtor-privacy \
    #     -Wdisabled-optimization -Wformat=2 -Winit-self -Wlogical-op \
    #     -Wmissing-include-dirs -Wnoexcept -Wold-style-cast -Woverloaded-virtual \
    #     -Wredundant-decls -Wshadow -Wsign-conversion -Wsign-promo -Wstrict-null-sentinel \
    #     -Wstrict-overflow=5 -Wswitch-default -Wundef -Wconversion -Wuseless-cast \
    #     -Wzero-as-null-pointer-constant -O2 -march=native -O2 "$1" -o "output.out" || exit

    g++ -D NDEBUG -g -std=c++14 -Wall -Wextra -Weffc++ -Waggressive-loop-optimizations -Wc++0x-compat -Wc++11-compat -Wc++14-compat -Wcast-align -Wcast-qual -Wchar-subscripts -Wconditionally-supported -Wconversion -Wctor-dtor-privacy -Wempty-body -Wfloat-equal -Wformat-nonliteral -Wformat-security -Wformat-signedness -Wformat=2 -Winline -Wlarger-than=8192 -Wlogical-op -Wnon-virtual-dtor -Wopenmp-simd -Woverloaded-virtual -Wpacked -Wpointer-arith -Wredundant-decls -Wshadow -Wsign-conversion -Wsign-promo -Wstack-usage=8192 -Wstrict-null-sentinel -Wstrict-overflow=2 -Wsuggest-attribute=noreturn -Wsuggest-final-methods -Wsuggest-final-types -Wsuggest-override -Wswitch-default -Wswitch-enum -Wsync-nand -Wundef -Wunreachable-code -Wunused -Wuseless-cast -Wvariadic-macros -Wno-literal-suffix -Wno-missing-field-initializers -Wno-narrowing -Wno-old-style-cast -Wno-varargs -fcheck-new -fsized-deallocation -fstack-check -fstack-protector -fstrict-overflow -flto-odr-type-merging -fno-omit-frame-pointer -fPIE -fsanitize=address -fsanitize=alignment -fsanitize=bool -fsanitize=bounds -fsanitize=enum -fsanitize=float-cast-overflow -fsanitize=float-divide-by-zero -fsanitize=integer-divide-by-zero -fsanitize=leak -fsanitize=nonnull-attribute -fsanitize=null -fsanitize=object-size -fsanitize=return -fsanitize=returns-nonnull-attribute -fsanitize=shift -fsanitize=signed-integer-overflow -fsanitize=undefined -fsanitize=unreachable -fsanitize=vla-bound -fsanitize=vptr -march=native -O2 -lm -pie "$1" -o "output.out" || return

    compilation_end_time="$(date +%s%N)"

    echo "-------- OUTPUT --------"
    run_beg_time="$(date +%s%N)"
    ./output.out
    run_end_time="$(date +%s%N)"
    echo -ne "\n"

    compilation_time="$((($compilation_end_time - $compilation_beg_time)/1000000))"
    run_time="$((($run_end_time - $run_beg_time)/1000000))"

    number_length="$((${#compilation_time} > ${#run_time} ? ${#compilation_time} : ${#run_time}))"

    echo "-------- STATUS --------"
    printf "Compilation time: %*sms\n" "$number_length" "$compilation_time"
    printf "Execution   time: %*sms\n" "$number_length" "$run_time"

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
