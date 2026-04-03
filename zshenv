pathmunge () {
    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
    esac
}

export EDITOR=vim

export COLORTERM=truecolor

export IZK_INPUT_MODE="emacs"

# Golang
export GOPATH=$HOME/.go

# JAVA_HOME
if [[ $(uname) == "Darwin" ]]; then
    export JAVA_HOME=`/usr/libexec/java_home`
elif [[ "${$(uname -s)[1,5]}" == "Linux" ]]; then
    export JAVA_HOME=${JAVA_HOME:=/usr/lib64/jvm/default}
fi

# debuginfod urls
export DEBUGINFOD_URLS="https://repo.archlinuxcn.org"

# bun
export BUN_INSTALL="$HOME/.bun"

# Additional PATH
if [[ $(uname) == "Darwin" ]]; then
    pathmunge /opt/homebrew/bin before
fi
pathmunge $HOME/.local/bin after
pathmunge $HOME/.go/bin after
pathmunge $HOME/.cargo/bin after
pathmunge $BUN_INSTALL/bin after
pathmunge /usr/share/bcc/tools after
pathmunge /usr/share/bcc/introspection after
export PATH=$PATH

# riggrep
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/ripgreprc

# plantuml
export PLANTUML_JAR=$HOME/.config/emacs/share/plantuml/plantuml.jar

# manpage
export MANPAGER="sh -c 'col -bx | bat -l man -p --theme ansi'"
export MANROFFOPT="-c"

# homebrew
export HOMEBREW_API_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"

# bun completions
[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"

# vterm
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

if [ -f "$HOME/.zshenv.private" ]; then
    source $HOME/.zshenv.private
fi
