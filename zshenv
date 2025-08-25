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

# manpage
export MANPAGER="sh -c 'col -bx | bat -l man -p --theme ansi'"
export MANROFFOPT="-c"

# export OPENAI_API_BASE=https://ark.cn-beijing.volces.com/api/v3
# export AIDER_MODEL=openai/deepseek-v3-250324
export OPENAI_BASE_URL="https://dashscope.aliyuncs.com/compatible-mode/v1"
export OPENAI_MODEL="qwen3-coder-plus"
export AIDER_MODEL=openai/qwen3-coder-plus

# homebrew
export HOMEBREW_API_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"


# bun completions
[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"

if [ -f "$HOME/.zshenv.private" ]; then
    source $HOME/.zshenv.private
fi
