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
export GOROOT=/usr/lib/go
export GOPATH=$HOME/.go

# Python virtualenv
export WORKON_HOME=$HOME/.local/share/virtualenv

# JAVA_HOME
export JAVA_HOME=${JAVA_HOME:=/usr/lib64/jvm/default}

# debuginfod urls
export DEBUGINFOD_URLS="https://repo.archlinuxcn.org"

# Additional PATH
pathmunge $HOME/.go/bin after
pathmunge $HOME/.local/share/virtualenv/daily/.venv/bin after
pathmunge $HOME/.cargo/bin after
pathmunge /usr/share/bcc/tools after
pathmunge /usr/share/bcc/introspection after
export PATH=$PATH

# riggrep
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/ripgreprc

# manpage
export MANPAGER="sh -c 'col -bx | bat -l man -p --theme ansi'"
export MANROFFOPT="-c"

export OPENAI_API_BASE=https://ark.cn-beijing.volces.com/api/v3
export AIDER_MODEL=openai/deepseek-v3-250324

if [ -f "$HOME/.zshenv.private" ]; then
    source $HOME/.zshenv.private
fi
