# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Stolen from /etc/profile
# Need to redefine pathmunge, it gets undefined at the end of /etc/bashrc
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

# Golang
export GOROOT=/usr/lib/go
export GOPATH=$HOME/.go

# Python virtualenv
export WORKON_HOME=$HOME/.conda/envs

# JAVA_HOME
export JAVA_HOME=${JAVA_HOME:=/usr/lib64/jvm/default}

# Additional PATH
pathmunge $HOME/.go/bin after
pathmunge $HOME/.conda/envs/daily/bin after
pathmunge $HOME/.cargo/env after
pathmunge /usr/share/bcc/tools after
pathmunge /usr/share/bcc/introspection after
export PATH=$PATH

# riggrep
export RIPGREP_CONFIG_PATH=~/.config/ripgrep/ripgreprc

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

