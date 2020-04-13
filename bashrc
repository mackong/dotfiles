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

# Golang
export GOROOT=/usr/lib/go
export GO111MODULE=on
export GOPATH=$HOME/.go

# Python virtualenv
export VIRTUALENVWRAPPER_PYTHON=python3
export WORKON_HOME=$HOME/.virtualenvs

# JAVA_HOME
export JAVA_HOME=${JAVA_HOME:=/usr/lib64/jvm/default}

# Additional PATH
pathmunge $GOPATH/bin after
pathmunge $HOME/.virtualenvs/daily-*/bin after
export PATH

# Auto change directory in emacs
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() {
        print -P "\032/$(pwd)"
    }
fi

