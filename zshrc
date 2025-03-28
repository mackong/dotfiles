# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  z mvn golang rust docker
)

DISABLE_MAGIC_FUNCTIONS=true

source $ZSH/oh-my-zsh.sh

if [ -f ~/.zshenv ]; then
    source ~/.zshenv
fi

# User configuration

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
export GOPATH=$HOME/.go:$GOPATH

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
export RIPGREP_CONFIG_PATH=~/.config/ripgrep/ripgreprc

# manpage
export MANPAGER="sh -c 'col -bx | bat -l man -p --theme ansi'"
export MANROFFOPT="-c"

# Aider
export OPENAI_API_BASE=https://ark.cn-beijing.volces.com/api/v3
export AIDER_MODEL=openai/deepseek-v3-250324

# Keybindings
bindkey '\e#' pound-insert
bindkey '^[[Z' reverse-menu-complete

# Aliases
alias xclip='xclip -sel clip'
alias cat='bat -pp --theme ansi'

