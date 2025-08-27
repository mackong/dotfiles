# System packages
if [ "$(uname)" == "Darwin" ]; then
    export HOMEBREW_API_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"
    export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"
    brew install antlr ascii autoconf automake bat bison chezscheme cmake csvlens coreutils dos2unix extra-cmake-modules ffmpeg flex git-lfs go graphviz jq maven maxima meson moreutils neovim node openjdk pre-commit python-lsp-server radare2 rcm ripgrep texlive tmux tree tree-sitter uv websocat xz
    brew install --cask alacritty emacs-app docker google-chrome wireshark-app
    sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    sudo pacman -Sy alacritty antlr4 apitrace ascii autoconf automake autorandr bash bat bcc-tools binwalk bison bpf bpftrace ccache cmake crash cscope csvlens ctags diffutils docker docker-buildx docker-compose dos2unix dosfstools doxygen emacs expect extra-cmake-modules fcitx5-chinese-addons fcitx5-gtk fcitx5-material-color fd feh ffmpeg flameshot flex fx gdb git-lfs glew glfw glm glmark2 glow glslang gnu-netcat go gperf gperftools graphviz grep grpc gst-libav gst-plugin-va gst-plugins-bad gst-plugins-good gzip heaptrack i3-wm i3status-rust jdk-openjdk jemalloc jq k9s kubectl less liblo libtool lightdm-slick-greeter linux-headers-meta linux-meta linux-nvidia-meta llvm logrotate lrzip lsof ltrace make man-db man-pages mariadb maxima mesa-utils meson minicom minikube mold moreutils neovim net-tools netctl networkmanager networkmanager-pptp networkmanager-vpnc nm-connection-editor nmap nodejs nvidia-utils opencv openfortivpn openssh opus-tools os-prober pacman patch patchelf perf perl pkgconf plocate polkit-kde-agent pre-commit pwndbg python-bcc python-dbus python-lsp-server python-pip python-pyflakes python-pylint python-pynvim python-requests qemu-desktop qemu-system-aarch64 qt5ct qt6-svg qt6-webengine qt6ct r2ghidra radare2 renderdoc ripgrep rofi rofi-emoji rsync ruby sdl2_mixer sdl2_ttf sed socat sox strace sudo taglib tar tcpdump texinfo texlive-bin texlive-fontsrecommended texlive-langchinese texlive-langcjk texlive-latex texlive-latexextra texlive-plaingeneric tinyxxd tk tmux trace-cmd traceroute tree ttf-font-awesome ttf-jetbrains-mono unrar unzip uv valgrind vnstat wabt wasm-pack wasmer websocat wget which wireshark-qt wmctrl wpa_supplicant xclip xdg-user-dirs xdg-utils xz yay zathura-pdf-mupdf zip zsh
    yay -Sy chez-scheme dingtalk-bin dragon-drop flamegraph-git freeimage google-chrome hotspot i3lock-fancy-git libcgroup nvtop-git otf-stix rcm ttf-twemoji vmtouch
fi

# Tools based on Golang
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install golang.org/x/tools/cmd/stringer@latest
go install golang.org/x/lint/golint@latest
go install github.com/nsf/gocode@latest
go install golang.org/x/tools/gopls@latest
go install github.com/rogpeppe/godef@latest
go install github.com/kisielk/errcheck@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install github.com/zeromicro/goctl-swagger@latest

# Tools based on python
uv_index=https://pypi.tuna.tsinghua.edu.cn/simple
uv tool install aider-chat --with audioop-lts --default-index=$uv_index
uv tool install compiledb --default-index=$uv_index
uv tool install frida-tools --default-index=$uv_index
uv tool install httpie --default-index=$uv_index
uv tool install iredis --default-index=$uv_index
uv tool install --with diagrams,matplotlib,numpy,jupyter-console,pandas jupyterlab --default-index=$uv_index
uv tool install litecli --default-index=$uv_index
uv tool install mitmproxy --default-index=$uv_index
uv tool install mycli --default-index=$uv_index
uv tool install scapy --default-index=$uv_index

ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/ipython $HOME/.local/bin/
ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/jupyter $HOME/.local/bin/
ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/pygmentize $HOME/.local/bin/

# Tools based on bun
curl -fsSL https://bun.com/install | bash -s "bun-v1.2.20"
bun install -g @gltf-transform/cli
bun install -g @qwen-code/qwen-code
bun install -g markmap-cli
bun install -g typescript
bun install -g typescript-language-server
