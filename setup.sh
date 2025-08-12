# System packages
sudo pacman -Sy alacritty apitrace ascii autoconf automake autorandr bash bat bcc-tools binwalk bison bpf bpftrace ccache chez-scheme cmake crash cscope csvlens ctags diffutils docker docker-buildx docker-compose dos2unix dosfstools doxygen dragon-drop emacs expect extra-cmake-modules fcitx5-chinese-addons fcitx5-gtk fcitx5-material-color fd feh ffmpeg flamegraph-git flameshot flex freeimage fx gdb git-lfs glew glfw glm glmark2 glow glslang gnu-netcat go gperf gperftools graphviz grep grpc gst-libav gst-plugin-va gst-plugins-bad gst-plugins-good gzip heaptrack i3-wm i3lock-fancy-git i3status-rust jdk-openjdk jemalloc jq k9s kubectl less libcgroup liblo libtool lightdm-slick-greeter linux-headers-meta linux-meta linux-nvidia-meta llvm logrotate lrzip lsof ltrace make man-db man-pages mariadb maxima mesa-utils meson minicom minikube mold moreutils neovim net-tools netctl networkmanager networkmanager-pptp networkmanager-vpnc nm-connection-editor nmap npm nvidia-utils nvtop-git opencv openfortivpn openssh opus-tools os-prober otf-stix pacman patch patchelf perf perl pkgconf plocate polkit-kde-agent pre-commit pwndbg python-bcc python-dbus python-lsp-server python-pip python-pyflakes python-pylint python-pynvim python-requests qemu-desktop qemu-system-aarch64 qt5ct qt6-svg qt6-webengine qt6ct r2ghidra radare2 rcm renderdoc ripgrep rofi rofi-emoji rsync ruby sdl2_mixer sdl2_ttf sed socat sox strace sudo taglib tar tcpdump texinfo texlive-bin texlive-fontsrecommended texlive-langchinese texlive-langcjk texlive-latex texlive-latexextra texlive-plaingeneric tinyxxd tk tmux trace-cmd traceroute tree ttf-font-awesome ttf-jetbrains-mono ttf-twemoji typescript-language-server unrar unzip uv valgrind vmtouch vnstat wabt wasm-pack wasmer websocat wget which wireshark-qt wmctrl wpa_supplicant xclip xdg-user-dirs xdg-utils xz yarn yay zathura-pdf-mupdf zip zsh
yay -Sy dingtalk-bin google-chrome hotspot

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
go install github.com/xo/usql@master

# Tools based on python
uv_index=https://pypi.tuna.tsinghua.edu.cn/simple
uv tool install aider-chat --default-index=$uv_index
uv tool install compiledb --default-index=$uv_index
uv tool install frida-tools --default-index=$uv_index
uv tool install httpie --default-index=$uv_index
uv tool install iredis --default-index=$uv_index
uv tool install --with diagrams,matplotlib,numpy,jupyter-console,pandas jupyterlab --default-index=$uv_index
uv tool install mitmproxy --default-index=$uv_index
uv tool install mycli --default-index=$uv_index
uv tool install scapy --default-index=$uv_index

ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/ipython $HOME/.local/bin/
ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/jupyter $HOME/.local/bin/
ln -sf $HOME/.local/share/uv/tools/jupyterlab/bin/pygmentize $HOME/.local/bin/

# Tools based on node
sudo npm install -g @qwen-code/qwen-code
