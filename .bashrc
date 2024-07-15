#export PATH="/Directory1:$PATH" .bashrc

HISTSIZE=-1
HISTFILESIZE=-1

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

# --------------------------------------
export PATH="$HOME/.config/emacs/bin:$PATH"

alias em="emacsclient -c -a ''"
alias fanstate="cat /sys/devices/platform/asus-nb-wmi/fan_boost_mode"

function fcd {
  cd $(find ~ \( \
    -name "node_modules" -o \
    -name ".*" \
  \) -prune -false -o -type d -print | fzf)
}

alias dupd="sudo dnf update"
alias fupd="flatpak update"
alias upd="dupd; fupd"

export VISUAL=nvim
export EDITOR="$VISUAL"

# --------------------------------------
unset rc
export PATH=$PATH:/home/berkay/.spicetify:/opt/riscv64_1/bin
. "$HOME/.cargo/env"


eval "$(starship init bash)"
