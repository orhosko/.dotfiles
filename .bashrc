#export PATH="/Directory1:$PATH" .bashrc

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

export VISUAL=nvim
export EDITOR="$VISUAL"

# --------------------------------------
unset rc
export PATH=$PATH:/home/berkay/.spicetify:/opt/riscv64_1/bin
. "$HOME/.cargo/env"
