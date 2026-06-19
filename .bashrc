# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
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
unset rc

# Disable Ctrl-S and Ctrl-Q flow control to prevent terminal freezing
# This allows using Ctrl-S (i-search) without freezing the terminal
stty -ixon

# Set unlimited history size and ensure history is saved after each command
HISTSIZE=-1
HISTFILESIZE=-1
# export PROMPT_COMMAND="history -a; history -n;$PROMPT_COMMAND"
shopt -s histappend

s(){ du -ah --max-depth=1 "$@" | sort -h;}

export VISUAL=emtui
export EDITOR="$VISUAL"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/berkay/opt/miniforge3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/berkay/opt/miniforge3/etc/profile.d/conda.sh" ]; then
        . "/home/berkay/opt/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="/home/berkay/opt/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba shell init' !!
export MAMBA_EXE='/home/berkay/opt/miniforge3/bin/mamba';
export MAMBA_ROOT_PREFIX='/home/berkay/opt/miniforge3';
__mamba_setup="$("$MAMBA_EXE" shell hook --shell bash --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias mamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<

# opencode
export PATH=/home/berkay/.opencode/bin:$PATH

if [[ "$PATH" == *"/nix/store"* ]]; then
    PS1=" ❄️ - $PS1"
fi

alias ff='fastfetch -c examples/25'
