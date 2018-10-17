# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f ~/.bashrc_local ]; then
    source ~/.bashrc_local
fi

# R = colors (ansi escapes); F = don't page if text fits in window; i = case insensitive search
export LESS='-Ri'
export GIT_PAGER='less -Ri'
export EDITOR='emacs -nw'
export VISUAL=emacs

# Set the ls colors. Dircolors returns presets that look better than the default
eval $(dircolors)

# User specific aliases and functions
PATH=$HOME/tools:$PATH

alias l='ls -FG --color'
alias la='ls -FGa --color'
alias ll='ls -FGl --color'
alias lla='ls -FGla --color'

alias gs='git status'
alias gl="git log --graph --date-order --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
alias gb='git branch'
alias gd='git diff --ws-error-highlight=new,old'
alias gdh='git diff HEAD^..HEAD'
alias gco='git checkout'
alias gdc='git diff --cached'
alias gsu='git submodule update'
alias sdu="du -k * | sort -nr | cut -f2 | xargs -d '\n' du -sh"

alias gittop="git rev-parse --show-toplevel"
alias reload='source ~/.bashrc'


function f {
    find . -iname "*$1*"
}

function clip {
    ssh laptop "DISPLAY=:0 xclip -selection clipboard > /dev/null"
}

# Run whatever follows at the top git directory and then come back
# gt = "git top level"
function gtl {
    pushd $(git rev-parse --show-toplevel)
    $@
    popd
}

# Pipe ag | less
function lag {
    ag "$1" --group --color | less -r ;
}

function e {
    emacsclient -a "" -t $@
}

function ev {
    emacsclient -a "" -c $@
}

function bak {
    cp $1 "$1.bak";
}

# Highlight matching regex. Show all lines
function hl {
    GREP_COLORS=ne grep --color=always -P "$1|$"
}

# Print terminal colors with their names. Can be used e.g. in tmux
function print_colors {
    for i in {0..255}; do 
        printf "\x1b[38;5;${i}mcolor%-5i\x1b[0m" $i ;
        if ! (( ($i + 1 ) % 8 )); 
            then echo ; 
        fi ; 
    done
}

function ediff {
    if [[ "$#" -ne 2 ]]; then
        echo "Usage: ediff <file 1> <file 2>";
    else
        emacs -q --eval "(ediff-files \"$1\" \"$2\")";
    fi
}

# Time the given commands and append to the file in seconds
function mytime {
    /usr/bin/time -o ~/build_times.txt -a -f "%e" $@
}

# Ring bell. Useful if called from PS1 to notify
# that a long command (e.g. a build) has finished
function bell {
    echo -n -e "\a"
}

function highlight() {
	declare -A fg_color_map
	fg_color_map[black]=30
	fg_color_map[red]=31
	fg_color_map[green]=32
	fg_color_map[yellow]=33
	fg_color_map[blue]=34
	fg_color_map[magenta]=35
	fg_color_map[cyan]=36
	 
	fg_c=$(echo -e "\e[1;${fg_color_map[$1]}m")
	c_rs=$'\e[0m'
	sed -u -r s"/$2/$fg_c\0$c_rs/g"
}

#function stow {
#    /usr/bin/stow -t /usr/local $@
#}
#
#function unstow {
#    /usr/bin/stow -D -t /usr/local $@
#}

# Disable the super annoying Ctrl+S that freezes the terminal
if [[ $- == *i* ]]  # interactive shell
then
    stty -ixon
fi

# enable core files
ulimit -c unlimited



##########
# Get ssh agent working
export SSH_AUTH_SOCK=~/.ssh/ssh-agent.$HOSTNAME.sock
ssh-add -l 2>/dev/null >/dev/null
if [ $? -ge 2 ]; then
    ssh-agent -a "$SSH_AUTH_SOCK" >/dev/null
    ssh-add  # load the default key
fi
##########

source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"  # Show if you're ahead (>) or behind (<) upstream

# # Make prompt pretty
# green=$(tput setaf 2)
# blue=$(tput setaf 4)
# bold=$(tput bold)
# red=$(tput setaf 1)
# reset=$(tput sgr0)

function my_git_branch_ps1 () {
    local _branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if [ -z "${_branch}" ]; then
       echo ""
    else
        echo "($_branch) "
    fi
}


function setPS1()
{
	# Reset
	local Reset='\[\e[0m\]'             # Text Reset

	# Regular s
	local Black='\[\e[0;30m\]'        # Black
	local Red='\[\e[0;31m\]'          # Red
	local Green='\[\e[0;32m\]'        # Green
	local Yellow='\[\e[0;33m\]'       # Yellow
	local Blue='\[\e[0;34m\]'         # Blue
	local Purple='\[\e[0;35m\]'       # Purple
	local Cyan='\[\e[0;36m\]'         # Cyan
	local White='\[\e[0;37m\]'        # White

	# Bold
	local BBlack='\[\e[1;30m\]'       # Black
	local BRed='\[\e[1;31m\]'         # Red
	local BGreen='\[\e[1;32m\]'       # Green
	local BYellow='\[\e[1;33m\]'      # Yellow
	local BBlue='\[\e[1;34m\]'        # Blue
	local BPurple='\[\e[1;35m\]'      # Purple
	local BCyan='\[\e[1;36m\]'        # Cyan
	local BWhite='\[\e[1;37m\]'       # White

	local ColorArray=($BRed $BGreen $BYellow $BBlue $BCyan $BRed $BGreen $BBlue $BYellow $BCyan)     # Need 10 options since there's no modulo
	local ColorForHost=${ColorArray[$(echo "${USER}@${HOSTNAME}" | md5sum | sed s/[abcdef]*// | head -c 1)]}  # get first single digit from hash

	# export PS1="[${ColorForHost}\h ${BBlue}\w]${Red}\$(__git_ps1)${Reset} $ "
	# export PS1="[${ColorForHost}\h ${BBlue}\w]${Reset} $ "
	export PS1="[${ColorForHost}\h ${BBlue}\w] ${Red}\$(my_git_branch_ps1)${Reset}$ "
}

setPS1
