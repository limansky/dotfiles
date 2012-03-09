# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/mlimansk/.zshrc'

autoload -Uz compinit 
compinit
# End of lines added by compinstall

eval $(dircolors)

setopt hist_ignore_all_dups

autoload -U promptinit
promptinit
prompt adam2 8bit grey
zle_highlight="default"

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' menu select=2

bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "^[[3~" delete-char

export EDITOR=/usr/bin/vim

#aliases
alias ls='ls --color=auto'
alias harm_X='Xephyr :1 -host-cursor -screen 864x480x16 -dpi 96 -ac +extension Composite'
alias frem_X='Xephyr :1 -host-cursor -screen 800x480x16 -dpi 96'
