################################################################################
#====[ Aliases ]================================================================
################################################################################

shopt -s expand_aliases

#====[ Interactive ]============================================================
alias rm='rm -i' # Prevents accidentally clobbering files
alias cp='cp -i'
alias mv='mv -i'

#====[ With options ]===========================================================
alias mkdir='mkdir -p' 
alias h='history'
alias j='jobs -l'
alias which='type -a'
alias ..='cd ..'

#====[ Path ]===================================================================
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'

#====[ Human readable ]=========================================================
alias du='du -kh'
alias df='df -kTh'

#====[ Color ]==================================================================
alias grep='grep --color=auto'
alias diff='diff --color=auto'

#====[ ls ]=====================================================================
alias ls='ls -h --color=auto' # Add colors for filetype and human-readable sizes by default on 'ls':
alias lx='ls -lXB'            #  Sort by extension.
alias lk='ls -lSr'            #  Sort by size, biggest last.
alias lt='ls -ltr'            #  Sort by date, most recent last.
alias lc='ls -ltcr'           #  Sort by/show change time,most recent last.
alias lu='ls -ltur'           #  Sort by/show access time,most recent last.
alias lst='ls -R | grep ":$" | sed -e '"'"'s/:$//'"'"' -e '"'"'s/[^-][^\/]*\//--/g'"'"' -e '"'"'s/^/ /'"'"' -e '"'"'s/-/|/'"'"''
alias lst2='find . | sed -e "s/[^-][^\/]*\// |/g" -e "s/|\([^ ]\)/|-\1/"'
alias ll="ls -lv --group-directories-first" # The ubiquitous 'll': directories first, with alphanumeric sorting:
alias lm='ll | more'          #  Pipe through 'more'
alias lr='ll -R'              #  Recursive ls.
alias la='ll -A'              #  Show hidden files.
alias tree='tree -Csuh'       #  Nice alternative to 'recursive ls' ...

#====[ Pager ]==================================================================
alias more='less' # more is less

#====[ tmux ]===================================================================
alias tmux="tmux -2"

#====[ R ]======================================================================
alias R='R --quiet --no-save'
