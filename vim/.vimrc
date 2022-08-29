filetype detect

"====[ Vim shell as command prompt ]===========================================
"set shellcmdflag=-ic

"====[ Bash aliases ]==========================================================
let $BASH_ENV="~/.bash_aliases"

"====[ Remap ]=================================================================
" kj and jk to <Esc> 
inoremap kj <Esc>
inoremap jk <Esc>
" Use space to jump down a page
nnoremap <Space> <PageDown>
xnoremap <Space> <PageDown>

"====[ Highlighting ]==========================================================
" Syntax
syntax enable
" Search
set hlsearch
" Current line in insert mode
autocmd InsertEnter,InsertLeave * set cul!
" Comments
highlight Comment term=bold ctermfg=white
" Highlighting with vimdiff
if &diff
  highlight! link DiffText MatchParen
endif

"====[ Line numbers ]==========================================================
set number relativenumber
set nu rnu

"====[ Enable tabs in makefiles ]==============================================
autocmd FileType make set noexpandtab

"====[ Tabs to two spaces ]====================================================
set shiftwidth=2 softtabstop=2 expandtab

"====[ Fuzzy find and Wild menu ]==============================================
set path+=**
set wildmenu

"====[ Automatically remove all trailing whitespace ]==========================
autocmd FileType R,r,Rmd,rmd,MD,md,Md,Py,py,m,adoc autocmd BufWritePre <buffer> %s/\s\+$//e

"====[ Show when lines extend past column 80 ]=================================
highlight ColorColumn ctermbg=magenta
" Color bar
" set colorcolumn=81
" Show color only when a line extends past column 80
call matchadd('ColorColumn', '\%81v', 100)

"====[ Markdown ]==============================================================
autocmd Filetype markdown map <F5> :!pandoc<space><c-r>%<space>-s<space>--toc<space>-c<space>pandoc.css<space>--mathjax<space>-o<space>%:r.html<enter><enter>
autocmd Filetype markdown map <F6> :!pandoc<space>-t<space>gfm<space>--atx-headers<space><c-r>%<space>-s<space>-o<space><c-r>%<enter>

"====[ RMarkdown ]=============================================================
autocmd Filetype Rmd,rmd,R,r map <F5> :!echo<space>"require(rmarkdown);<space>render('<c-r>%')"<space>\|<space>R<space>--vanilla<enter><enter>
autocmd Filetype Rmd,rmd,R,r map <F6> :!echo<space>"require(styler);<space>style_file('<c-r>%')"<space>\|<space>R<space>--vanilla<enter>

"====[ Colorscheme ]===========================================================
set background=dark
colorscheme default

"====[ vim-slime ]=============================================================
let g:slime_target = "vimterminal"
