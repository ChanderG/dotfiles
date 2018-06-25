syntax on

set number
set relativenumber

"indent options
set ai

"search incrementally and highlighting
set incsearch
set hlsearch

"show matching brackets
set showmatch

"tab completion
set wildmode=longest:full
set wildmenu

"set semi-colon to colon
nore ; :
nore : ;

" open up the full color range
set t_Co=256
"dark background color
set background=dark

"shift >>/<< width
set shiftwidth=0

set tabstop=2

"Use Enter/Shift-Enter to introduce new lines above/below w/o leaving normal mode
map <Enter> o<ESC>

call plug#begin('~/.vim/bundle')

"""scala highlighting
Plug 'derekwyatt/vim-scala'

"""License adder
Plug 'antoyo/vim-licenses'

"""racket highlighting
Plug 'wlangstroth/vim-racket'

"""find ideal positions to jump to
Plug 'bradford-smith94/quick-scope'

"""list of tags
Plug 'majutsushi/tagbar'

"""comment toggle
Plug 'tpope/vim-commentary'

"""for auto git diffs
Plug 'airblade/vim-gitgutter'

"""syntax based code folding for python
Plug 'tmhedberg/SimpylFold'

"""slime for vim
Plug 'jpalardy/vim-slime'

"""zeavim - access zeal docs from vim
Plug 'kabbamine/zeavim.vim'

""" Online docs
Plug 'keith/investigate.vim'

""" Neocomplete - general purpose auto complete frontend
Plug 'Shougo/neocomplete.vim'

""" Python autocomplete engine
" Requires jedi(pip)
Plug 'davidhalter/jedi-vim'

""" Syntax checking
Plug 'scrooloose/syntastic'

""" Auto formatting
Plug 'chiel92/vim-autoformat'

""" Snippets
" engine
Plug 'SirVer/ultisnips'
" snippets
Plug 'honza/vim-snippets'

""" extra objects
Plug 'michaeljsmith/vim-indent-object'

""" C/C++ completion engine based on clang
Plug 'Rip-Rip/clang_complete'

""" More text objects
Plug 'wellle/targets.vim'

""" Sneak - medium distance motion
Plug 'justinmk/vim-sneak'

""" Better word motion in terms of camel and snake case
Plug 'chaoren/vim-wordmotion'

""" Tagfile management
Plug 'ludovicchabant/vim-gutentags'

""" Trying out surround
Plug 'machakann/vim-sandwich'

""" Pony syntax support
Plug 'jakwings/vim-pony'

""" Async runner
Plug 'skywind3000/asyncrun.vim'

""" Go editing engine
Plug 'fatih/vim-go'

""" 256 Color theme
Plug 'morhetz/gruvbox'

""" Custom text objects
Plug 'kana/vim-textobj-user'
Plug 'glts/vim-textobj-comment'
Plug 'kana/vim-textobj-function'

""" Extra syntax highlighting for C
Plug 'justinmk/vim-syntax-extra'

""" Extra syntax highlight for many languages
Plug 'sheerun/vim-polyglot'

""" Autocomplete stuff from other tmux panes
Plug 'wellle/tmux-complete.vim'
call plug#end()

" set color scheme
colorscheme gruvbox

"""For the License Adder
let g:licenses_authors_name = 'Govindarajan, Chander <chandergovind@gmail.com>'
let g:licenses_copyright_holders_name = 'Govindarajan, Chander <chandergovind@gmail.com>'

""" set terminal mode
set term=rxvt-unicode
""" this way, by using tmux (with xterm keys on) also works

""" for urxvt
map [5^ <C-PageUp>
map [6^ <C-PageDown>

""" Tabs management

"""switching between tabs
nnoremap <C-PageUp> :tabprevious<CR>
nnoremap <C-PageDown> :tabnext<CR>

""" new tab creation
nnoremap tn :tabnew<CR>

""" Timeouts for commands
set notimeout
set ttimeout

""" Show partial commands
set showcmd

""" QuickFix window options

""" direct move to next
" nmap cn ;cn<CR>
" no mapping for prev -- as I rarely need it

" In the quickfix window, <CR> is used to jump to the error under the
" cursor, so undefine the mapping there.
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
" required to counter the effect of our mapping of <CR> in clist window

"" location list

""" A better escape
inoremap <silent> <Left> <ESC><Left>
inoremap <silent> <Right> <ESC><Right>

""" remove highlighting till next search
nnoremap <silent> <esc> :noh<return><esc>

""" Courtesy of nvie/vimrc

""" to paste without mass auto indendation
set pastetoggle=<F2>

""" fold using syntax
set foldmethod=syntax
set foldclose=all "automatically reclose after navigating out
set foldlevel=10

" open close folds with space
nnoremap <space> za

""" underline current line
set cursorline
noremap <silent> <F3> :set nocursorline!<CR>

""" keep para indentation when wrapping text
set breakindent

" Quick yanking to the end of the line
nnoremap Y y$

""" End courtesy

""" slime configuration
let g:slime_target = "tmux"
noremap <silent> X :SlimeSendCurrentLine<CR>

""" Neocomplete settings
set completeopt=longest,menu,menuone
let g:neocomplete#enable_at_startup=1
" have selection on first option
let g:neocomplete#enable_auto_select = 1
" cancel the current neocomplete suggestion and use one of these instead
" inoremap <tab>o <c-e><c-x><c-o>
" inoremap <tab>] <c-e><c-x><c-]>
" inoremap <tab>l <c-e><c-x><c-l>

""" Syntastic configuration
let g:syntastic_always_populate_loc_list = 1

"" python checkers
" Require: pylint (apt-get)
let g:syntastic_python_checkers = ['pylint', 'pep8']

"" c/cpp checkers
" Require: gcc , cppcheck (apt-get), splint(apt-get), clang(apt-get)
let g:syntastic_c_checkers = ['gcc','cppcheck','splint','clang_check','clang_tidy']
let g:syntastic_cpp_checkers = ['gcc','cppcheck','clang_check','clang_tidy']

""" Auto formatting
" format on save
au BufWrite *.py :Autoformat
au BufWrite *.c :Autoformat
au BufWrite *.cpp :Autoformat
" python: formatting requries python-autopep8 (apt-get)
" c/c++ : clangformat, comes with clang (apt-get)

" snippets
let g:UltiSnipsExpandTrigger="<c-tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

""" clang_complete
" Require: clang (apt-get), exact path needs to be put here
let g:clang_library_path='/usr/lib64/llvm/libclang.so'
let g:clang_complete_loaded="0"

""" jedi-vim
let g:jedi#show_call_signatures = "2"
let g:jedi#popup_select_first = 1

""" ctags configuration
" searches upwards for tags file
set tags=./tags;

""" cscope configuration
" auto loading cscope database
" similar functionality with tags -> set tags=tags;/ (auto towards)
fun! LoadCscope()
    let db = findfile("cscope.out", '.;')
    if (!empty(db))
	let path = strpart(db, 0, match(db, "/cscope.out$"))
	set nocscopeverbose " suppress 'duplicate connection' error
	exe "cs add " . db . " " . path
	set cscopeverbose
    endif
endfun
au BufEnter * call LoadCscope()

""" jump to uses of function using cscope
nnoremap <leader>] :cs find c <C-R>=expand("<cword>")<CR><CR>

""" highlight useless trailing whitespace for removal
highlight ExtraWhiteSpace ctermbg=red
match ExtraWhiteSpace /\s\+$/

""" Easier access to system clipboard
nnoremap <leader>y "*y
nnoremap <leader>p "*p

""" Easy technique for clean alignment
" still need to reindent manually
vnoremap <leader>a !column -t<CR>
" also use the -s flag to manage multi word coumns

""" Home grown CtrlP alternative powered by dmenu--------------------------->

function! DmenuOpen(cmd)
  let iname = system(a:cmd . " 2>/dev/null | dmenu -i -f -l 20 -p open")
  if empty(iname)
    return
  endif
  execute "e " . iname
endfunction

" files being tracked by git
nnoremap <silent> <C-p> :call DmenuOpen("git ls-files --exclude-standard -co")<cr>
" files currently being tracked in quilt
nnoremap <silent> <C-u> :call DmenuOpen("quilt files")<cr>
" using the normal find command
nnoremap <silent> <C-f> :call DmenuOpen("find . -type f")<cr>
" git files currently tracked with modifications set
nnoremap <silent> <C-e> :call DmenuOpen("(git diff --name-only --cached; git ls-files -m)")<cr>
" old files from viminfo
nnoremap <silent> <Space>o :call DmenuOpen("grep '^>' ~/.viminfo \| cut -c3-")<cr>
" add more as needed !!!

""" ------------------------------------------------------------------------<

""" Home grown register selector powered by dmenu--------------------------->
" Not really using.
function! RegisterView(op)
  let save_pos = getpos(".")
  let tmpfile = tempname()
  execute "redir > " . tmpfile . "|silent registers|redir END"
  let rawout = system("cat " . tmpfile . "| dmenu -i -l 15 -p Reg")
  execute delete(tmpfile)
  call setpos(".", save_pos)
  if (empty(rawout)) " for exiting flow if esc is entered from dmenu
    return
  endif
  let reg = split(rawout)[0]
  call feedkeys(a:op . reg[1], 'n')
endfunction

" nnoremap <silent> " :call RegisterView("\"")<CR>
""" ------------------------------------------------------------------------<

""" For automatic search of include files etc
" can now jump to local include files in projects
set path+=**

" find the next number
nnoremap <silent> <leader>n /\d\+<CR>

""" Home grown :Gblame ----------------------------------------------------->
" displays only the author and relative date to commit in a vertical window
" scrollbinded to the main info.
function! GitBlameInfo()
	set scrollbind
	set cursorbind
	vnew | r!git blame -c --date=relative #
	%norm df(f)DdaW
	norm ggdd
	vertical res -15
	" lock the screens
	set scrollbind
	set cursorbind
	set readonly
endfunction
command! Gblame :call GitBlameInfo()<CR>
""" ------------------------------------------------------------------------<

""" Navigate loclist entries
nnoremap ]l :lnext<CR>
nnoremap [l :lprevious<CR>
nnoremap ]L :llast<CR>
nnoremap [L :lfirst<CR>

""" Navigate quickfix entries
nnoremap ]q :cnext<CR>
nnoremap [q :cprevious<CR>

" highlight word under cursor
highlight WordUnder ctermbg=237
autocmd CursorMoved * exe printf('match WordUnder /\V\<%s\>/', escape(expand('<cword>'), '/\'))

" Switching between diff with master and INDEX
let g:gitgutter_diff_target_master = 0
function! GitDiffTargetToggle()
	if g:gitgutter_diff_target_master
		let g:gitgutter_diff_base = ''
		let g:gitgutter_diff_target_master = 0
		echom "Showing diff with INDEX"
	else
		let g:gitgutter_diff_base = 'master'
		let g:gitgutter_diff_target_master = 1
		echom "Showing diff with master"
	endif
	GitGutter
endfunction
nnoremap <silent> gM :call GitDiffTargetToggle()<CR>

""" Home grown "helm-mini" --------------------------------------------->
" Switch buffer easily with fuzzy find
" Even simpler alt:  nnoremap <Space>b :ls<CR>:b<Space>
" Problem with this: am not really using long lived buffers and C-e already
" covers this use case heavily.
function! BufferSwitch()
  let save_pos = getpos(".")
  let tmpfile = tempname()
  execute "redir > " . tmpfile . "|silent ls|redir END"
  let rawout = system("cat " . tmpfile . " | dmenu -i -l 10 -p buffer")
  execute delete(tmpfile)
  call setpos(".", save_pos)
  if (empty(rawout)) " for exiting flow if esc is entered from dmenu
    return
  endif
  let buffer = split(rawout)[0]
	execute "b " . buffer
endfunction

nnoremap <silent> <C-y> :call BufferSwitch()<CR>
" ----------------------------------------------------------------------<

""" Home grown line finder using dmenu --------------------------------->
" Like the FZF :Blines command.
function! LineSelect()
  let rawout = system("cat -n " . expand('%:p') . " | dmenu -i -l 20 -p line")
  if (empty(rawout)) " for exiting flow if esc is entered from dmenu
    return
  endif
	let lineno = split(rawout)[0]
	execute lineno
endfunction

noremap <silent> <C-l> :call LineSelect()<CR>
" ----------------------------------------------------------------------<

" use ag if available
if executable("ag")
	set grepprg=ag\ --vimgrep
	set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

""" Home grown grep/search helper using dmenu  ------------------------->
" Search for word under cursor
" Or use like :grep, with fuzzy finding on top
" Can use other searchers instead of Ag
function! WordSearch(word)
  let rawout = system("ag --nogroup --nocolor " . a:word . " | dmenu -i -l 20 -p search")
  if (empty(rawout)) " for exiting flow if esc is entered from dmenu
    return
  endif
	let filename = split(rawout, ':')[0]
	let lineno = split(rawout, ':')[1]
	execute "e" filename
	execute lineno
endfunction
" search for word under cursor
noremap <silent> <space>s :call WordSearch(expand('<cword>'))<CR>
" search for WORD under cursor
noremap <silent> <space>S :call WordSearch(expand('<cWORD>'))<CR>
" search custom string: eg, :Search abc def
command! -nargs=1 Search :call WordSearch(string(<f-args>))
" ----------------------------------------------------------------------<

" visual tab characters
set list
set lcs=tab:├─
" set lcs=tab:\|\ 
" set lcs=tab:▶\ 

"" gitgutter text objects
" c conflicts with 'comment' text object
omap ih <Plug>GitGutterTextObjectInnerPending
omap ah <Plug>GitGutterTextObjectOuterPending
xmap ih <Plug>GitGutterTextObjectInnerVisual
xmap ah <Plug>GitGutterTextObjectOuterVisual

""" Tmux complete
" reset user completefunc, use only with neocomplete
let g:tmuxcomplete#trigger = ''

""" Recently entered words completions
" Complete from list of newly inserted words
" Meant to be used without any prefix; if some prefix is known/entered, normal
" autocomplete, currently neocomplete, will kick in
function! RecentAdditions(findstart, base)
	" if first invocation - use nothing already entered
	if a:findstart == 1
		return col('.')
	endif

	" list of words in addition sections in the git diff
	let wordlist = split(system('git diff | grep "^+[^+]\{2\}" | cut -c2- | paste -sd " " - | sed "s/[^a-zA-Z0-9_]/\ /g"'))
	"" can do additional processing here, like
	" 1. filtering out small words
	" 2. removing words that already exist in buffer
	return wordlist
endfunction
set completefunc=RecentAdditions
" has to be triggered using C-X C-U
