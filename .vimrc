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
" set t_Co=256
""" Use truecolours
set termguicolors
"dark background color
set background=dark

"shift >>/<< width
set shiftwidth=0

set tabstop=2

"Use Enter/Shift-Enter to introduce new lines above/below w/o leaving normal mode
nnoremap <Enter> o<ESC>

call plug#begin('~/.vim/bundle')

""" Language specific syntax,completion etc
Plug 'derekwyatt/vim-scala'
Plug 'wlangstroth/vim-racket'
Plug 'jakwings/vim-pony'
" Requires jedi(pip)
Plug 'davidhalter/jedi-vim'
" syntax based code folding for python
Plug 'tmhedberg/SimpylFold'
" C/C++ completion engine based on clang
Plug 'Rip-Rip/clang_complete'
" Go all-in-all
Plug 'fatih/vim-go'

""" Language independent syntax stuff
Plug 'sheerun/vim-polyglot'
Plug 'justinmk/vim-syntax-extra'

"""License adder
Plug 'antoyo/vim-licenses'

"""find ideal positions to jump to
Plug 'bradford-smith94/quick-scope'

"""comment toggle
Plug 'tpope/vim-commentary'

""" Git related
Plug 'airblade/vim-gitgutter'
Plug 'iberianpig/tig-explorer.vim'

"""slime for vim
Plug 'jpalardy/vim-slime'

"""zeavim - access zeal docs from vim
Plug 'kabbamine/zeavim.vim'

""" Online docs
Plug 'keith/investigate.vim'

""" Syntax checking
Plug 'w0rp/ale'

""" Auto formatting
Plug 'chiel92/vim-autoformat'

""" Async runner
Plug 'skywind3000/asyncrun.vim'

""" 256 Color theme
Plug 'morhetz/gruvbox'

""" Custom text objects
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'kana/vim-textobj-user'
Plug 'glts/vim-textobj-comment'
Plug 'kana/vim-textobj-function'
"" Operators
Plug 'machakann/vim-sandwich'
Plug 'vim-scripts/ReplaceWithRegister'

" camel-Snake case helper
Plug 'chaoren/vim-wordmotion'

""" quickfix helpers
Plug 'romainl/vim-qf'

""" minimal autocomplete
" Plug 'lifepillar/vim-mucomplete'

""" LSP
Plug 'natebosch/vim-lsc'

""" Autoclose chars
Plug 'vim-scripts/AutoClose'

""" Snippets
Plug 'KeyboardFire/vim-minisnip'
call plug#end()

" set color scheme
colorscheme gruvbox

"""For the License Adder
let g:licenses_authors_name = 'Govindarajan, Chander <chandergovind@gmail.com>'
let g:licenses_copyright_holders_name = 'Govindarajan, Chander <chandergovind@gmail.com>'

""" set terminal mode
"" DON'T TOUCH THIS VARIABLE - NO OTHER VALUE WORKS
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

""" Better 'complete'
" i - too slow when ** is in path
" t - usually too many tags;
" in any case can manually trigger one of the above
set complete=.,w,b,u
set completeopt=longest,menu,menuone,preview

"" ale linters
" Require: pylint (apt-get)
" Require: gcc , cppcheck (apt-get), splint(apt-get), clang(apt-get)
let g:ale_linters = {
\  'python': ['pylint', 'pep8'],
\  'c': ['gcc','cppcheck','splint','clang_check','clang_tidy'],
\  'cpp': ['gcc','cppcheck','clang_check','clang_tidy'],
\}

""" Auto formatting
" format on save
au BufWrite *.py :Autoformat
au BufWrite *.c :Autoformat
au BufWrite *.cpp :Autoformat
" python: formatting requries python-autopep8 (apt-get)
" c/c++ : clangformat, comes with clang (apt-get)

""" clang_complete
" Require: clang (apt-get), exact path needs to be put here
let g:clang_library_path='/usr/lib64/llvm/libclang.so'
let g:clang_complete_loaded="0"

""" jedi-vim
let g:jedi#show_call_signatures = "2"
let g:jedi#popup_select_first = 1

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
  let iname = system(a:cmd . " 2>/dev/null | dmenu -i -f -l 10 -p open")
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
" or: nnoremap <Space>b :b<Space><Tab>
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
  let rawout = system("cat -n " . expand('%:p') . " | dmenu -i -l 10 -p line")
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

""" Recently entered words completions
" Complete from list of newly inserted words
" Meant to be used without any prefix; if some prefix is known/entered, normal
" autocomplete, currently neocomplete, will kick in
function! RecentAdditions(findstart, base)
	" if first invocation - grab full keyword before cursor
	if a:findstart == 1
		return match(strpart(getline('.'), 0, col('.')), '\k*$')
	endif

	" list of words in addition sections in the git diff
	let wordlist = split(system('git diff | grep "^+[^+]\{2\}" | cut -c2- | paste -sd " " - | sed "s/[^a-zA-Z0-9_]/\ /g"'))
	"" can do additional processing here, like
	" 1. filtering out small words
	" 2. removing words that already exist in buffer
	" right now: keeps words that contain stuff already entered
	call filter(wordlist, 'v:val =~ a:base')
	return wordlist
endfunction
set completefunc=RecentAdditions
" has to be triggered using C-X C-U

" cleaner form
highlight link Function GruvboxBlue

" comments in italic
highlight Comment cterm=italic

""" Use gtags for everything
" Source gtag functions
source /usr/share/gtags/gtags.vim
" Use gtags instead of cscope (all cscope functions should work out of the
" box)
source /usr/share/gtags/gtags-cscope.vim
" activate cscope db if possible; cscope functions will work after this
silent GtagsCscope
" Use cscope (now actually gtag) for tags instead of (c|e)tags
set cscopetag

""" Home grown one-shot multi substring find.
"" Doing it this way is a bit expensive/slow
" Convert wordstring to regex suitable for substring search
function! SSS(words)
	return '.*'.substitute(escape(a:words, '.'), '\s\+', '\\\&.*', &gdefault ? 'gg' : 'g')
endfunction

"" call tag function
function! TagSSS(words)
	execute "tag /".SSS(a:words)
endfunction
command! -nargs=* Tag :call TagSSS(<q-args>)

"" list matches in buffer
function! ListSSS(words)
	execute "g/" . SSS(a:words) . "\\c"
endfunction
command! -nargs=* LS :call ListSSS(<q-args>)

"" file opening helper
" takes cmd to use to create a list to search for files in
function! FileOpenSSS(cmd, words)
	let rawout = split(system(a:cmd), "\n")
	execute "e" matchstr(rawout, "\c" . SSS(a:words))
endfunction
command! -nargs=* G :call FileOpenSSS("git ls-files --exclude-standard -co", <q-args>)
command! -nargs=* F :call FileOpenSSS("find . -type f", <q-args>)

"" Mappings for tig-explorer
nnoremap <Space>g :Tig<CR>
nnoremap <Space>b :TigBlame<CR>

""" LSP
let g:lsc_auto_map = v:true " Use defaults
let g:lsc_server_commands = {
 \ 'python': 'pyls',
 \ 'java': '/home/chanderg/Documents/Dabblings/lsp/jdt_language_server',
 \ }

""" Shell/Terminal Workflow
" single escape used by arrow keys internally etc
tnoremap <Esc><Esc> <C-\><C-n>

" Excellent idea for one off shell interactions
" Found here: https://stackoverflow.com/a/7185348
" Works because C-d on bash side brings one back to vim
nnoremap <C-d> :sh<CR>

""" Redirect to buffer
"" Based on https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7
function! Redir(cmd)
	if a:cmd =~ '^!'
		execute "let output = system('" . substitute(a:cmd, '^!', '', '') . "')"
	else
		redir => output
		execute a:cmd
		redir END
	endif
	vnew
	let w:scratch = 1
	setlocal buftype=nofile noswapfile
	call setline(1, split(output, "\n"))
	" Prompt for name of scratch buffer
	call inputsave()
  let name = input('Buffer name (empty for default): ')
	redraw
	if name != ""
		execute "file " . name
	endif
	call inputrestore()
endfunction

command! -nargs=1 -complete=command Redir call Redir(<f-args>)

""" Replace operator
map r <Plug>ReplaceWithRegisterOperator

" vim-go settings
" disable def mapping; use global with gogtags instead
let g:go_def_mapping_enabled = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1

""" vim-qf mappings
nmap <space>q <Plug>(qf_qf_toggle)
nmap <space>l <Plug>(qf_loc_toggle)
nmap [q <Plug>(qf_qf_previous)
nmap ]q <Plug>(qf_qf_next)
nmap [l <Plug>(qf_loc_previous)
nmap ]l <Plug>(qf_loc_next)
" restore CR for quickfix
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
autocmd BufReadPost quickfix nnoremap <buffer> D :Reject<CR>
