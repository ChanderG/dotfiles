syntax on

"line numbering that toggles between normal and relative
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber

"original line number
set number

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

"dark background color
set background=light

"shift >>/<< width
set shiftwidth=2

set tabstop=8

"Use Enter/Shift-Enter to introduce new lines above/below w/o leaving normal mode
map <Enter> o<ESC>

call plug#begin('~/.vim/bundle')

"""CtrlP plugin
Plug 'ctrlpvim/ctrlp.vim'

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

""" Avoid repeated movements
Plug 'takac/vim-hardtime'

""" Better word motion in terms of camel and snake case
Plug 'chaoren/vim-wordmotion'

""" In an attempt to make use of the full set of registers
Plug 'junegunn/vim-peekaboo'
call plug#end()

"Ctrlp fuzzy finder : thanks to Robin Ward
"for quick indexing by using git file listing => no files from gitignore, but untracked files
"let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files --exclude-standard -co']
" use ag instead
let g:loaded_ctrlp = 1
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" to search in mru files as well
let g:ctrlp_cmd = 'CtrlPMixed'
""" search through tags
let g:ctrlp_extensions = ['tag', 'mixed']
nmap <leader>p ;CtrlPTag<CR>

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
map <C-PageUp> ;tabprevious<CR>
map <C-PageDown> ;tabnext<CR>

""" new tab creation
map tn <Esc>;tabnew<CR>


""" QuickScope plugin

""" toggle with
nmap <leader>q <plug>(QuickScopeToggle)
vmap <leader>q <plug>(QuickScopeToggle)

""" tagbar plugin

""" easy toggle
nmap <F8> ;TagbarToggle<CR>

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
let g:neocomplete#enable_auto_select = 0
" cancel the current neocomplete suggestion and open omnicomplete instead
inoremap <tab> <c-e><c-x><c-o>

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
let g:jedi#show_call_signatures = "1"
let g:jedi#popup_select_first = 0

""" Disable use of hjkl without number prefix =======================>
" courtesy of https://gist.github.com/jeetsukumaran/96474ebbd00b874f0865
" NOT included to test hardtime

""" vim-hardtime: avoid repeated movement keys
let g:hardtime_default_on = 1
let g:list_of_normal_keys = ["<UP>", "<DOWN>"]
let g:hardtime_timeout = 2000
let g:hardtime_maxcount = 2

" automatically leave insert mode after 'updatetime' milliseconds of inaction
" set updatetime=5000
" au CursorHoldI * stopinsert
" Need a suitable value

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

""" Home grown CtrlP alternative powered by dmenu
function! DmenuOpenAll()
  " obtain files of interest in order
  " 1. quilt operating files
  " 2. Recent files opened in this sub folder
  " 3. From git ls.
  " 4. Find in subdir -> if not a git repo.
  let iname = system("(quilt files 2>/dev/null; git ls-files 2>/dev/null) | dmenu -i -l 20 -p open")
  if empty(iname)
    return
  endif
  execute "e " . iname
endfunction

noremap <silent> <C-p> :call DmenuOpenAll()<cr>
