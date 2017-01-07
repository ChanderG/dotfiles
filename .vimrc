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

"""setup vundle
set nocompatible
filetype off

set rtp^=~/.vim/bundle/Vundle.vim
call vundle#begin()

"""manage vundle
Plugin 'VundleVim/Vundle.vim'

"""CtrlP plugin
Plugin 'ctrlpvim/ctrlp.vim'

"""scala highlighting
Plugin 'derekwyatt/vim-scala'

"""License adder
Plugin 'antoyo/vim-licenses'

"""racket highlighting
Plugin 'wlangstroth/vim-racket'

"""find ideal positions to jump to
Plugin 'unblevable/quick-scope'

"""list of tags
Plugin 'majutsushi/tagbar'

"""comment toggle
Plugin 'tpope/vim-commentary'

"""for auto git diffs
Plugin 'airblade/vim-gitgutter'

"""syntax based code folding for python
Plugin 'tmhedberg/SimpylFold'

"""slime for vim
Plugin 'jpalardy/vim-slime'

"""zeavim - access zeal docs from vim
Plugin 'kabbamine/zeavim.vim'

""" Online docs
Plugin 'keith/investigate.vim'

""" Neocomplete - general purpose auto complete frontend
Plugin 'Shougo/neocomplete.vim'

""" Python autocomplete engine
" Requires jedi(pip)
Plugin 'davidhalter/jedi-vim'

""" Syntax checking
Plugin 'scrooloose/syntastic'

""" Auto formatting
Plugin 'chiel92/vim-autoformat'

""" Snippets
" engine
Plugin 'SirVer/ultisnips'
" snippets
Plugin 'honza/vim-snippets'

""" extra objects
Plugin 'michaeljsmith/vim-indent-object'

""" C/C++ completion engine based on clang
Plugin 'Rip-Rip/clang_complete'

""" More text objects
Bundle 'wellle/targets.vim'

""" Sneak - medium distance motion
Plugin 'justinmk/vim-sneak'

""" Avoid repeated movements
Plugin 'takac/vim-hardtime'

call vundle#end()
filetype plugin indent on

"Ctrlp fuzzy finder : thanks to Robin Ward
"for quick indexing by using git file listing => no files from gitignore, but untracked files
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files --exclude-standard -co']

""" search through tags
let g:ctrlp_extensions = ['tag']
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
nmap cn ;cn<CR>
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
set foldlevel=0

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

""" Syntastic configuration
let g:syntastic_always_populate_loc_list = 1

"" python checkers
" Require: pylint (apt-get)
let g:syntastic_python_checkers = ['pylint']

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
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

""" clang_complete
" Require: clang (apt-get), exact path needs to be put here
let g:clang_library_path='/usr/lib/llvm-3.6/lib/libclang-3.6.so.1'

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

""" Use TAB like in Orgmode
" Toggles between all open and all closed fully
" TODO
