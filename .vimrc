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
Plugin 'gmarik/Vundle.vim'

"""CtrlP plugin
Plugin 'kien/ctrlp.vim'

"""scala highlighting
Plugin 'derekwyatt/vim-scala'

"""License adder
Plugin 'antoyo/vim-licenses'

"""racket highlighting
Plugin 'wlangstroth/vim-racket'

"""find ideal positions to jump to
Plugin 'unblevable/quick-scope'

"""select jump destination easily
Plugin 'easymotion/vim-easymotion'

"""list of tags
Plugin 'majutsushi/tagbar'

"""comment toggle
Plugin 'tpope/vim-commentary'

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

""" EasyMotion plugin

""" one stop jump within line
""" note that this does not allow prefixing action
nmap <leader>s <plug>(easymotion-sl)

""" tagbar plugin

""" easy toggle
nmap <F8> ;TagbarToggle<CR>

""" Timeouts for commands
set notimeout
set ttimeout

""" Show partial commands
set showcmd

""" Allow project specific vimrc
set exrc
set secure

""" QuickFix window options

""" direct move to next
nmap cn ;cn<CR>
" no mapping for prev -- as I rarely need it

" In the quickfix window, <CR> is used to jump to the error under the
" cursor, so undefine the mapping there.
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
" required to counter the effect of our mapping of <CR> in clist window

""" A better escape
inoremap <silent> <Up> <ESC><Up>
inoremap <silent> <Down> <ESC><Down>
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
