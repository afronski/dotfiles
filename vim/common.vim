" Backup directory
set backupdir=~/.vim/backupfiles,/var/tmp,/tmp,.

" Swapfile directory
set directory=~/.vim/swapfiles,/var/tmp,/tmp,.

language messages en              	" Sets the language of the messages / ui (vim).
set encoding=utf-8 nobomb           " Use UTF-8 everywhere.

" Pathogen settings.
filetype off
call pathogen#runtime_append_all_bundles()

syntax enable                     	" Turn on syntax highlighting.
filetype plugin indent on         	" Turn on file type detection.

colorscheme molokai

set nocompatible			        " Behave like good program in Windows environment.
behave mswin

set nobackup				        " Do not save backup files.
set nowritebackup

set showcmd                       	" Display incomplete commands.
set showmode                      	" Display the mode you're in.
set cursorline

set magic 				            " Extended regexes.

set backspace=indent,eol,start    	" Intuitive backspacing.

set hidden                        	" Handle multiple buffers better.

set wildmenu                      	" Enhanced command line completion.
set wildmode=list:longest         	" Complete files like a shell.

set ignorecase                    	" Case-insensitive searching.
set smartcase                     	" But case-sensitive if expression contains a capital letter.

set number                        	" Show line numbers.
set ruler                         	" Show cursor position.
set ttyfast

set scrolloff=3
set shortmess=I

set incsearch                     	" Highlight matches as you type.
set hlsearch                      	" Highlight matches.

set wrap                          	" Turn on line wrapping.
set scrolloff=3                   	" Show 3 lines of context around the cursor.

set textwidth=79
set formatoptions=qrn1

set title                         	" Set the terminal's title

set visualbell                    	" No beeping.
			
set tabstop=4                   	" Global tab width.
set shiftwidth=4                 	" And again, related.
set softtabstop=4
set expandtab                    	" Use spaces instead of tabs

set whichwrap+=<,>,[,]

set laststatus=2                  	" Show the status line all the time

set mouse=a                         " Enable mouse.

" Useful status information at bottom of screen
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y\ %{fugitive#statusline()}%{exists('*CapsLockStatusline')?CapsLockStatusline():''}%=%-16(\ %l,%c-%v\ %)%P

set cmdheight=3				        " Avoid Hit Enter to continue prompts.

" Switch off arrow keys - learn hjkl keys.
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

nnoremap j gj
nnoremap k gk

" Rope settings.
inoremap <leader>j <ESC>:RopeGotoDefinition<cr>

" Get Rid of stupid Goddamned help keys.
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Changing leader key.
let mapleader = ","

" Map : to ; also in command mode.
nnoremap ; :

" Adding More Shorcuts keys using leader kye.
" Leader Kye provide separate namespace for specific commands.
" ,W Command to remove white space from a file.
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>


" ,ft Fold tag, helpful for HTML editing.
nnoremap <leader>ft vatzf

" ,q Re-hardwrap Paragraph
nnoremap <leader>q gqip

" ,v Select just pasted text.
nnoremap <leader>v V`]

" ,ev Shortcut to edit .vimrc file on the fly on a vertical window.
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

set listchars=tab:▸\ ,eol:¬         " Invisibles using the Textmate style.

" Wildmenu completion.
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.git,.svn                     " Version control directories.
set wildignore+=*.aux,*.out,*.toc                 " Latex indermediate files.
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg    " Binary images.
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest  " Compiled object files.
set wildignore+=*.spl                             " Compiled speolling world list.
set wildignore+=*.sw?                             " Vim swap files.
set wildignore+=*.luac                            " Lua byte code.
set wildignore+=migrations                        " Django migrations.
set wildignore+=*.pyc                             " Python Object codes.
set wildignore+=*.orig                            " Merge resolution files.

" Tab mappings.
map <leader>tt :tabnew<cr>
map <leader>te :tabedit
map <leader>tc :tabclose<cr>
map <leader>to :tabonly<cr>
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>
map <leader>tf :tabfirst<cr>
map <leader>tl :tablast<cr>
map <leader>tm :tabmove
map <leader>d :NERDTreeToggle<cr>

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

autocmd BufNewFile,BufRead *_spec.rb compiler rspec
au BufRead,BufNewFile *.jst set filetype=jst

" Mapping to NERDTree.
nnoremap <C-n> :NERDTreeToggle<cr>

" Mini Buffer some settigns.
let g:bufExplorerShowRelativePath=1
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" Rope Plugin settings.
imap <leader>j <ESC>:RopeGotoDefinition<cr>
nmap <leader>j <ESC>:RopeGotoDefinition<cr>

" Tagbar key bindings.
nmap <leader>l <ESC>:TagbarToggle<cr>
imap <leader>l <ESC>:TagbarToggle<cr>
