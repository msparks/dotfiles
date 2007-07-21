"scriptencoding utf-8             " Use UTF-8

"set encoding=utf-8               " Default encoding should always be UTF-8
set nocompatible                 " Don't emulate vi's limitations
set tabstop=2                    " Use 2 spaces for tabs
set shiftwidth=2                 " Use 2 spaces for next line indent
set smarttab                     " Tab next line based on current line
set expandtab                    " Use spaces for tabs
set autoindent                   " Automatically indent next line if indented
set smartindent                  " Indent next line based on current line
set linebreak                    " Display long lines wrapped at word boundary
set incsearch                    " Enable incremental search
set ignorecase                   " Ignore case when searching
set infercase                    " Try to figure out the case when searching
set hlsearch                     " Highlight the search results
set ve=block                     " Visual Edit Blocking
set lazyredraw                   " Enable Lazy Redraw (faster macro execution)
set showmode                     " Show the current mode
set showmatch                    " Show matching brackets
set mat=5                        " Blink for 5/10 of a second
set wildmenu                     " Enable wildmenu
set wildmode=longest,full        " Match the longest and full matches
set wildignore=*.o,*~            " Always ignore these files
set scrolloff=3                  " Show 3 lines of context on vertical scroll
set sidescrolloff=2              " Show 2 columns of context on horizontal scroll
set whichwrap+=<,>,h,l           " Backspace and cursor keys wrap
set backspace=2                  " Normal backspace behavior
set number                       " Display line numbers
set textwidth=80                 " Break lines at 80 characters
set showfulltag                  " Show full tags when doing completion
set history=1000                 " lines of history
set isk+=_,$,@,%,#,-             " ignore these in words (word dividers)
set hidden                       " Allow flipping of buffers without saving
set magic                        " Regular expression characters have special meaning
set mouse=a                      " Use mouse everywhere
set noerrorbells                 " No noise

if (v:version >= 700)
  set nospell                    " Disable spell checking by default
  set spelllang=en_us            " Spelling check language
endif

set background=dark

if has("folding")
  set nofoldenable              " Require user to fold
  set fillchars=fold:-          " Fold characters should be -

  " If window is greater than 90 characters, set a column for folds
  if has("eval")
    fun! <SID>WideFold()
      if winwidth(0) > 90
        setlocal foldcolumn=1
      else
        setlocal foldcolumn=0
      endif
    endfun

    if has("autocmd")
      autocmd BufEnter * :call <SID>WideFold()
    endif
  endif
endif


if has("syntax")
  syntax on                      " Enable syntax highlighting
endif

if has("eval")
  filetype on                    " Detect filetype by extension
  filetype indent on             " Enable indents based on extensions
  filetype plugin on             " Load filetype plugins
endif

" Turn off search hilite when idle
if has("autocmd")
   autocmd CursorHold * nohls | redraw
endif

" Always refresh syntax from the start
if has("autocmd")
   autocmd BufEnter * syntax sync fromstart
endif

" subversion commit messages need not be backed up
if has("autocmd")
   autocmd BufRead svn-commit.tmp :setlocal nobackup
endif

" Disable case insensitivity for source files
if has("autocmd")
   autocmd BufEnter *
      \  if &filetype == 'c' ||
      \     &filetype == 'cs' ||
      \     &filetype == 'cpp' ||
      \     &filetype == 'java' |
      \        set noignorecase noinfercase |
      \  endif
endif

" Show trailing whitespace visually
" Shamelessly stolen from Ciaran McCreesh <ciaranm@gentoo.org>
if (&termencoding == "utf-8") || has("gui_running")
  if v:version >= 700
    set list listchars=tab:»·,trail:·,extends:?,nbsp:?
  else
    set list listchars=tab:»·,trail:·,extends:?
  endif
else
  if v:version >= 700
    set list listchars=tab:>-,trail:.,extends:>,nbsp:_
  else
    set list listchars=tab:>-,trail:.,extends:>
  endif
endif

" Get rid of the annoying UI
if has("gui")
  set guioptions-=t    " Disable Menu tear-offs
  set guioptions-=T    " Disable the tool-bar
  set guioptions-=m    " Disable the menu
  set guioptions-=l
  set guioptions-=L
  set guioptions-=r    " Disable the scrollbar
  set guioptions-=R
endif

" Link the imports in JAVA sources
if has("autocmd")
  autocmd BufRead *.java set include=^#\s*import
  autocmd BufRead *.java set includeexpr=substitute(v:fname,'\\.','/','g')
endif

" Map F3 to nohls
nmap <silent> <F3> :silent nohls<CR>
imap <silent> <F3> <ESC>:silent nohls<CR>i

" Map F12 to numbering
nmap <silent> <F12> :silent set number!<CR>
imap <silent> <F12> <ESC>:silent set number!<CR>i

" Spell check can be annoying at times
nmap <silent> <F4> :silent set spell!<CR>
imap <silent> <F4> <ESC>:silent set spell!<CR>i

" tab indents
vmap <silent> <Tab> >gv

" shift-tab un-indents
vmap <silent> <S-Tab> <gv

" Backup
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/temp

set title                        " Set a title on the terminal
set laststatus=2                 " Always display the status line
set shortmess=atI                " Enable short messages (press a key is annoying)
set statusline=%F%m%r%h%w\ [%{&ff}]\ %y\ [ascii=\%03.3b]\ [hex=\%02.2B]\ [pos=%04l,%04v][%p%%]\ [len=%L]
set laststatus=2                 " Always show the status line

let perl_extended_vars=1 " highlight advanced perl vars inside strings

set insertmode

" alt-backspace stuff
map! <m-del> <m-f><c-w>
map! ÿ <c-w>
map! <m-bs> <c-w>

" Buffer switching
nmap <c-p> :bprevious<CR>
vmap <c-p> :bprevious<CR>
imap <c-p> <ESC>:bprevious<CR>i
nmap <c-n> :bnext<CR>
vmap <c-n> :bnext<CR>
imap <c-n> <ESC>:bnext<CR>i

