" ~/.vimrc
" $Id$
"
" Like many .vimrc files, a lot of what is below was copied from other .vimrcs
" and modified to meet my specifications. Significant features were taken from
" Andre Pang's Vimacs plugin for vim:
"   http://www.vim.org/scripts/script.php?script_id=300
" Thus, some emacs-native keybindings are present in this .vimrc.

"scriptencoding utf-8             " Use UTF-8

"set encoding=utf-8               " Default encoding should always be UTF-8
set nocompatible                 " Don't emulate vi's limitations
set tabstop=2                    " Use 2 spaces for tabs
set shiftwidth=2                 " Use 2 spaces for next line indent
set smarttab                     " Tab next line based on current line
set expandtab                    " Use spaces for tabs
set cindent                      " C indenting
set cinoptions=t0,(0,)60,u0,*100 " All sorts of options
set linebreak                    " Display long lines wrapped at word boundary
set incsearch                    " Enable incremental search
set ignorecase                   " Ignore case when searching
set infercase                    " Try to figure out the case when searching
set hlsearch                     " Highlight the search results
set ve=block                     " Visual Edit Blocking
set lazyredraw                   " Enable Lazy Redraw (faster macro execution)
set showmode                     " Show the current mode
set showmatch                    " Show matching brackets
set mat=2                        " Blink (for mat*.1s) during bracket match
set wildmenu                     " Enable wildmenu
set wildmode=longest,full        " Match the longest and full matches
set wildignore=*.o,*~            " Always ignore these files
set scrolloff=3                  " Lines of context on vertical scroll
set sidescrolloff=2              " Columns of context on horizontal scroll
set whichwrap+=<,>,h,l           " Backspace and cursor keys wrap
set backspace=2                  " Normal backspace behavior
set textwidth=80                 " Break lines at 80 characters
set showfulltag                  " Show full tags when doing completion
set history=1000                 " lines of history
set isk+=_,$,@,%,#,-             " ignore these in words (word dividers)
set hidden                       " Allow flipping of buffers without saving
set magic                        " Regular expression characters have special meaning
set noerrorbells                 " No noise
set cmdheight=2                  " Show two lines for command history
set showcmd                      " Show partial command / info about visual selection
set background=dark              " I use a black background

" Backup locations
set backup
set backupdir=~/.vim/backup
set directory=~/.vim/temp

set title                        " Set a title on the terminal
set laststatus=2                 " Always display the status line
set shortmess=atI                " Enable short messages (press a key is annoying)

" Customize the statusline
highlight User1 ctermfg=White ctermbg=Blue cterm=reverse
highlight User2 ctermfg=White ctermbg=Magenta cterm=bold,reverse
set statusline=%2*%2n%*:%1*%F%*%m%r%h%w\ [%{&ff}]\ %y\ [asc:%03b\ hex:%02B]\ [l:%03l\ c:%03v]\ \ [%p%%/%L]
set laststatus=2                 " Always show the status line

" Line numbers
set number                       " Display line numbers
highlight LineNr ctermfg=DarkGray

let perl_extended_vars=1         " highlight advanced perl vars inside strings

" screen has broken this before...
if &term != "screen"
  set mouse=a                    " Use mouse everywhere
endif

if (v:version >= 700)
  set nospell                    " Disable spell checking by default
  set spelllang=en_us            " Spelling check language
endif

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

" Reset indendation for the buffer when called; 'filetype indent on' seems to
" break the standard C indentation (line continuation in particular) for Perl
" and PHP.
fun! <SID>ResetIndent()
  set indentexpr=
  set cindent
endfun

fun! <SID>Perl()
  set cinkeys=0{,0},!^F,o,O,e    " Perl-friendly reindent keys
  set cinwords=if,else,while,do,for,elsif,sub
  call <SID>ResetIndent()
endfun

autocmd FileType perl :call <SID>Perl()
autocmd FileType php  :call <SID>ResetIndent()

" Highlight text beyond the set text width
highlight LongLines ctermfg=Red
au BufEnter *.py,*.c,*.cpp,*.java,*.h,*.pl,*.php exec 'match LongLines /\%>'.&textwidth.'v.\+/'

" Turn off search hilite when idle
if has("autocmd")
   autocmd CursorHold * nohls | redraw
endif

" Always refresh syntax from the start
if has("autocmd")
   autocmd BufEnter * syntax sync fromstart
endif

" Subversion commit messages need not be backed up
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
    \       set noignorecase noinfercase |
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
highlight SpecialKey ctermbg=Yellow guibg=Yellow ctermfg=DarkGray

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

" Map F3 to nohls (turn off highlighting -- from a search, for instance)
nmap <silent> <F3> :silent nohls<CR>
imap <silent> <F3> <ESC>:silent nohls<CR>i<Right>

" Map F12 to line numbering
nmap <silent> <F12> :silent set number!<CR>
imap <silent> <F12> <ESC>:silent set number!<CR>i<Right>

" Spell check can be annoying at times
nmap <silent> <F4> :silent set spell!<CR>
imap <silent> <F4> <ESC>:silent set spell!<CR>i<Right>

" Tab indents the selected block of code by &shiftwidth, in visual mode.
vmap <silent> <Tab> >gv

" Shift-Tab un-indents does the opposite in visual mode
vmap <silent> <S-Tab> <gv

" <C-c><C-q> reindents current block
imap <C-c><C-q> <C-o>=a{
nmap <C-c><C-q> =a{

" Go into insert mode immediately
startinsert

if has("eval")
  " fix meta-keys which generate <esc>a .. <esc>z
  let c='a'
  while c != 'z'
    exec "map <esc>".c." <M-".c.">"
    exec "map! <esc>".c." <M-".c.">"
    exec "map <esc>".toupper(c)." <M-".toupper(c).">"
    exec "map! <esc>".toupper(c)." <M-".toupper(c).">"
    let c = nr2char(1+char2nr(c))
  endw

  " same for numbers
  let n = 0
  while n != 9
    exec "map <esc>".n." <M-".n.">"
    exec "map! <esc>".n." <M-".n.">"
    exec "map <esc>".n." <M-".n.">"
    exec "map! <esc>".n." <M-".n.">"
    let n = n+1
  endw

  " Fix arrow keys
  exec "set <C-Left>=\eOd"
  exec "set <C-Right>=\eOc"
  imap <esc>Oa <C-Up>
  imap <esc>Ob <C-Down>
  map <esc>OA <Up>
  map! <esc>OB <Down>

  map <esc><Space> <M-Space>
  map! <esc><Space> <M-Space>
endif

" Make Alt-Backspace delete the last word
map! <m-del> <m-f><c-w>
map! <esc><del> <m-f><c-w>
map! ÿ <c-w>
map! <m-bs> <c-w>
map! <esc><bs> <c-w>

" Buffer switching with <C-n> and <C-p>
nmap <C-p> :bprevious<CR>
vmap <C-p> :bprevious<CR>
imap <C-p> <esc>:bprevious<CR>i
nmap <C-n> :bnext<CR>
vmap <C-n> :bnext<CR>
imap <C-n> <esc>:bnext<CR>i

" Switch buffers with meta-#
if has("eval")
  let n = 1
  while n != 9
    exec "imap <M-".n."> <C-o>:b ".n."<CR>"
    exec "nmap <M-".n."> <C-o>:b ".n."<CR>"
    exec "vmap <M-".n."> <C-o>:b ".n."<CR>"
    let n = n + 1
  endw
endif

" Map ctrl-delete to delete buffer
imap <esc>[3^ <C-o>:bdelete

" emacs/shell keybindings
imap <M-x> <C-o>:
nmap <M-x> :
imap <C-x><C-s> <C-o>:update<CR>
nmap <C-x><C-s> :update<CR>
imap <C-x><C-f> <C-o>:hide edit<Space>
nmap <C-x><C-f> :hide edit<Space>
imap <C-x>s <C-o>:wall<CR>
imap <C-x>i <C-o>:read<Space>
imap <C-x><C-w> <C-o>:write<Space>
imap <C-x><C-q> <C-o>:set invreadonly<CR>
imap <C-x><C-r> <C-o>:hide view<Space>
inoremap <C-c> <nop>
inoremap <C-x><C-c> <C-o>:confirm qall<CR>
nmap <C-x><C-c> :confirm qall<CR>
map <C-z> :suspend!<CR>
imap <C-z> <C-o>:suspend!<CR>
imap <C-_> <C-o>u
inoremap <C-l> <C-o>zz<C-o><C-l>
nnoremap <C-l zz<C-l>

" Navigation
map <M-f> <S-Right>
map! <M-f> <S-Right>
map <M-b> <S-Left>
map! <M-b> <S-Left>
map <C-a> <Home>
map! <C-e> <End>
map <C-f> <Right>
map! <C-f> <Right>
map <C-b> <Left>
map! <C-b> <Left>
map <C-d> <Del>
map! <C-d> <Del>

" Window Operations (Emacs style)
inoremap <C-x>2 <C-o><C-w>s
nnoremap <C-x>2 <C-w>s
inoremap <C-x>3 <C-o><C-w>v
nnoremap <C-x>3 <C-w>v
inoremap <C-x>0 <C-o><C-w>c
nnoremap <C-x>0 <C-w>c
inoremap <C-x>1 <C-o><C-w>o
nnoremap <C-x>1 <C-w>o
inoremap <C-x>o <C-o><C-w>w
nnoremap <C-x>o <C-w>w
inoremap <C-x>O <C-o><C-w>W
nnoremap <C-x>O <C-w>W
inoremap <C-Tab> <C-o><C-w>w
nnoremap <C-Tab> <C-w>w
inoremap <C-S-Tab> <C-o><C-w>W
nnoremap <C-S-Tab> <C-w>w
inoremap <C-x>+ <C-o><C-w>=
nnoremap <C-x>+ <C-w>=

" Aborting
map <C-g> <C-c>
map! <C-g> <C-c>

" Killing
inoremap <silent> <M-d> <C-r>=<SID>KillWord()<CR>
inoremap <silent> <C-k> <C-r>=<SID>KillLine()<CR>
vnoremap <C-w> "1d

" Pasting
inoremap <silent> <C-y> <C-o>:call <SID>ResetKillRing()<CR><C-r><C-o>"
inoremap <S-Ins> <C-r><C-o>*

set sel=exclusive
" Visual mode
inoremap <silent> <C-Space> <C-r>=<SID>StartVisualMode()<CR>
" Unix terminals produce <C-@>, not <C-Space>
imap <C-@> <C-Space>
vnoremap <C-x><C-Space> <Esc>

" Marking blocks
inoremap <M-Space> <C-o>:call <SID>StartMarkSel()<CR><C-o>viw
inoremap <M-h> <C-o>:call <SID>StartMarkSel()<CR><C-o>vap
inoremap <C-<> <C-o>:call <SID>StartMarkSel()<CR><C-o>v1G0o
inoremap <C->> <C-o>:call <SID>StartMarkSel()<CR><C-o>vG$o
inoremap <C-x>h <C-o>:call <SID>StartMarkSel()<CR><Esc>1G0vGo

" Fill/reformat paragraph
inoremap <silent> <M-q> <C-o>:call <SID>FillParagraph()<CR>

" Case Change
inoremap <M-l> <C-o>gul<C-o>w
inoremap <M-u> <C-o>gUe<C-o>w
inoremap <M-c> <C-o>gUl<C-o>w

function! <SID>StartMarkSel()
  if &selectmode =~ 'key'
    set keymodel-=stopsel
  endif
endfunction

function! <SID>ResetKillRing()
  let s:kill_ring_position = 3
endfunction

function! <SID>StartVisualMode()
  call <SID>StartMarkSel()
  if col('.') > strlen(getline('.'))
    " At EOL
    return "\<Right>\<C-o>v\<Left>"
  else
    return "\<C-o>v"
  endif
endfunction

command! FillParagraph :call <SID>FillParagraph()

function! <SID>FillParagraph()
  let old_cursor_pos = <SID>Mark()
  normal! gqip
  execute old_cursor_pos
endfunction

function! <SID>KillWord()
  if col('.') > strlen(getline('.'))
    return "\<Del>\<C-o>dw"
  else
    return "\<C-o>dw"
  endif
endfunction

function! <SID>KillLine()
  if col('.') > strlen(getline('.'))
    " At EOL; join with next line
    return "\<Del>"
  else
    " Not at EOL; kill until end of line
    return "\<C-o>d$"
  endif
endfunction

function! <SID>LetDefault(var_name, value)
  if !exists(a:var_name)
    execute 'let ' . a:var_name . '=' . a:value
  endif
endfunction

function! <SID>Mark(...)
  if a:0 == 0
    let mark = line(".") . "G" . virtcol(".") . "|"
    normal! H
    let mark = "normal!" . line(".") . "Gzt" . mark
    execute mark
    return mark
  elseif a:0 == 1
    return "normal!" . a:1 . "G1|"
  else
    return "normal!" . a:1 . "G" . a:2 . "|"
  endif
endfun

command! -nargs=+ LetDefault call s:LetDefault(<f-args>)

" Incremental Searching and Query Replace
inoremap <C-s> <C-o>:call <SID>StartSearch('/')<CR><C-o>/
inoremap <C-r> <C-o>:call <SID>StartSearch('?')<CR><C-o>?
inoremap <M-n> <C-o>:cnext<CR>
inoremap <M-p> <C-o>:cprevious<CR>
inoremap <C-M-s> <C-o>:call <SID>StartSearch('/')<CR><C-o>/
inoremap <C-M-r> <C-o>:call <SID>StartSearch('?')<CR><C-o>?
inoremap <M-s> <C-o>:set invhls<CR>
inoremap <M-%> <C-o>:call <SID>QueryReplace()()<CR>
inoremap <C-M-%> <C-o>:call <SID>QueryReplace()_regexp()<CR>
cnoremap <C-r> <CR><C-o>?<Up>

command! QueryReplace :call <SID>QueryReplace()()
command! QueryReplaceRegexp :call <SID>QueryReplace()_regexp()

LetDefault g:VM_SearchRepeatHighlight 0

function! <SID>StartSearch(search_dir)
  let s:incsearch_status = &incsearch
  let s:lazyredraw_status = &lazyredraw
  set incsearch
  cmap <C-c> <CR>
  cnoremap <C-s> <CR><C-o>:call <SID>SearchAgain()<CR><C-o>/<Up>
  cnoremap <C-r> <CR><C-o>:call <SID>SearchAgain()<CR><C-o>?<Up>
  cnoremap <silent> <CR> <CR><C-o>:call <SID>StopSearch()<CR>
  cnoremap <silent> <C-g> <C-c><C-o>:call <SID>AbortSearch()<CR>
  cnoremap <silent> <Esc> <C-c><C-o>:call <SID>AbortSearch()<CR>
  if a:search_dir == '/'
    cnoremap <M-s> <CR><C-o>:set invhls<CR><Left><C-o>/<Up>
  else
    cnoremap <M-s> <CR><C-o>:set invhls<CR><Left><C-o>?<Up>
  endif
  let s:before_search_mark = <SID>Mark()
endfunction

function! <SID>StopSearch()
  cunmap <C-c>
  cunmap <C-s>
  cunmap <C-r>
  cunmap <CR>
  cunmap <C-g>
  cnoremap <C-g> <C-c>
  if exists("s:incsearch_status")
    let &incsearch = s:incsearch_status
    unlet s:incsearch_status
  endif
  if g:VM_SearchRepeatHighlight == 1
    if exists("s:hls_status")
      let &hls = s:hls_status
      unlet s:hls_status
    endif
  endif
endfunction

function! <SID>AbortSearch()
  call <SID>StopSearch()
  if exists("s:before_search_mark")
    execute s:before_search_mark
    unlet s:before_search_mark
  endif
endfunction

function! <SID>SearchAgain()
  if g:VM_SearchRepeatHighlight == 1
    if !exists("s:hls_status")
      let s:hls_status = &hls
    endif
    set hls
  endif
endfunction
