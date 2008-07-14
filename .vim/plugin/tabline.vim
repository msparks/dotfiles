if v:version >= 700
else
  finish
endif

function! MyTabLine()
  let s = ' '
  for i in range(tabpagenr('$'))
    " select the highlighting
    if i + 1 == tabpagenr()
      let s .= '%#TabLineNumSel#' . (i + 1) . '%## %#TabLineSel#'
    else
      let s .= '%#TabLineNum#' . (i + 1) . '%## %#TabLine#'
    endif

    " set the tab page number (for mouse clicks)
    let s .= '%' . (i + 1) . 'T'

    " the label is made by MyTabLabel()
    let s .= '%{MyTabLabel(' . (i + 1) . ')}%##  '
  endfor

  " after the last tab fill with TabLineFill and reset tab page nr
  let s .= '%#TabLineFill#%T'

  " right-align the label to close the current tab page
  if tabpagenr('$') > 1
    let s .= '%=%#TabLine#%999X X'
  endif

  "echomsg 's:' . s
  return s
endfunction

function! MyTabLabel(n)
  let buflist = tabpagebuflist(a:n)
  let winnr = tabpagewinnr(a:n)
  let numtabs = tabpagenr('$')
  " account for space padding between tabs, and the "close" button
  let maxlen = ( &columns - ( numtabs * 2 ) - 4 ) / numtabs
  let tablabel = bufname(buflist[winnr - 1])
  while strlen( tablabel ) < 4
    let tablabel = tablabel . " "
  endwhile
  let tablabel = fnamemodify( tablabel, ':t' )
  let tablabel = strpart( tablabel, 0, maxlen )
  return tablabel
endfunction

set tabline=%!MyTabLine()
set showtabline=1  " 2=always

hi! TabLineNum cterm=underline term=underline ctermfg=magenta
hi! TabLine cterm=underline term=underline
hi! TabLineFill cterm=underline term=underline

hi! TabLineNumSel ctermfg=magenta cterm=underline,bold
hi! TabLineSel cterm=bold,underline
