
" vim6:fdm=marker:foldenable:ts=4:sw=4:foldclose=

"           TabLineSet.vim  -- Vim7+ tabline configuration utility  {{{
" ---------------------------------------------------------------------
"
"
" 
" Author:    Eric Arnold ( eric_p_arnold@yahoo.com )
" Last Change:  Sun Apr 16, 04/16/2006 8:06:24 AM
" Requires:    Vim 7
" Version:     1.2    Sat Apr 01, 04/01/2006 2:53:43 AM
" Version:     1.3    Sun Apr 02, 04/02/2006 1:36:01 AM
"             - Added more indicators, and toggle mapping funcs
"               for verbose control.
"             - Changed the name of the script from tabset.vim
"             - Solidified the non-GUI color scheme.
"            - Started some hooks to customize slack area.
" Version:     1.4    Mon Apr 03, 04/03/2006 6:47:11 AM
"            - added comma list of buffers contained in tab
"            - changed toggle and rotate mapping functions to
"              handle multiple option sets to switch through
" Version:     1.5    Sun Apr 09, 04/09/2006 1:50:28 AM
"             - added filter lists
"             - re-arranged the close button, the window counter,
"               and the (tab,win,buf) list.
" Version:     1.55  Sun Apr 16, 04/16/2006 8:04:17 AM
"             - disable the g:TabLineSetFillerFunc per the problem
"               with VIm70d.
" Version:     1.56  Thu Apr 20, 04/20/2006 5:14:24 AM
"             - Fixed bug with verbose auto-shutoff causing infinite
"               loop
" Version:     1.6    Mon May 01, 05/01/2006 10:40:15 AM
"             - Fixed problem where slow-down occurs when the
"             tabline function is called for every keystroke.  Use
"             the toggle or rotate mappings/functions to reset the
"             highlighting, if something clobbers it.
" Version:     1.61  Mon May 01, 05/01/2006 10:40:15 AM
"             - Really fix it this time.
" Version:     1.7    Thu May 04, 05/04/2006 1:04:14 AM
"             - Added pre- and post-proc function hooks
" Version:     1.7.1  Sat May 06, 05/06/2006 10:05:38 AM
"            - Reset highlighting based on synIDattr() and whether
"            some external operation has cleared all attributes.
" Version:     1.8    Sun May 14, 05/14/2006 10:45:07 AM
"            - Use the tabline wrapping patch if available.
" Version:     1.8.1  Sun May 14, 05/14/2006 4:56:45 PM
"            - Corrected indexing for mousefunc patch.
" Version:     1.8.2  Sun May 14, 05/14/2006 4:56:45 PM
"            - Added wrap up/down arrows for 'mousefunc' patch.
" Version:     1.8.3  Wed May 17, 05/17/2006 10:11:48 AM
"             - Checks for need to recalc, now ~instant return 
"             for page redraws
" Version:     2.0    Thu May 18, 05/18/2006 7:48:25 PM
"             - Heavily restructured for support for gui tabs;
"             probably installed some bugs too :-P
"             - Some speed tweaks
"             - Added g:TabLineSet_max_cols to enable GUI tab
"             scrolling
" Version:     2.0.1  Thu May 18, 05/18/2006 7:48:25 PM
"             - startup bug with Tab.._min undefined
"
" Acknowledgements:  Well, I started with the doc page example, I guess :-)
"
" Synopsis:
"
"  -  Configurable, intelligent/dynamic tab field sizing.
"
"  -  See all buffers loaded in windows in all tabs.
"
"  -  New colorscheme and general presentation.  The highlighting groups are
"    defined at the end of the script.  Tweak as desired.
"
"  -  The indicator sets are customizable.
"    (It will also turn verbose mode off automatically, as needed.)
"
"    You can add the   g:TabLineSet_.*   vars to your .vimrc, where you can
"    customize tab min/max, etc., and these these indicators:
"      modified    : whether any window in the tab needs saving
"      windows      : window count in the tab
"      buffers_list  : tab label contains comma list of buffers contained
"      closers      : add hot spots ("!") to the tab for click-to-close
"
"      These are mostly for development, but might be useful otherwise:
"      tabnr      : include the tab number for the selected tab/window
"      winnr      : ... window number
"      bufnr      : ... buffer number
"      filler_func    : tell it to use   g:TabLineSetFillerFunc   to
"                contain the name of a function to be evaluated
"                at runtime.  It's proof of concept, mostly.
"
"      All the user settable global variables are listed at the top of
"      the script after this.
"
"  -  This is an option which works with the experimental tabline wrapping
"    patch:
"      g:TabLineSet_max_wrap = 3
"    Change this to let tabs grow into GUI tab scrolling buttons:
"       g:TabLineSet_max_cols
"    and also to tweak for difference in GUI font size.
"
"  -  You can add these mappings to your .vimrc to control the verbose
"    settings on the fly:
"
"    The first one toggles all indicators off:
"
"      nmap <silent> <Leader>tv :call TabLineSet_verbose_toggle()<CR>
"
"    The second rotates through a list of option settings which
"    configurable/extensible via g:TabLineSet_verbose_sets.  See below.
"
"      nmap <silent> <Leader>tr :call TabLineSet_verbose_rotate()<CR>
"
"  -  Additional customization can be done via the filter lists.  These are
"    more complex, requiring use of regex's and such, but they allow you to
"    make arbitrary changes to the TabLine string at runtime.  There is
"    also preproc and postproc variables which can call back to your
"    function(s).
"
"  -  You have the choice of editing stuff in place here, but it might be
"    better to copy the vars and highlights of interest into your .vimrc .
"    I think I've isolated it all to be easy to cut and paste, maybe.
"
"  -  Finally, the ultimate customization:  dink around with the script
"    yourself :-)
"
"
" Issues:
"
" }}}

 



"                     Configuration variables section                   {{{
" -------------------------------------------------------------------------
"
" 

if v:version >= 700
else
  echomsg "Vim 7 or higher is required for TabLineSet.vim"
  finish
endif

let g:TabLineSet_version = 2.0

if exists( 'g:no_load_TabLineSet' )    " Turn it off from .vimrc
  finish
endif


let g:TabLineSet_min_tab_len = 6    " minimum tab width (space padded)

let g:TabLineSet_max_tab_len = 999
                    " try some smaller number, i.e. 30
                    " when using 'full_path' option

let g:TabLineSet_max_cols = &columns  
                    " you might want to set it higher than
                    " &columns for GUI with scroll buttons

let g:TabLineSet_verbose_auto = 4    " turn on/off verbose at this tab width

let g:TabLineSet_max_wrap = 15      " maximum rows to wrap tabline into
                    " Value can be 1+


" Readonly, but could be useful:
if !exists('g:TabLineSet_tab_status')
  let g:TabLineSet_tab_status = {}
endif



" Masterlist:  do not change.
let s:all_verbose_options = 
  \ [ 
  \  'modified', 'windows', 'full_path', 'buffers_list', 'closers', 
  \  'tabnr', 'winnr', 'bufnr', 'filler_func'
  \ ]


" You can config this in place here, or add() to it (see below):

let g:TabLineSet_verbose_sets = 
  \ [
    \ [ 
    \  'modified', 'windows', 'buffers_list', 'closers', 
    \  'tabnr', 'winnr', 'bufnr', 'filler_func'
    \ ],
    \ [ 'modified', 'windows', 'buffers_list', 'closers', 'filler_func' ],
    \ [ 'modified', 'windows', 'closers' ],
    \ [ 'modified', 'windows', 'closers', 'full_path' ],
    \ [ 'buffers_list' ],
    \ [ 'modified', 'tabnr', 'windows', 'buffers_list' ]
  \ ]
    " ^^^
    "  |
    "  +-----------    Your option list(s) here:
    "                  or
    "  Here:
    "  |
    "  V
call add( g:TabLineSet_verbose_sets, [ 'closers', 'buffers_list' ] )


" As promised, there is still a string variable for the options list
" which can be set like:
"
"    let g:TabLineSet_verbose = 'modified,windows,closers'
"
" even though here it is being set from the lists above:
"
let g:TabLineSet_verbose = join( g:TabLineSet_verbose_sets[5], ',' )
"
" You should then probably add it to the option/verbose-level set, so 
" it doesn't get clobbered if you rotate through the sets:
"
"    call add( g:TabLineSet_verbose_sets, [ g:TabLineSel_verbose ] )
"



" This nested list holds substitution triplets that well be applied to each
" buffer name via  substitute()  before it is added to the tab.   Note that
" this script does a pretty brute force method of shrinking the tabs to fit by
" looping a lot.  If you add bunches of filters, it might affect performance.
"
let g:TabLineSet_bufname_filters = [ 
    \   [ '\[No Name\]',    '[]'    ],
    \   [ '^--*\(.*\)--*$',    '\1',  '' ]
    \ ]
    " The first example filters out all unnamed buffers.
    " The second example filters out --minibufexplorer-- type lines that
    " show up in all tabs if the minibufexplorer is used.


" The following allows you to define substitutions on each tab as a whole.
" This begins to get tricky, since what you see at the top of your Vim is a
" small part of the format string that is sent to the tabline formatter.
" You're on your own as to whether it messes up the tab's formatting, length,
" etc.
"
let g:TabLineSet_tab_filters = [
    \   [ ',',     ';',  'g' ]
    \ ]
    "\   [ '%#TabPunct\w*#,%#Tab\w*#',     ';',  'g' ]
    " This example removes the commans and their highlighting, and
    " replaces them with semi-colons.

if &guioptions =~ 'e'
  call add( g:TabLineSet_tab_filters, [ '[,;]',     ' & ',  'g' ] )
endif


if 0  "  don't execute, example only

" The folowing example replaces the leading "!" to include the current time
" using the substitute special \= evaluation feature.  
"
" First, clean out any copies of our changes because the tab length
" calculation makes multiple passes, each of which would other wise insert
" another timestamp.
call add( g:TabLineSet_tab_filters,   [ '%X%#DiffChange#\d\d:\d\d:\d\d',     '',  'g' ] )

" Note also that the new current time string would inherit the color of the
" "!" char, if it didn't include the %X%#..#  format string around the "!" .
call add( g:TabLineSet_tab_filters,   [ '!%X%#[^#]*#',     '\=MyFunc()',  '' ] )

function! MyFunc()
  let s = strftime("%H:%M:%S")
  " Since this increases the length of the tab outside of the TabLineSet
  " functions, so incrementing the  g:TabLineSet_out_pos to account for
  " the extra chars will help it, a little, to draw.  
  let g:TabLineSet_out_pos += strlen(s)
  return submatch(0) . '%X%#DiffChange#' . s
endfunction

endif  " end of don't execute



" This performs substitutions at the full tabline level.  The possibility to
" make a mess increases :-)
"
let g:TabLineSet_tabline_filters = [
      \ [ '^',    '%#TabLine#>>%#TabSep# | ' ],
      \ [ '$',     '' ]
      \ ]


" This holds a copy of the final (huge!) string with all the embedded syntax
" and or highlighting.  You can use it to help decide how you want to make
" filters.
"
let g:TabLineSet_output_pre = ''
let g:TabLineSet_output_post = ''




" Use the filler func to doddle in the ending  space in the tabline:
"
"let g:TabLineSetFillerFunc = 'TabLineSetFillerTest'
let g:TabLineSetFillerFunc = ''


" This is called for each evaluation pass and can set the initial
" value of the tabline string.
"
let g:TabLineSet_preproc_func = ''
"let g:TabLineSet_preproc_func = 'Tst_preproc'



" This is passed the final tabline string.  It's a final hook for whatever.
"
let g:TabLineSet_postproc_func = ''
"let g:TabLineSet_postproc_func = 'Tst_postproc_modified'



"  End config vars  
" 
" ----------------------------------------------------------------------}}}





"                          TabLineSet_main()                           {{{
" -------------------------------------------------------------------------
"
" 



" These are all static script vars so that it will handle the way guitablabel
" re-enters for each tab:
"
let s:tabline_out = ''
let s:tabline_pieces = {}
let g:TabLineSet_tablabels = {}

function! TabLineSet_main( ... )

  let s:is_gui = &guioptions =~ 'e'

  if !exists('s:called_hl_init')
    let s:called_hl_init = 1
    call TabLineSet_hl_init()
  endif

  if synIDattr(synIDtrans(hlID("TabPunct")), "fg") == ''
    call TabLineSet_hl_init()
  endif


  if s:is_gui && v:lnum > 1
    return g:TabLineSet_tablabels[ v:lnum ]
  endif

  " ------------------------------------------------------------
  " Don't recalc unless something has changed:
  "
  let s:check_bufnrs = []
  let t = {}
  for tabnr in range( 1, tabpagenr('$') )
    for bufnr in  tabpagebuflist( tabnr )
      if index( s:check_bufnrs, bufnr ) < 0
        call add( s:check_bufnrs, bufnr )
      endif
      let t[bufnr] = {}
      let t[bufnr].tabnr = tabnr

      if s:is_gui
        " winnr isn't stabile with GUI tabs
        "let t[bufnr].winnr = tabpagewinnr( bufwinnr( bufnr ) )
      else
        let t[bufnr].winnr = tabpagewinnr( bufwinnr( bufnr ) )
      endif

            " Seems to need bufname as well as bufnr in some cases:
            "
      let t[bufnr].bufname = bufname( bufnr )
      let t[bufnr].modified = getbufvar( bufnr, '&modified' )

      if s:is_gui
        let t[bufnr].curr_window = s:bufenter_winnr
      else
        let t[bufnr].curr_window = 
                        \( tabpagenr() && winnr() == tabpagewinnr( tabnr ) )
      endif
    endfor
  endfor
  if s:is_gui
    let t.curr_tabnr = s:bufenter_tabnr
  else
    let t.curr_tabnr = tabpagenr()
  endif
  let t.is_gui = s:is_gui
  let t.wrap = g:TabLineSet_max_wrap
  let t.max_cols = g:TabLineSet_max_cols
  let t.max_tab_len = g:TabLineSet_max_tab_len
  let t.verbose = g:TabLineSet_verbose
  let t.columns = &columns

"  for key in keys(t)
"    if t[key] != g:TabLineSet_tab_status[key]
"      echomsg 'diff ' . key . ' = ' . string( t[key] ) . ' != ' . string( g:TabLineSet_tab_status[key] )
"    endif
"  endfor

  if t == g:TabLineSet_tab_status
  \ && g:TabLineSet_output_post != ''
    if s:is_gui 
      "echomsg 'return '.g:TabLineSet_tablabels[ v:lnum ]
      return g:TabLineSet_tablabels[ v:lnum ]
    else
      return g:TabLineSet_output_post
    endif
  endif
  let g:TabLineSet_tab_status = deepcopy(t)

  "
  " End recalc calc
  " ------------------------------------------------------------

  return s:Fill_tab_labels()

endfunction





function! s:Fill_tab_labels()

  let s:verbose = g:TabLineSet_verbose
  let s:min_tab_len = g:TabLineSet_min_tab_len
  let g:TabLineSet_tablabels = {}

  " Must check bufnames instead of numbers, since when opening a file name
  " from a no-name buffer, the bufnr doesn't seem to change.
  "
  for bufnr in s:check_bufnrs
    if has_key( s:bufnames, bufnr ) && bufname( bufnr ) == s:bufnames[bufnr]
    else
      call s:Fill_bufnames()
      break
    endif
  endfor

  let usable_columns = max( [ &columns , g:TabLineSet_max_cols ] )
  let usable_columns = usable_columns * g:TabLineSet_max_wrap


  let s:avail = usable_columns

  let s:tabline_out = ''
  let s:overflow = 1


  let loop = 0
  while ( loop < 10 ) && ( s:avail > 1 ) && ( s:overflow > 0 )
    "echomsg 'loop: ' . loop . ', overflow: ' . s:overflow

    let loop += 1

    " g:TabLineSet_out_pos will hold the total number of chars, as they
    " will appear in the tab line (without formatting strings).
    "
    let g:TabLineSet_out_pos = 0
    let g:TabLineSet_row = 0
    let g:TabLineSet_col = 0
    let g:TabLineSet_idxs = ''


    let tabs_overflow = 0

    "    if s:verbose =~ 'buffers_list'
    "      "let s:min_tab_len = g:TabLineSet_min_tab_len * 2
    "      let s:min_tab_len = 15
    "    endif

    " ------------------------------------------------------------
    "
    "  Pre-processing custom regex:
    "
    let tabline_out = ''
    if g:TabLineSet_preproc_func != ''
      let tabline_out = {g:TabLineSet_preproc_func}( )
    endif



    let tabnrs = range( 1, tabpagenr('$') )

    " ------------------------------------------------------------
    "
    " Info gathering tab page loop
    " 
    for tabnr in tabnrs

      let s:tabline_pieces[tabnr] = {}

      if s:is_gui
        let is_selected = ( tabnr == s:bufenter_tabnr ) 
      else
        let is_selected = ( tabnr == tabpagenr() ) 
      endif

      let bufnr_list = tabpagebuflist( tabnr )


      let tab_curr_winnr = tabpagewinnr( tabnr )
      let numwins = tabpagewinnr( tabnr, ("$") )


      let tablabel = ''
      let tablabel_len = 0

      let tablabel .= is_selected ? '%#TabLineSel#' : '%#TabLine#'


      " ------------------------------------------------------------
      " Add an indicator that some buffer in the tab is modified:
      "

      let s:tabline_pieces[tabnr].modded_chars = ''
      for bufnr in bufnr_list
        if s:verbose =~ 'modified' && getbufvar( bufnr,  '&modified' ) > 0
          let s:tabline_pieces[tabnr].modded_chars = '+'
          let tablabel .= '%#TabModded#'
                \ . "+"
                \ . ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
          let tablabel_len += 1
          break
        endif
      endfor



      " ------------------------------------------------------------
      " Misc values
      "

      let s:tabline_pieces[tabnr].misc_vals = ''

      let numwins_out = ''
      if is_selected == 0 && s:verbose =~ 'windows'  && len( bufnr_list ) > 1
        let numwins_out = numwins
      endif

      let tabnr_out = ''
      if s:verbose =~ 'tabnr' "&& is_selected
        let tabnr_out .= tabnr
      endif

      let winnr_out = ''
      if s:verbose =~ 'winnr' && is_selected
        let winnr_out .= 'w' . tab_curr_winnr 
      endif

      let bufnr_out = ''
      if s:verbose =~ 'bufnr' && is_selected
        if s:is_gui
          let bufnr_out .= 'b' . s:bufenter_bufnr
        else
          let bufnr_out .= 'b' . winbufnr( tab_curr_winnr )
        endif
      endif


      let out_list = [ numwins_out, tabnr_out, winnr_out, bufnr_out ]
      let out_list = filter( out_list, 'v:val != "" ' )
      if len( out_list ) > 0
        let s:tabline_pieces[tabnr].misc_vals = join( out_list , ',' )
      endif


      if s:tabline_pieces[tabnr].misc_vals != ''
        let r_brac = ''
        let l_brac = ':'
        let tablabel .= 
              \   r_brac
              \ . ( is_selected ? '%#TabWinNumSel#' : '%#TabWinNum#' )
              \ . s:tabline_pieces[tabnr].misc_vals
              \ . ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
              \ . l_brac

        let tablabel_len += strlen( s:tabline_pieces[tabnr].misc_vals
              \ . r_brac . l_brac )
      endif

      "
      " End Misc values, i.e. the number of windows in the tab:
      "
      " ------------------------------------------------------------





      " ------------------------------------------------------------
      "
      " Gather bufnames
      "

      "      let winnr_start = 1
      "      let winnr_stop = numwins
      "      if s:verbose !~ 'buffers_list'
      "        let winnr_start = tab_curr_winnr
      "        let winnr_stop = tab_curr_winnr
      "      endif


      if s:is_gui
        let s:tabline_pieces[tabnr].curr_bufnr = s:bufenter_bufnr
      else
        let s:tabline_pieces[tabnr].curr_bufnr =
              \ bufnr_list[ tab_curr_winnr - 1 ]
      endif

      if s:verbose =~ 'buffers_list'
        let s:tabline_pieces[tabnr].bufnr_list = bufnr_list
      else
        let s:tabline_pieces[tabnr].bufnr_list = 
              \ [ s:tabline_pieces[tabnr].curr_bufnr ]
      endif

      let save_tablabel_len = tablabel_len

      let stop = 0
      while !stop
        "
        " Loop is for resetting/growing bufnames into available space
        " due to s:min_tab_len

        let out_bufname_list = []

        let tablabel_len = save_tablabel_len

        for bufnr in s:tabline_pieces[tabnr].bufnr_list
          let out_bufname = s:bufnames[ bufnr ]
          let tablabel_len += strlen( out_bufname )
          if is_selected
                \ && s:verbose =~ '\(tabnr\|winnr\|bufnr\)' 
                \ && s:tabline_pieces[tabnr].curr_bufnr == bufnr
                \ && len( s:tabline_pieces[tabnr].bufnr_list ) > 1

            let out_bufname = 
                  \ '%#TabModded#' 
                  \ . '>'
                  \ . out_bufname
                  \ .  ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
          endif
          call add( out_bufname_list, out_bufname )
        endfor

        let sep = ''
              \ . ( is_selected ? '%#TabPunctSel#' : '%#TabPunct#' )
              \ . ','
              \ . ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )

        let tabbufnames = join( out_bufname_list, sep )
        let tablabel_len += 1 * len( out_bufname_list )    " add in separators len

        "
        " If there is extra space because of min_tab_len, grow the
        " names if possible:
        "
        if tablabel_len < ( s:min_tab_len - 3 )
          let bufs = {}
          for bufnr in  s:tabline_pieces[tabnr].bufnr_list
            let bufs[ bufnr ] = s:bufnames_orig[ bufnr ]
          endfor
          let bufs_len = strlen( join( values( bufs ), ',' ) )
          "echomsg 'checking ' . string(bufs)
          "echomsg bufs_len .' >  '. strlen( tabbufnames )
          if bufs_len > strlen( tabbufnames )
            let avail = ( s:min_tab_len - save_tablabel_len  -  3 )
            let shrink = ( bufs_len - avail )
            if shrink > 0
              let bufs = s:Shrink_bufnames( bufs, shrink )
            endif
            for bufnr in  keys( bufs )
              let s:bufnames[ bufnr ] = bufs[ bufnr ]
            endfor
            "echomsg 'reset ' . string(bufs)

            " restart the loop to re-format and reset tablabel_len
            continue
          endif
        endif

        " end bufnames formatting section
        " ------------------------------------------------------------
        break
      endwhile


      "echomsg 'min len ' . s:min_tab_len . ', label len ' . tablabel_len . ', s: ' . tabbufnames
      let tab_pad = max( [ 0, s:min_tab_len - ( 3 + tablabel_len ) ] )
      if tab_pad
        "echomsg 'padding ' . tab_pad . ' for ' . tabbufnames
        let tab_pad = repeat( ' ', tab_pad )
        if !s:is_gui
          let tabbufnames .= ''
                \ . ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
                \ . tab_pad
        else
          let tabbufnames .= tab_pad
        endif
      endif




      let tablabel_len += tab_pad

      let tabs_overflow += max( [ 0, ( 3 + tablabel_len ) - g:TabLineSet_max_tab_len ] )




      " ------------------------------------------------------------
      "  Closers
      "
      "
      let tabexit = ''
      if s:verbose =~ 'closers' && !s:is_gui
        let tabexit .= ( is_selected ? '%#TabExitSel#' : '%#TabExit#' )
              \ . '%' . tabnr . 'X!%X'
        let tabexit .= ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
      endif




      " ------------------------------------------------------------
      "  Put the pieces together
      "
      if !s:is_gui
        let tablabel_len += 2
        let tablabel = '%' . ( tabnr ) . 'T'
              \ . tabexit . tablabel . '' . tabbufnames
              \ . '%T'
              \ . '%#TabSep#' . ' | '
              \ . ( is_selected ? '%#TabLineSel#' : '%#TabLine#' )
        "
        " Note: it's important to have the final %T before
        " the separator char, '|', so it won't be a part
        " of the tab, and therefore won't help the
        " wrapping function to break correctly.
      else
        let tablabel .= tabbufnames
      endif



      " ------------------------------------------------------------
      "  Tab label custom regex:
      "
      for elem in g:TabLineSet_tab_filters
        while len( elem ) < 3 | call add( elem, '' ) | endwhile
        let tablabel = substitute( tablabel, 
              \ elem[0], elem[1], elem[2] )
      endfor




      " ------------------------------------------------------------
      "
      "  Start of length calculation
      "
      "      let total += strlen( s:tabline_pieces[tabnr].modded_chars )
      "      let total += strlen( join( s:tabline_pieces[tabnr].bufnr_list, ',' ) )
      "      let total += s:tabline_pieces[tabnr].curr_bufnr > 0 
      "      let total += strlen( r_brac . s:tabline_pieces[tabnr].misc_vals . l_brac )
      "      let total += s:verbose =~ 'closers' && !s:is_gui






      " ------------------------------------------------------------
      "  Handle tab wrapping:
      "
      if !s:is_gui && g:TabLineSet_max_wrap > 1

        " compensate for trailing line space on wrapped tablines
        " created by the internal wrapping [patch]
        "
        if ( ( g:TabLineSet_out_pos + tablabel_len + 1 ) / &columns )
              \ > g:TabLineSet_row 
          let g:TabLineSet_row += 1
          let g:TabLineSet_out_pos += &columns - g:TabLineSet_col
          let g:TabLineSet_idxs .= 
                \ repeat( ' ', &columns - g:TabLineSet_col )
          let g:TabLineSet_col = 0
        endif
      endif


      let s = substitute( tablabel, '%#[^#]*#', '', 'g' )
      let s = substitute( s, '%\d*[=XT]', '', 'g' )
      let tablabel_len = strlen( s )


      let g:TabLineSet_col += tablabel_len
      let g:TabLineSet_out_pos += tablabel_len
      let g:TabLineSet_idxs .= repeat( tabnr, tablabel_len )


      let g:TabLineSet_tablabels[ tabnr ] = tablabel




    endfor " for tabnr in tabnrs
    "
    " --------------------------------------------------



    if s:is_gui
      " This wigs out the gui labels after a certain size.
    else
      for tabnr in tabnrs
        let tabline_out .= g:TabLineSet_tablabels[ tabnr ]
      endfor
    endif



    " --------------------------------------------------
    "  Final formatting
    "
    " after the last tab fill with TabLineFill and reset tab page nr
    let last_close = ''
    if !s:is_gui
      let tabline_out .= '%#TabLineFillEnd#'

      "let last_close = repeat(' ', &columns - g:TabLineSet_col )
      if tabpagenr('$') > 1 && s:verbose == ''
        let last_close .= '%=%#TabLine#%999X!X%X%##'
      endif
      let g:TabLineSet_out_pos += 2
    endif

    if !s:is_gui && exists('&mousefunc')       
      " tabline called from in mousefunc? && &mousefunc != ''
      if g:TabLineSet_max_wrap > 1
        let last_close .= ' <-'
        let g:TabLineSet_out_pos += 2
      endif
      let s = ' ' . g:TabLineSet_max_wrap . ' -> '
      let g:TabLineSet_out_pos += strlen( s )
      let last_close .= s
    endif


    let a = ( usable_columns - 1 ) 
          \ - g:TabLineSet_out_pos 
          \ - ( last_close == '' ? 2 : 0 )

    if g:TabLineSetFillerFunc != '' && s:verbose =~ 'filler_func'
      let tabline_out .= '%{' . g:TabLineSetFillerFunc . '(' . a . ')}'
    endif

    let tabline_out .= last_close



    let g:TabLineSet_output_pre = tabline_out

    for elem in g:TabLineSet_tabline_filters 
      while len( elem ) < 3 | call add( elem, '' ) | endwhile
      let tabline_out = substitute( tabline_out, 
            \ elem[0], elem[1], elem[2] )
    endfor

    let g:TabLineSet_output_post = tabline_out

    if g:TabLineSet_postproc_func != ''
      call call( g:TabLineSet_postproc_func, [ tabline_out ] )
    endif


    let s = substitute( tabline_out, '%#[^#]*#', '', 'g' )
    let s = substitute( s, '%\d*[=XT]', '', 'g' )


    let s:overflow = strlen( s ) - s:avail 
    "echomsg 's:' . s
    "echomsg 'avail:' . s:avail . ', overflow end ' . s:overflow
    "echomsg 's len:' . strlen(s)

    "let g:TabLineSet_out_pos += tablabel_len
    if loop == 3 && s:overflow > 0
      let verbose = ""
      continue
    endif
    if loop == 5 && s:overflow > 0
      "echomsg 'clearing min tab len ..........'
      let s:min_tab_len = 1
      continue
    endif

    if s:overflow < tabs_overflow
      let s:overflow = tabs_overflow
    endif

    if s:overflow > 0
      let s:bufnames = s:Shrink_bufnames( s:bufnames, s:overflow + 1 )
    endif
    "echomsg 'tabs o '.tabs_overflow

    "echomsg 'shrunk ' . string( s:bufnames )


    if s:longest_bufname < g:TabLineSet_verbose_auto 
      let s:save_verbose = s:verbose
      let s:verbose = '' 
      "echomsg 'verbose off'
      call s:Fill_bufnames()
      "    elseif s:verbose == ''
      "    \ && s:longest_bufname >= g:TabLineSet_verbose_auto 
      "      let s:verbose = s:save_verbose
      "      echomsg 'verbose on'
      "      call s:Fill_bufnames()
    endif

  endwhile " big loop

  if s:is_gui && v:lnum > 0
    return g:TabLineSet_tablabels[ v:lnum ]
  endif

  return tabline_out

endfunction


" End main function  }}}





" ------------------------------------------------------------
" Fill bufnames{} 
"
let s:bufnames = {}
let s:bufnames_orig = {}

function! s:Fill_bufnames()

  let s:bufnames = {}

  for tabnr in range( 1, tabpagenr('$') )

    "let winnr_start = 1
    "let winnr_stop = tabpagewinnr( tabnr, "$")

    "?for winnr1 in range( winnr_start, winnr_stop )
    for bufnr in tabpagebuflist( tabnr )
      "let bufnr = winbufnr( winnr1 )
      let bufname = bufname( bufnr )
      if s:verbose =~ 'full_path'
        let bufname = fnamemodify( bufname, ':p' )
      else
        let bufname = fnamemodify( bufname, ':t' )
      endif
      if bufname == ''
        let bufname = '[No Name]'
      endif

"      while strlen( bufname ) < g:TabLineSet_min 
"        let bufname .= ' '
"      endwhile

      " Custom regex:
      "
      for elem in g:TabLineSet_bufname_filters
        while len( elem ) < 3 | call add( elem, '' ) | endwhile
        let bufname = substitute( bufname, 
              \ elem[0], elem[1], elem[2] )
      endfor

      let s:bufnames[ bufnr ] = bufname
    endfor
  endfor

  let s:bufnames_orig = deepcopy( s:bufnames )

endfunction
"
" End Fill bufnames{} 
"
" ------------------------------------------------------------





function! s:abs( i )
  return max( [ a:i, -1 * a:i ] )
endfunction

" ------------------------------------------------------------
"
" Shrink the names to fit available columns
"
let s:longest_bufname = 0
"let s:shortest_bufname = 0

function! s:Shrink_bufnames( bufnames, shrink )

  let bufnames = a:bufnames

  function! Dict_by_strlen(i1, i2)
    let len1 = strlen( s:sort_dict[ a:i1 ] )
    let len2 = strlen( s:sort_dict[ a:i2 ] )
    return len1 == len2 ? 0 : len1 > len2 ? 1 : -1
  endfunction

  let s:sort_dict = bufnames
  let bufnames_keys_by_len = reverse( sort( keys( s:sort_dict ), "Dict_by_strlen") )

  let bufnames_count = min( [ 5, len( bufnames_keys_by_len ) ] )
  if bufnames_count < 1 | return [] | endif
  let s:longest_bufname = strlen( bufnames[ bufnames_keys_by_len[0] ] )
"  let s:shortest_bufname = s:longest_bufname
  let bufnames_joined_len = strlen( join( values( bufnames ), '' ) )
  let shrink = s:abs( a:shrink )
  let reduced_total = 0
  let loop_counter = 0

  "echomsg string( bufnames_keys_by_len )
  "echomsg string( bufnames )
  "echomsg 'longest ..... ' . s:longest_bufname
  
  "while bufnames_joined_len >= a:avail
  while reduced_total < shrink
    let reduced = 0
    " Too slow to use increment of 1, so:

    "let incr = s:longest_bufname * ( ( shrink - reduced_total ) / bufnames_count )
    let incr = ( shrink - reduced_total ) / bufnames_count

    let incr = min( [ incr, ( s:longest_bufname / 2 ) ] )
    if incr < 1 | let incr = 1 | endif

    "echomsg ' shrink:' . shrink . ', total:'.reduced_total . '=' . ( shrink -reduced_total) . ', incr:' . incr . ', bufn len:' . bufnames_count
    " Preserves sorted order for keys:
    for bufnr in bufnames_keys_by_len
      if strlen( bufnames[ bufnr ] ) >= s:longest_bufname

  let loop_counter += 1
  "echomsg 'cutting ' . bufnames[bufnr] . ', len=' . strlen( bufnames[ bufnr ] ) . ', longest='  . s:longest_bufname . ', incr=' . incr



"        if strlen( bufnames[ bufnr ]  ) == 0
"          "continue
"        else
"          let s:shortest_bufname = 
"            \ min( [ s:shortest_bufname, strlen( bufnames[ bufnr ]  ) - incr ] )
"        endif
        if bufnames[ bufnr ] =~ '[/\\]'
          let bufnames[ bufnr ]  = bufnames[ bufnr ][ incr :]
        else
          let bufnames[ bufnr ]  = bufnames[ bufnr ][0:0 - incr - 1]
        endif
        let reduced += incr
        let reduced_total += incr
        let bufnames_joined_len -= incr
      
        "if bufnames_len < a:avail
        if reduced_total < shrink
          break
        endif
      else
      endif
    endfor
    if !reduced 
      let s:longest_bufname -= incr
    endif
  endwhile
  "echomsg 'shrink loop count ' .  loop_counter 

  return bufnames
  
endfunction
"
" End Shrink the names to fit available columns
"
" ------------------------------------------------------------






function! Tst_preproc( tabline )
  return '[test]' . a:tabline
endfunction


let s:modified_table = {}
function! Tst_postproc_modified( tabline )
  let updated = []
  for bufnr in range( 1, bufnr("$") )
    if !bufexists( bufnr ) | continue | endif
    let modded = getbufvar( bufnr, '&modified' )
    if !has_key( s:modified_table, bufnr )
      let s:modified_table[ bufnr ] = modded
    endif
    if s:modified_table[ bufnr ] != modded
      let s:modified_table[ bufnr ] = modded
      call add( updated, [ bufnr, modded ] )
    endif
  endfor

  for elem in updated
    let bufnr = elem[0]
    let modded = elem[1]
    echomsg 'bufnr#' . bufnr . ', ' . bufname( bufnr ) . ' is now ' . ( modded ? '' : 'no' ) . 'modified'
  endfor

  " call somefunction( updated )

endfunction
    




"                          Misc functions                              {{{
" -------------------------------------------------------------------------
"
" 


function! TabLineSetFillerNull( avail )
  return ''
endfunction


let s:test_count = 0
function! TabLineSetFillerTest( avail )
  let s:test_count += 1
  let out = strftime( '%H:%M:%S' ) . '#' . s:test_count
  if strlen( out ) > a:avail
    let out = ''
  else
    while strlen( out ) <= a:avail
      let out = '.'. out
    endwhile
  endif
  return out
endfunction


let s:TabLineSet_verbose_save = ''


function! TabLineSet_verbose_toggle()
  call TabLineSet_hl_init()
  call TabLineSet_verbose_toggle0()
  call s:Force_tabline_update()
endfunction



" Have it split up to use this internally, when a "1 new" will fail in the
" sandbox.
function! TabLineSet_verbose_toggle0()
  if s:TabLineSet_verbose_save == ''
    let s:TabLineSet_verbose_save = g:TabLineSet_verbose
    let g:TabLineSet_verbose = ''
  else
    let g:TabLineSet_verbose = s:TabLineSet_verbose_save
    let s:TabLineSet_verbose_save = ''
  endif

endfunction



function! s:Force_tabline_update()
  " Make it update:
  1new
  quit
endfunction



let s:all_verbose_sets_idx = 0

function! TabLineSet_verbose_rotate()
  call TabLineSet_hl_init()
  let s:all_verbose_sets_idx = s:all_verbose_sets_idx + 1
  if s:all_verbose_sets_idx > len( g:TabLineSet_verbose_sets ) - 1
    let s:all_verbose_sets_idx = 0
  endif

  let g:TabLineSet_verbose = join( 
        \g:TabLineSet_verbose_sets[ s:all_verbose_sets_idx ], ',' )
  "silent! normal! gtgT
  echomsg 'Tabline options: ' . g:TabLineSet_verbose
  call s:Fill_tab_labels()
  1new
  quit
endfunction


" End Misc functions  }}}





"                          Highlighting (configurable)                  {{{
" -------------------------------------------------------------------------
"
" 


set tabline=%!TabLineSet_main()
set guitablabel=%!TabLineSet_main()
"set guitablabel=%!TabLineSet_main()
if exists('&guitabtooltip')
  set guitabtooltip=%!TabLineSet_guitabtooltip()
endif

if &showtabline < 1
  set showtabline=1  " 2=always
endif

function! TabLineSet_hl_init()
  "              *cterm-colors*
  "      NR-16   NR-8    COLOR NAME ~
  "      0      0      Black
  "      1      4      DarkBlue
  "      2      2      DarkGreen
  "      3      6      DarkCyan
  "      4      1      DarkRed
  "      5      5      DarkMagenta
  "      6      3      Brown, DarkYellow
  "      7      7      LightGray, LightGrey, Gray, Grey
  "      8      0*      DarkGray, DarkGrey
  "      9      4*      Blue, LightBlue
  "      10      2*      Green, LightGreen
  "      11      6*      Cyan, LightCyan
  "      12      1*      Red, LightRed
  "      13      5*      Magenta, LightMagenta
  "      14      3*      Yellow, LightYellow
  "      15      7*      White
  "
  "  The number under "NR-16" is used for 16-color terminals ('t_Co'

  hi! TabWinNum term=bold,underline cterm=underline gui=bold,underline
        \ ctermfg=green guifg=green guibg=DarkGrey

  hi! TabWinNumSel term=bold,underline cterm=underline gui=bold,underline
        \ ctermfg=green guifg=green guibg=darkgrey

  hi! TabPunct term=bold,underline cterm=underline gui=bold,underline
        \ ctermfg=cyan guifg=cyan ctermbg=darkgrey guibg=DarkGrey

  hi! TabPunctSel term=bold,underline cterm=underline gui=bold,underline
        \ ctermfg=magenta ctermbg=blue guifg=Magenta guibg=#0000ff

  hi! TabLineFill term=underline cterm=underline gui=underline

  hi! TabLineFillEnd term=underline cterm=underline gui=underline

  hi! TabLine term=underline cterm=underline

  hi! TabLineSel  term=bold,reverse,underline
        \ ctermfg=lightgreen cterm=underline,bold

  hi! TabModded term=underline,bold cterm=underline,bold ctermfg=red

  hi! TabExit term=underline,bold ctermfg=red guifg=red guibg=darkgrey
        \  cterm=underline gui=underline

  hi! TabExitSel gui=underline term=underline,bold guifg=green guibg=blue
        \  cterm=underline ctermfg=green ctermbg=blue

  hi! TabSep term=underline cterm=underline
        \ gui=underline


endfunction


call TabLineSet_hl_init()

" End highlighting   }}}



function! TabLineSet_guitabtooltip()

  let tabnr = v:lnum

  "let numwins_out = tabpagewinnr( tabnr, "$" )
  let label = ''

  for winnr in range( 1, tabpagewinnr( tabnr, "$" ) )
    "for bufnr in tabpagebuflist( tabnr )
    let bufnr = winbufnr( winnr )

    let tabnr_out = ''
    if g:TabLineSet_verbose =~ 'tabnr'
      let tabnr_out = 't' . tabnr
    endif
    let winnr_out = ''
    if g:TabLineSet_verbose =~ 'winnr'
      let winnr_out = 'w' . winnr
    endif
    let bufnr_out = ''
    if g:TabLineSet_verbose =~ 'bufnr'
      let bufnr_out = 'b' . bufnr
    endif


    let out_list = [ tabnr_out, winnr_out, bufnr_out ]
    let out_list = filter( out_list, 'v:val != "" ' )

    if len( out_list ) > 0
      let misc_vals = join( out_list , ',' )
      let r_brac = ''
      let l_brac = ':'
      let label .= 
            \   r_brac
            \ . misc_vals
            \ . l_brac
    endif

    if g:TabLineSet_verbose =~ 'full_path'
      let label .= fnamemodify( bufname( bufnr ), ':p' ) 
    else
      let label .= fnamemodify( bufname( bufnr ), ':t' ) 
    endif

    "let label .= "\r\n\<nl>"
    "let label .= "\<eol>"
    let label .= " ; "
  endfor
  return label
endfunction

aug TabLineSet_au
  au!
  au BufEnter * call TabLineSet_BufEnter()
  au WinEnter * call TabLineSet_WinEnter()
aug END


" This is for the GUI tabs, which don't have information on what is the
" current notion of tab/winnr
function! TabLineSet_BufEnter()
  let s:bufenter_tabnr = tabpagenr()
  let s:bufenter_winnr = winnr()
  let s:bufenter_bufnr = bufnr("%")
  "echomsg 't ' .s:bufenter_tabnr . ', w ' . s:bufenter_winnr . ', b ' . s:bufenter_bufnr 
endfunction

function! TabLineSet_WinEnter()
  call TabLineSet_BufEnter()
endfunction

call TabLineSet_BufEnter()




"let g:TabLineSet_tab_status = {}
"let g:TabLineSet_min_tab_len = 30
