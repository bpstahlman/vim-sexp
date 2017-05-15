
"              o8o
"              '"'
"  oooo    ooooooo ooo. .oo.  .oo.        .oooo.o  .ooooo. oooo    ooooo.ooooo.
"   `88.  .8' `888 `888P"Y88bP"Y88b      d88(  "8 d88' `88b `88b..8P'  888' `88b
"    `88..8'   888  888   888   888 8888 `"Y88b.  888ooo888   Y888'    888   888
"     `888'    888  888   888   888      o.  )88b 888    .o .o8"'88b   888   888
"      `8'    o888oo888o o888o o888o     8""888P' `Y8bod8P'o88'   888o 888bod8P'
"                                                                      888
"                                                                     o888o
"  Author:   guns <self@sungpae.com>
"  License:  MIT
"  Homepage: https://github.com/guns/vim-sexp

if exists('g:sexp_loaded')
    finish
endif
let g:sexp_loaded = 1

""" Global State {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp,timl'
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_insert_after_wrap')
    let g:sexp_insert_after_wrap = 1
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

let s:sexp_mappings = {
    \ 'sexp_toggle_special':            '<C-k>',
    \ 'sexp_outer_list':                'af',
    \ 'sexp_inner_list':                'if',
    \ 'sexp_outer_top_list':            'aF',
    \ 'sexp_inner_top_list':            'iF',
    \ 'sexp_outer_string':              'as',
    \ 'sexp_inner_string':              'is',
    \ 'sexp_outer_element':             'ae',
    \ 'sexp_inner_element':             'ie',
    \ 'sexp_move_to_prev_bracket':      '(',
    \ 'sexp_move_to_next_bracket':      ')',
    \ 'sexp_move_to_prev_element_head': '<M-b>',
    \ 'sexp_move_to_next_element_head': '<M-w>',
    \ 'sexp_move_to_prev_element_tail': 'g<M-e>',
    \ 'sexp_move_to_next_element_tail': '<M-e>',
    \ 'sexp_move_to_prev_top_element':  '[[',
    \ 'sexp_move_to_next_top_element':  ']]',
    \ 'sexp_select_prev_element':       '[e',
    \ 'sexp_select_next_element':       ']e',
    \ 'sexp_indent':                    '==',
    \ 'sexp_indent_top':                '=-',
    \ 'sexp_round_head_wrap_list':      '<LocalLeader>i',
    \ 'sexp_round_tail_wrap_list':      '<LocalLeader>I',
    \ 'sexp_square_head_wrap_list':     '<LocalLeader>[',
    \ 'sexp_square_tail_wrap_list':     '<LocalLeader>]',
    \ 'sexp_curly_head_wrap_list':      '<LocalLeader>{',
    \ 'sexp_curly_tail_wrap_list':      '<LocalLeader>}',
    \ 'sexp_round_head_wrap_element':   '<LocalLeader>w',
    \ 'sexp_round_tail_wrap_element':   '<LocalLeader>W',
    \ 'sexp_square_head_wrap_element':  '<LocalLeader>e[',
    \ 'sexp_square_tail_wrap_element':  '<LocalLeader>e]',
    \ 'sexp_curly_head_wrap_element':   '<LocalLeader>e{',
    \ 'sexp_curly_tail_wrap_element':   '<LocalLeader>e}',
    \ 'sexp_insert_at_list_head':       '<LocalLeader>h',
    \ 'sexp_insert_at_list_tail':       '<LocalLeader>l',
    \ 'sexp_splice_list':               '<LocalLeader>@',
    \ 'sexp_raise_list':                '<LocalLeader>o',
    \ 'sexp_raise_element':             '<LocalLeader>O',
    \ 'sexp_swap_list_backward':        '<M-k>',
    \ 'sexp_swap_list_forward':         '<M-j>',
    \ 'sexp_swap_element_backward':     '<M-h>',
    \ 'sexp_swap_element_forward':      '<M-l>',
    \ 'sexp_emit_head_element':         '<M-S-j>',
    \ 'sexp_emit_tail_element':         '<M-S-k>',
    \ 'sexp_capture_prev_element':      '<M-S-h>',
    \ 'sexp_capture_next_element':      '<M-S-l>',
    \ }

" Define the (non-insert) modes in which each mapping should be created.
" Note: Intentionally keeping this separate from s:sexp_mappings because user
" might want to copy s:sexp_mappings into his vimrc for tweaking, and user has
" no control over the modes in which mappings apply.
" TODO: Align this list.
let s:plug_map_modes = [
    \ ['sexp_outer_list',                'xo'],
    \ ['sexp_inner_list',                'xo'],
    \ ['sexp_outer_top_list',            'xo'],
    \ ['sexp_inner_top_list',            'xo'],
    \ ['sexp_outer_string',              'xo'],
    \ ['sexp_inner_string',              'xo'],
    \ ['sexp_outer_element',             'xo'],
    \ ['sexp_inner_element',             'xo'],
    \ ['sexp_move_to_prev_bracket',      'nxo'],
    \ ['sexp_move_to_next_bracket',      'nxo'],
    \ ['sexp_move_to_prev_element_head', 'nxo'],
    \ ['sexp_move_to_next_element_head', 'nxo'],
    \ ['sexp_move_to_prev_element_tail', 'nxo'],
    \ ['sexp_move_to_next_element_tail', 'nxo'],
    \ ['sexp_move_to_prev_top_element',  'nxo'],
    \ ['sexp_move_to_next_top_element',  'nxo'],
    \ ['sexp_select_prev_element',       'nxo'],
    \ ['sexp_select_next_element',       'nxo'],
    \ ['sexp_indent',                    'n'],
    \ ['sexp_indent_top',                'n'],
    \ ['sexp_insert_at_list_head',       'n'],
    \ ['sexp_insert_at_list_tail',       'n'],
    \ ['sexp_convolute',                 'n'],
    \ ['sexp_splice_list',               'n'],
    \ ['sexp_round_head_wrap_list',      'nx'],
    \ ['sexp_round_tail_wrap_list',      'nx'],
    \ ['sexp_square_head_wrap_list',     'nx'],
    \ ['sexp_square_tail_wrap_list',     'nx'],
    \ ['sexp_curly_head_wrap_list',      'nx'],
    \ ['sexp_curly_tail_wrap_list',      'nx'],
    \ ['sexp_round_head_wrap_element',   'nx'],
    \ ['sexp_round_tail_wrap_element',   'nx'],
    \ ['sexp_square_head_wrap_element',  'nx'],
    \ ['sexp_square_tail_wrap_element',  'nx'],
    \ ['sexp_curly_head_wrap_element',   'nx'],
    \ ['sexp_curly_tail_wrap_element',   'nx'],
    \ ['sexp_raise_list',                'nx'],
    \ ['sexp_raise_element',             'nx'],
    \ ['sexp_swap_list_backward',        'nx'],
    \ ['sexp_swap_list_forward',         'nx'],
    \ ['sexp_swap_element_backward',     'nx'],
    \ ['sexp_swap_element_forward',      'nx'],
    \ ['sexp_emit_head_element',         'nx'],
    \ ['sexp_emit_tail_element',         'nx'],
    \ ['sexp_capture_prev_element',      'nx'],
    \ ['sexp_capture_next_element',      'nx'],
    \ ['sexp_toggle_sexp_mode',          'nx']
\ ]

if !empty(g:sexp_filetypes)
    augroup sexp_filetypes
        autocmd!
        execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:sexp_create_mappings()'
    augroup END
endif

" Autoload and detect repeat.vim
silent! call repeat#set('')
let s:have_repeat_set = exists('*repeat#set')

""" Functions {{{1

command! -nargs=+       DEFPLUG  call <SID>defplug('000', <f-args>)
command! -nargs=+ -bang Defplug  call <SID>defplug('1' . string(!empty('<bang>')) . '0', <f-args>)
command! -nargs=+ -bang DefplugN call <SID>defplug('1' . string(!empty('<bang>')) . '1', <f-args>)

" Create a <Plug> mapping. The 'flags' faux bitfield dictates behavior:
"
"   * flags == 0**: Map rhs as a key sequence
"   * flags == 100: Map rhs as an expression
"   * flags == 110: Map rhs as an expression, and setup repeat
"   * flags == 101: Map rhs as an expression, and do not set '`
"   * flags == 111: Map rhs as an expression, set up repeat, and do not set '`
"
" We don't use an actual bitfield because the bitwise functions and() and or()
" were not introduced until patch 7.3.377.
"
function! s:defplug(flags, mapmode, name, ...)
    let lhs = a:mapmode . ' <silent> <Plug>(' . a:name . ')'
    let rhs = join(a:000)

    let asexpr = a:flags[0] == '1'
    let repeat = a:flags[1] == '1'
    let nojump = a:flags[2] == '1'
    let opmode = a:mapmode[0] ==# 'o'

    " Key sequence
    if !asexpr
        execute lhs . ' ' . rhs
        return 1
    endif

    " Common mapping prefix
    " RE: vv
    "   Due to a ?bug? in vim, we need to set curwin->w_curswant to the
    "   current cursor position by entering and exiting character-wise visual
    "   mode before completing an operator-pending command so that the cursor
    "   returns to its original position after an = command.
    let prefix = lhs . ' '
                 \ . ':<C-u>let b:sexp_count = v:count \| '
                 \ . (nojump ? '' : 'execute "normal! ' . (opmode ? 'vv' : '') . 'm`" \| ')
                 \ . 'call ' . substitute(rhs, '\v<v:count>', 'b:sexp_count', 'g')

    " Expression, non-repeating
    if !repeat || (repeat && !s:have_repeat_set)
        execute prefix . '<CR>'
    " Expression, repeating, operator-pending mode
    elseif opmode
        execute prefix . ' \| '
                \ . 'if v:operator ==? "c" \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')\<lt>C-r>.\<lt>C-Bslash>\<lt>C-n>", b:sexp_count) \| '
                \ . 'else \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')", b:sexp_count) \| '
                \ . 'endif<CR>'
    " Expression, repeating, non-operator-pending mode
    else
        execute prefix . ' \| call <SID>repeat_set("\<Plug>(' . a:name . ')", b:sexp_count)<CR>'
    endif
endfunction

" Calls repeat#set() and registers a one-time CursorMoved handler to correctly
" set the value of g:repeat_tick.
"
" cf. https://github.com/tpope/vim-repeat/issues/8#issuecomment-13951082
function! s:repeat_set(buf, count)
    call repeat#set(a:buf, a:count)
    augroup sexp_repeat
        autocmd!
        autocmd CursorMoved <buffer> let g:repeat_tick = b:changedtick | autocmd! sexp_repeat
    augroup END
endfunction

" Return pair of lhs corresponding to input 'plug' string:
" [<lhs>, <sexp-mode-lhs>]
" Logic: Prefer user overrides in g:sexp_mappings, but fallback to default in
" s:sexp_mappings if user has not overridden, or override is invalid. Warn if
" override is invalid.
function! s:sexp_get_mapping(plug)
    for map in [g:sexp_mappings, s:sexp_mappings]
        if has_key(map, a:plug)
            let m = map[a:plug]
            " Use type/len of m to determine what's been overridden.
            "   string:            "lhs"
            "   1 element list:  [ "sexp_mode_lhs" ]
            "   2 element list:  [ "lhs", "sexp_mode_lhs" ]
            " First, validate the form of the sexp_mappings entry.
            " Design Decision: Empty arrays and/or lhs strings will not be
            " considered error: rather, they will be indication that user has
            " chosen to disable the corresponding map. Note that, due to the way
            " mappings are defined, an all whitespace lhs is effectively empty.
            if empty(m)
                " Mapping completely disabled.
                return ['', '']
            endif
            let mt = type(m)
            if mt == 1 " string
                " Legacy format: i.e., no sexp-mode mapping
                let ret = [m, '']
            elseif mt == 3 " list
                if len(m) == 1
                    " sexp-mode map only
                    " Rationale: If user plans to use a map *only* in
                    " sexp-mode, he free up the lhs defined in
                    " s:sexp_mappings. Note that the following 2 forms are
                    " equivalent: ['lhs'], ['', 'lhs']
                    let ret = ['', m[0]]
                elseif len(m) == 2
                    " both normal and sexp-mode maps
                    let ret = [m[0], m[1]]
                endif
            endif
            if exists('ret')
                " We have a valid entry.
                break
            endif
            " Key exists but was invalid.
            " Assumption: Must be g:sexp_mappings, since invalid entry in
            " s:sexp_mappings would imply internal error.
            echohl WarningMsg
            echomsg "Skipping invalid entry in sexp_mappings list for " . a:plug
            echohl None
        endif
    endfor
    " Make sure all whitespace lhs is treated as empty.
    call map(ret, 'substitute(v:val, ''^\s\+$'', "", "g")')
    return ret
endfunction

" Combine raw map configuration represented in both sexp_mappings and
" s:plug_map_modes into a convenient form and cache the resulting dictionary
" in buf-local b:sexp_map_cfg.
" Cache Format: { plug: {modes: ['n|x|o', ...], ['lhs', 'sexp-mode-lhs']}}
" Return: The built cache, as convenience to caller.
" Note: The configuration information is cached only once, when mappings are
" first created for a buffer. Subsequent changes to g:sexp_mappings will have
" no effect until the buffer has been deleted.
function! s:sexp_get_map_cfg()
    " Try cache first.
    if exists('b:sexp_map_cfg')
        return b:sexp_map_cfg
    endif
    " Build the cache.
    let b:sexp_map_cfg = {}
    for [plug, modestr] in s:plug_map_modes
        let b:sexp_map_cfg[plug] = {
            \ 'modes': split(modestr, '\zs'),
            \ 'lhs': s:sexp_get_mapping(plug)
        \ }
    endfor
    " Return the cache as convenience to caller.
    return b:sexp_map_cfg
endfunction

" Delete any maps (global or buf-local) that are ambiguous or conflicting with
" the input lhs in the input mode, saving the information needed to restore
" the deleted map(s) in the input dictionary under a key determined by lhs:
" i.e.,
"   'lhs' => maparg('lhs', mode, 0, 1),
function! s:shadow_conflicting_maps(lhs, mode, maps)
    " Keep looping till there are no more ambiguities/conflicts.
    while 1
        " Check for conflict/ambiguity for specified lhs in indicated mode.
        let rhs = mapcheck(a:lhs, a:mode)
        if empty(rhs)
            " No more conflict/ambiguity
            return
        endif
        let hide = 0
        " Ambiguity or conflict exists, but we need more information to know
        " how to handle...
        if has_key(b:sexp_map_cfg, rhs)
            " sexp map (presumably a non-special one)
            let o = b:sexp_map_cfg[rhs]
            if !empty(o.lhs[1]) && index(o.modes, a:mode) >= 0
                " The non-special map's functionality is covered by a special
                " map in this mode; delete the non-special map to remove
                " ambiguity/conflict.
                " Possible Alternative Approach: Re-run sexp_create_mappings
                " upon exiting special; in that case, we wouldn't need to
                " save/restore the sexp buf-local maps.
                let hide = 1
            endif
        else
            " non-sexp map
            let ma = maparg(a:lhs, a:mode, 0, 1)
            if ma.buffer
                " Permit restoration when we exit special.
                let hide = 1
            else
                " No need to hide, since <buffer> and <nowait> effectively
                " prevent conflict with global maps, and since buf-local maps
                " take priority, the fact that we've reached a global implies
                " there are no more buf-locals.
                return
            endif
        endif
        " If hiding existing map, save info needed to restore it upon exit
        " from special, then delete.
        if hide
            let a:maps[a:lhs] = maparg(a:lhs, a:mode, 0, 1)
            execute a:mode . 'unmap <buffer>' . a:lhs
        endif
    endwhile
endfunction

" Restore mappings that were overridden upon entry into sexp mode, using
" information saved in the input dictionary, whose format is as follows:
" { plug: {modes: [], [lhs, sexp-mode-lhs]}}
function! s:sexp_restore_non_special_mappings(maps)
    " Delete the special maps.
    for [plug, cfg] in items(b:sexp_map_cfg)
        if !empty(cfg.lhs[1])
            for mode in cfg.modes
                " Unmap our buffer-specific temporary map.
                exe mode . 'unmap <buffer>' . cfg.lhs[1]
            endfor
        endif
    endfor
    " Restore ambiguous/conflicting buf-local maps.
    " Loop over modes.
    for [mode, maps] in items(a:maps)
        " Loop over mappings for this mode.
        for [lhs, mapobj] in items(maps)
            " Remap overridden map, taking into account its various modifiers.
            let mapcmd = mode . (mapobj.noremap ? 'noremap' : 'map')
                \ . (mapobj.silent ? ' <silent>' : '')
                \ . (mapobj.expr ? ' <expr>' : '')
                \ . (mapobj.buffer ? ' <buffer>' : '')
                \ . (mapobj.nowait ? ' <nowait>' : '')
                \ . ' ' . lhs . ' ' . mapobj.rhs
            if mapobj.sid
                " Caveat: If the map was originally defined in a script
                " context, we need to replace <SID> in the map with the
                " proper script-specific identifier.
                " Note: Vim doesn't appear to support escaping <SID> in
                " rhs, so we don't either.
                let mapcmd = substitute(mapcmd, '<SID>', '<SNR>' . mapobj.sid . '_', 'g')
            endif
            exe mapcmd
        endfor
    endfor
endfu

" Create all non-insert mode mappings applicable to the input plugin mode
" (special or non-special). If entering special mode (special=1), shadow any
" ambiguous/conflicting maps, saving the information needed to restore them in
" a buf-local map of the following form:
" {'n': {'lhs1': maparg('lhs1', 'n', 0, 1)}, ...,
"  'x': {'lhs1': maparg('lhs1', 'x', 0, 1)}, ...,
"  'o': {'lhs1': maparg('lhs1', 'o', 0, 1)}, ... }
function! s:sexp_create_non_insert_mappings(sexp_mode)
    if a:sexp_mode
        " The sexp mode mappings we're about to create are likely to override
        " existing mappings. Create a 2D hash (keyed by mode,lhs) to hold the
        " information we'll need to restore the original mappings upon exit
        " from sexp mode.
        " Note: Existence of this map implies we're in sexp mode.
        let b:sexp_map_save = {'n': {}, 'x': {}, 'o': {}}
    endif
    " Loop over configuration.
    for [plug, cfg] in items(s:sexp_get_map_cfg())
        if empty(cfg.lhs[!!a:sexp_mode])
            " No mapping in the current plugin mode.
            continue
        endif
        " Loop over Vim modes in which the plug is mapped.
        " Note: If deterministic order is desired, could use s:plug_map_modes
        " to define order.
        for mode in cfg.modes
            if a:sexp_mode
                " Hide (delete) ambiguous/conflicting maps, in such a way as
                " to permit restoration upon exit from special.
                call s:shadow_conflicting_maps(cfg.lhs[1], mode, b:sexp_map_save[mode])
            endif
            execute mode . 'map ' . (a:sexp_mode ? '<nowait>' : '')
                \ . '<silent><buffer> ' . cfg.lhs[!!a:sexp_mode] . ' <Plug>(' . plug . ')'
        endfor
    endfor
endfu

" Toggle between 'special' and non-special modes.
fu! s:toggle_sexp_mode(mode)
    " Are we in sexp mode or not?
    if exists('b:sexp_map_save')
        " Toggle OFF
        call s:sexp_restore_non_special_mappings(b:sexp_map_save)
        unlet! b:sexp_map_save
    else
        " Toggle ON
        call s:sexp_create_non_insert_mappings(1)
    endif
    if a:mode == 'v'
        " Don't let toggling sexp-mode clobber current selection.
        call sexp#select_current_marks('v')
    endif
endfu


" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
function! s:sexp_create_mappings()
    call s:sexp_create_non_insert_mappings(0)
    " Note: Insert-mode mappings are unaffected by sexp mode.
    if g:sexp_enable_insert_mode_mappings
        imap <silent><buffer> (    <Plug>(sexp_insert_opening_round)
        imap <silent><buffer> [    <Plug>(sexp_insert_opening_square)
        imap <silent><buffer> {    <Plug>(sexp_insert_opening_curly)
        imap <silent><buffer> )    <Plug>(sexp_insert_closing_round)
        imap <silent><buffer> ]    <Plug>(sexp_insert_closing_square)
        imap <silent><buffer> }    <Plug>(sexp_insert_closing_curly)
        imap <silent><buffer> "    <Plug>(sexp_insert_double_quote)
        imap <silent><buffer> <BS> <Plug>(sexp_insert_backspace)
    endif
endfunction

""" Toggle Sexp-Mode {{{1
DEFPLUG  nnoremap sexp_toggle_sexp_mode :call <SID>toggle_sexp_mode('n')<CR>
DEFPLUG  xnoremap sexp_toggle_sexp_mode <Esc>:<C-u>call <SID>toggle_sexp_mode('v')<CR>

""" Text Object Selections {{{1

" Current list (compound FORM)
Defplug  xnoremap sexp_outer_list sexp#docount(v:count, 'sexp#select_current_list', 'v', 0, 1)
Defplug! onoremap sexp_outer_list sexp#docount(v:count, 'sexp#select_current_list', 'o', 0, 1)
Defplug  xnoremap sexp_inner_list sexp#docount(v:count, 'sexp#select_current_list', 'v', 1, 1)
Defplug! onoremap sexp_inner_list sexp#docount(v:count, 'sexp#select_current_list', 'o', 1, 1)

" Current top-level list (compound FORM)
Defplug  xnoremap sexp_outer_top_list sexp#select_current_top_list('v', 0)
Defplug! onoremap sexp_outer_top_list sexp#select_current_top_list('o', 0)
Defplug  xnoremap sexp_inner_top_list sexp#select_current_top_list('v', 1)
Defplug! onoremap sexp_inner_top_list sexp#select_current_top_list('o', 1)

" Current string
Defplug  xnoremap sexp_outer_string sexp#select_current_string('v', 0)
Defplug! onoremap sexp_outer_string sexp#select_current_string('o', 0)
Defplug  xnoremap sexp_inner_string sexp#select_current_string('v', 1)
Defplug! onoremap sexp_inner_string sexp#select_current_string('o', 1)

" Current element
Defplug  xnoremap sexp_outer_element sexp#select_current_element('v', 0)
Defplug! onoremap sexp_outer_element sexp#select_current_element('o', 0)
Defplug  xnoremap sexp_inner_element sexp#select_current_element('v', 1)
Defplug! onoremap sexp_inner_element sexp#select_current_element('o', 1)

""" Text Object Motions {{{1

" Nearest bracket
Defplug  nnoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 0)
DEFPLUG  xnoremap sexp_move_to_prev_bracket <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 0)<CR>
Defplug! onoremap sexp_move_to_prev_bracket sexp#move_to_nearest_bracket('o', 0)
Defplug  nnoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 1)
DEFPLUG  xnoremap sexp_move_to_next_bracket <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 1)<CR>
Defplug! onoremap sexp_move_to_next_bracket sexp#move_to_nearest_bracket('o', 1)

" Adjacent element head
"
" Visual mappings must break out of visual mode in order to detect which end
" the user is using to adjust the selection.
DefplugN  nnoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('n', v:count, 0, 0, 0)
DEFPLUG   xnoremap sexp_move_to_prev_element_head <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 0, 0, 0)<CR>
DefplugN! onoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('o', v:count, 0, 0, 0)
DefplugN  nnoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('n', v:count, 1, 0, 0)
DEFPLUG   xnoremap sexp_move_to_next_element_head <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 1, 0, 0)<CR>
DefplugN! onoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('o', v:count, 1, 0, 0)

" Adjacent element tail
"
" Inclusive operator pending motions require a visual mode selection to
" include the last character of a line.
DefplugN  nnoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('n', v:count, 0, 1, 0)
DEFPLUG   xnoremap sexp_move_to_prev_element_tail <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 0, 1, 0)<CR>
DefplugN! onoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('o', v:count, 0, 1, 0)
DefplugN  nnoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('n', v:count, 1, 1, 0)
DEFPLUG   xnoremap sexp_move_to_next_element_tail <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 1, 1, 0)<CR>
DefplugN! onoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('o', v:count, 1, 1, 0)

" Adjacent top element
Defplug  nnoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('n', v:count, 0, 0, 1)
DEFPLUG  xnoremap sexp_move_to_prev_top_element <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 0, 0, 1)<CR>
Defplug! onoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('o', v:count, 0, 0, 1)
Defplug  nnoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('n', v:count, 1, 0, 1)
DEFPLUG  xnoremap sexp_move_to_next_top_element <Esc>:<C-u>call sexp#move_to_adjacent_element('v', v:prevcount, 1, 0, 1)<CR>
Defplug! onoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('o', v:count, 1, 0, 1)

" Adjacent element selection
"
" Unlike the other directional motions, calling this from normal mode places
" us in visual mode, with the adjacent element as our selection.
Defplug  nnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 0)
Defplug  xnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 0)
Defplug! onoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 0)
Defplug  nnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 1)
Defplug  xnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 1)
Defplug! onoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 1)

""" Commands {{{1

" Indent S-Expression
Defplug! nnoremap sexp_indent     sexp#indent(0, v:count)
Defplug! nnoremap sexp_indent_top sexp#indent(1, v:count)

" Wrap list
Defplug! nnoremap sexp_round_head_wrap_list  sexp#wrap('f', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_head_wrap_list  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_round_tail_wrap_list  sexp#wrap('f', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_tail_wrap_list  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_head_wrap_list sexp#wrap('f', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_head_wrap_list sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_tail_wrap_list sexp#wrap('f', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_tail_wrap_list sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_head_wrap_list  sexp#wrap('f', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_head_wrap_list  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_tail_wrap_list  sexp#wrap('f', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_tail_wrap_list  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Wrap element
Defplug! nnoremap sexp_round_head_wrap_element  sexp#wrap('e', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_head_wrap_element  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_round_tail_wrap_element  sexp#wrap('e', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_tail_wrap_element  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_head_wrap_element sexp#wrap('e', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_head_wrap_element sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_tail_wrap_element sexp#wrap('e', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_tail_wrap_element sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_head_wrap_element  sexp#wrap('e', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_head_wrap_element  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_tail_wrap_element  sexp#wrap('e', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_tail_wrap_element  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Insert at list terminal
Defplug! nnoremap sexp_insert_at_list_head sexp#insert_at_list_terminal(0)
Defplug! nnoremap sexp_insert_at_list_tail sexp#insert_at_list_terminal(1)

" Raise list
Defplug! nnoremap sexp_raise_list    sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_list', 'n', 0, 0)
Defplug  xnoremap sexp_raise_list    sexp#docount(v:count, 'sexp#raise', 'v', '')
Defplug! nnoremap sexp_raise_element sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_element', 'n', 1)
Defplug  xnoremap sexp_raise_element sexp#docount(v:count, 'sexp#raise', 'v', '')

" Splice list
Defplug! nnoremap sexp_splice_list sexp#splice_list(v:count)

" Swap list
Defplug! nnoremap sexp_swap_list_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 1)
DEFPLUG  xnoremap sexp_swap_list_backward <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 1)<CR>
Defplug! nnoremap sexp_swap_list_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 1)
DEFPLUG  xnoremap sexp_swap_list_forward  <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 1)<CR>

" Swap element
Defplug! nnoremap sexp_swap_element_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 0)
DEFPLUG  xnoremap sexp_swap_element_backward <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 0)<CR>
Defplug! nnoremap sexp_swap_element_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 0)
DEFPLUG  xnoremap sexp_swap_element_forward  <Esc>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 0)<CR>

" Emit/capture element
Defplug! nnoremap sexp_emit_head_element    sexp#docount(v:count, 'sexp#stackop', 'n', 0, 0)
Defplug  xnoremap sexp_emit_head_element    sexp#docount(v:count, 'sexp#stackop', 'v', 0, 0)
Defplug! nnoremap sexp_emit_tail_element    sexp#docount(v:count, 'sexp#stackop', 'n', 1, 0)
Defplug  xnoremap sexp_emit_tail_element    sexp#docount(v:count, 'sexp#stackop', 'v', 1, 0)
Defplug! nnoremap sexp_capture_prev_element sexp#docount(v:count, 'sexp#stackop', 'n', 0, 1)
Defplug  xnoremap sexp_capture_prev_element sexp#docount(v:count, 'sexp#stackop', 'v', 0, 1)
Defplug! nnoremap sexp_capture_next_element sexp#docount(v:count, 'sexp#stackop', 'n', 1, 1)
Defplug  xnoremap sexp_capture_next_element sexp#docount(v:count, 'sexp#stackop', 'v', 1, 1)

""" Insert mode mappings {{{1

" Insert opening delimiter
inoremap <silent><expr> <Plug>(sexp_insert_opening_round)  sexp#opening_insertion('(')
inoremap <silent><expr> <Plug>(sexp_insert_opening_square) sexp#opening_insertion('[')
inoremap <silent><expr> <Plug>(sexp_insert_opening_curly)  sexp#opening_insertion('{')

" Insert closing delimiter
inoremap <silent><expr> <Plug>(sexp_insert_closing_round)  sexp#closing_insertion(')')
inoremap <silent><expr> <Plug>(sexp_insert_closing_square) sexp#closing_insertion(']')
inoremap <silent><expr> <Plug>(sexp_insert_closing_curly)  sexp#closing_insertion('}')

" Insert double quote
inoremap <silent><expr> <Plug>(sexp_insert_double_quote) sexp#quote_insertion('"')

" Delete paired delimiters
inoremap <silent><expr> <Plug>(sexp_insert_backspace) sexp#backspace_insertion()

""" Cleanup {{{1

delcommand DefplugN
delcommand Defplug
delcommand DEFPLUG
