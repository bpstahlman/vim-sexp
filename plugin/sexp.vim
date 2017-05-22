
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
    \ 'sexp_toggle_sexp_state':         '<C-k>',
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
    \ 'sexp_flow_to_prev_close':        '<M-[>',
    \ 'sexp_flow_to_next_open':         '<M-]>',
    \ 'sexp_flow_to_prev_open':         '<M-{>',
    \ 'sexp_flow_to_next_close':        '<M-}>',
    \ 'sexp_flow_to_prev_leaf_head':    '<M-S-b>',
    \ 'sexp_flow_to_next_leaf_head':    '<M-S-w>',
    \ 'sexp_flow_to_prev_leaf_tail':    '<M-S-g>',
    \ 'sexp_flow_to_next_leaf_tail':    '<M-S-e>',
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
    \ 'sexp_convolute':                 '<LocalLeader>?',
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
    \ ['sexp_toggle_sexp_state',         'nx'],
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
    \ ['sexp_flow_to_prev_close',        'nx'],
    \ ['sexp_flow_to_next_open',         'nx'],
    \ ['sexp_flow_to_prev_open',         'nx'],
    \ ['sexp_flow_to_next_close',        'nx'],
    \ ['sexp_flow_to_prev_leaf_head',    'nx'],
    \ ['sexp_flow_to_next_leaf_head',    'nx'],
    \ ['sexp_flow_to_prev_leaf_tail',    'nx'],
    \ ['sexp_flow_to_next_leaf_tail',    'nx'],
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

    let postfix = ' \| call <SID>after_map_executed("' . a:name . '")<CR>'
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
        execute prefix . postfix
    " Expression, repeating, operator-pending mode
    elseif opmode
        execute prefix . ' \| '
                \ . 'if v:operator ==? "c" \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')\<lt>C-r>.\<lt>C-Bslash>\<lt>C-n>", b:sexp_count) \| '
                \ . 'else \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')", b:sexp_count) \| '
                \ . 'endif' . postfix
    " Expression, repeating, non-operator-pending mode
    else
        execute prefix . ' \| call <SID>repeat_set("\<Plug>(' . a:name . ')", b:sexp_count)' . postfix
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

" Return object corresponding to input 'plug' string:
" {'lhs': [<lhs>, <sexp-state-lhs>], 'flags': 'map-flags'}
" Logic: Prefer user overrides in g:sexp_mappings, but fallback to default in
" s:sexp_mappings if user has not overridden, or override is invalid. Warn if
" override is invalid.
function! s:sexp_get_mapping(plug)
    for map in [g:sexp_mappings, s:sexp_mappings]
        if has_key(map, a:plug)
            let m = map[a:plug]
            " Use type/len of m to determine what's been overridden.
            "   string:            "lhs"
            "   1 element list:  [ "sexp_state_lhs" ]
            "   2 element list:  [ "lhs", "sexp_state_lhs" ]
            "   2 element list:  [ "lhs", "sexp_state_lhs", "flags" ]
            " First, validate the form of the sexp_mappings entry.
            " Design Decision: Empty arrays and/or lhs strings will not be
            " considered error: rather, they will be indication that user has
            " chosen to disable the corresponding map. Note that, due to the way
            " mappings are defined, an all whitespace lhs is effectively empty.
            if empty(m)
                " Mapping completely disabled.
                return ret
            endif
            let mt = type(m)
            let flags = '' " flags default to clear
            if mt == 1 " string
                " Legacy format: i.e., no sexp-mode mapping
                let lhs = [m, '']
            elseif mt == 3 " list
                let mlen = len(m)
                if mlen == 1
                    " sexp-mode map only
                    " Rationale: If user plans to use a map *only* in
                    " sexp-mode, he can free up the lhs defined in
                    " s:sexp_mappings. Note that the following 2 forms are
                    " equivalent: ['lhs'], ['', 'lhs']
                    " Caveat: If flags are specified, non-sexp-state lhs must
                    " be specified explicitly, even if empty.
                    let lhs = ['', m[0]]
                elseif mlen >= 2 && mlen <= 3
                    " both normal and sexp-mode maps (and possibly flags)
                    let lhs = m[:1]
                    if mlen == 3
                        let flags = m[2]
                    endif
                endif
            endif
            if exists('lhs')
                " We have a valid entry; no need to look further.
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
    " Make sure an all whitespace lhs is treated as empty.
    call map(lhs, 'substitute(v:val, ''^\s\+$'', "", "g")')
    return {'lhs': lhs, 'flags': flags}
endfunction

" Combine raw map configuration represented in both sexp_mappings and
" s:plug_map_modes into a convenient form and cache the resulting dictionary
" in buf-local b:sexp_map_cfg.
" Cache Format:
" {plug: {'modes': ['n|x|o', ...],
"         'lhs': ['lhs', 'sexp-state-lhs'],
"         'flags': "map-flags"},
"  ...}
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
        " Get canonical form lhs/flags for this plug, and cache it along with
        " the mode info.
        let m = s:sexp_get_mapping(plug)
        let b:sexp_map_cfg[plug] = {
            \ 'modes': split(modestr, '\zs'),
            \ 'lhs': m.lhs,
            \ 'flags': m.flags
        \ }
    endfor
    " Return the cache as convenience to caller.
    return b:sexp_map_cfg
endfunction

function! s:destroy_map(lhs, modes)
    " Caveat: In some cases, map may have been removed already.
    for mode in a:modes
        silent! execute mode . 'unmap <buffer>' . a:lhs
    endfor
endfunction

function! s:create_map(lhs, plug, modes, nowait)
    for mode in a:modes
        execute mode . 'map ' . (a:nowait ? '<nowait>' : '')
            \ . '<silent><buffer> ' . a:lhs . ' <Plug>(' . a:plug . ')'
    endfor
endfunction

" TODO: Rewrite comment...
" Create all non-insert mode mappings applicable to the input plugin mode
" (special or non-special). If entering special mode (sexp_state=1), shadow
" any ambiguous/conflicting maps, saving the information needed to restore
" them in a buf-local structure of the following form:
" {state: <sexp-state>,
"  maps: [{plug1: modes1, plug2: modes2}, [plug1, plug2, ...]]}
" Note: Create both the first time we enter sexp-state.
function! s:sexp_create_non_insert_mappings()
    if !exists('b:sexp_map_cache')
        " Initialize data structure used to short-circuit processing after
        " the first full cycle of sexp-state toggles.
        let b:sexp_map_cache = {'state': 0}
    endif
    let smc = b:sexp_map_cache
    " Get configuration.
    let cfgs = s:sexp_get_map_cfg()
    if !has_key(smc, 'maps')
        " First or second half of first cycle
        if smc.state
            " Initialize data structure used to short-circuit processing
            " required for creating/destroying maps when toggling between sexp
            " and non-sexp states.
            " Note: We build the data structures on the second half of first
            " cycle, and simply re-use it thereafter.
            let smc.maps = [{}, []]
        endif
        " Loop over configuration.
        " Note: If deterministic order is desired, could use s:plug_map_modes
        " to define order.
        for [plug, cfg] in items(cfgs)
            let lhs = cfg.lhs[!!smc.state]
            if empty(lhs)
                " No mapping in the current plugin state.
                continue
            endif
            let modes = cfg.modes
            if smc.state
                " Entering sexp-state; check for conflicting non-sexp state
                " maps.
                for [plug_, cfg_] in items(cfgs)
                    let re_modes = '[' . join(cfg_.modes, '') . ']'
                    " Determine common modes.
                    let cmodes = filter(copy(modes), 'v:val =~ re_modes')
                    if !empty(cmodes) &&
                        \ stridx(cfg_.lhs[0], lhs) == 0 ||
                        \ stridx(lhs, cfg_.lhs[0]) == 0
                        " Record conflicting non-sexp-state map.
                        if has_key(smc.maps[0], plug_)
                            let smc.maps[0][plug_] =
                                \ uniq(extend(smc.maps[0][plug_], cmodes))
                        else
                            let smc.maps[0][plug_] = cmodes
                        endif
                        " Shadow (remove) the conflicting non-sexp-state map.
                        call s:destroy_map(lhs, cmodes)
                    endif
                endfor
                " Record sexp-state map.
                call add(smc.maps[1], plug)
            endif
            " Create the map in the indicated modes.
            " TODO: Consider making create/destroy_map take a cfg entry; would
            " permit create/destroy to be parameterized.
            call s:create_map(lhs, plug, modes, !!smc.state)
        endfor
    else
        " At least 1 toggle cycle complete.
        " Create or remove sexp-state maps
        for plug in smc.maps[1]
            let cfg = cfgs[plug]
            if smc.state
                call s:create_map(cfg.lhs[1], plug, cfg.modes, 1)
            else
                call s:destroy_map(cfg.lhs[1], cfg.modes)
            endif
        endfor
        " Restore or shadow non-sexp-state maps
        for [plug, modes] in items(smc.maps[0])
            let cfg = cfgs[plug]
            if smc.state
                " Shadow.
                call s:destroy_map(cfg.lhs[0], cfg.modes)
            else
                " Restore shadowed.
                call s:create_map(cfg.lhs[0], plug, cfg.modes, 0)
            endif
        endfor
    endif
endfunction

" Toggle between 'special' and non-special modes.
function! s:toggle_sexp_state(...)
    let mode = a:0 ? a:1 : ''
    " Could pass toggle arg to sexp_create_non_insert_mappings to obviate need
    " for setting toggle here.
    let b:sexp_map_cache.state = !b:sexp_map_cache.state
    " TODO: Perhaps change name of this, now that it does more...
    call s:sexp_create_non_insert_mappings()
    if mode == 'v'
        normal! gv
    endif
endfunction

function! s:in_sexp_state()
    return !!b:sexp_map_cache.state
endfunction

function! s:is_flag_set(plug, flag)
    " TODO: Maybe use a different method to search for literal chars only, and
    " possibly allow multiple flags?
    return b:sexp_map_cfg[a:plug].flags =~ a:flag
endfunction

" Callback called just after any map has been executed.
function! s:after_map_executed(plug)
    " If we're not currently in sexp-state, and the map just executed is
    " marked as auto-enabling sexp-state...
    if !s:in_sexp_state() && s:is_flag_set(a:plug, '>')
        " Toggle sexp-state ON
        call s:toggle_sexp_state()
    endif
endfunction


" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
function! s:sexp_create_mappings()
    call s:sexp_create_non_insert_mappings()
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
DEFPLUG  nnoremap sexp_toggle_sexp_state :call <SID>toggle_sexp_state('n')<CR>
DEFPLUG  xnoremap sexp_toggle_sexp_state <Esc>:<C-u>call <SID>toggle_sexp_state('v')<CR>

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

" List flow commands
Defplug   nnoremap sexp_flow_to_prev_close sexp#list_flow('n', v:count, 0, 1)
DEFPLUG   xnoremap sexp_flow_to_prev_close <Esc>:<C-u>call sexp#list_flow('v', v:prevcount, 0, 1)<CR>
Defplug   nnoremap sexp_flow_to_prev_open sexp#list_flow('n', v:count, 0, 0)
DEFPLUG   xnoremap sexp_flow_to_prev_open <Esc>:<C-u>call sexp#list_flow('v', v:prevcount, 0, 0)<CR>
Defplug   nnoremap sexp_flow_to_next_open sexp#list_flow('n', v:count, 1, 0)
DEFPLUG   xnoremap sexp_flow_to_next_open <Esc>:<C-u>call sexp#list_flow('v', v:prevcount, 1, 0)<CR>
Defplug   nnoremap sexp_flow_to_next_close sexp#list_flow('n', v:count, 1, 1)
DEFPLUG   xnoremap sexp_flow_to_next_close <Esc>:<C-u>call sexp#list_flow('v', v:prevcount, 1, 1)<CR>

" Leaf flow commands
DefplugN  nnoremap sexp_flow_to_prev_leaf_head sexp#leaf_flow('n', v:count, 0, 0)
DEFPLUG   xnoremap sexp_flow_to_prev_leaf_head <Esc>:<C-u>call sexp#leaf_flow('v', v:prevcount, 0, 0)<CR>
DefplugN  nnoremap sexp_flow_to_next_leaf_head sexp#leaf_flow('n', v:count, 1, 0)
DEFPLUG   xnoremap sexp_flow_to_next_leaf_head <Esc>:<C-u>call sexp#leaf_flow('v', v:prevcount, 1, 0)<CR>
DefplugN  nnoremap sexp_flow_to_prev_leaf_tail sexp#leaf_flow('n', v:count, 0, 1)
DEFPLUG   xnoremap sexp_flow_to_prev_leaf_tail <Esc>:<C-u>call sexp#leaf_flow('v', v:prevcount, 0, 1)<CR>
DefplugN  nnoremap sexp_flow_to_next_leaf_tail sexp#leaf_flow('n', v:count, 1, 1)
DEFPLUG   xnoremap sexp_flow_to_next_leaf_tail <Esc>:<C-u>call sexp#leaf_flow('v', v:prevcount, 1, 1)<CR>

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

" Convolute
" Note: convolute takes pains to preserve cursor position: hence, 'nojump'.
DefplugN! nnoremap sexp_convolute sexp#convolute(v:count, 'n')

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
