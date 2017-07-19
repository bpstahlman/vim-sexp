
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

let s:sexp_state_toggle = '<C-k>'

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

let s:re_unesc_pct = '\%(^\|[^%]\)\%(%%\)*\zs%%\@!'
let s:re_key_notation = '\c\v\<%('
    \ . '%(local)?leader'
    \ . '|%(t_)@!'
    \ . '%([SCMAD]-)*'
    \ . '%('
    \ . '|nul|bs|tab|nl|ff|cr|return|enter|esc|space|lt|bslash|bar|del|x?csi'
    \ . '|eol|up|down|left|right|f%(10|11|12|[1-9])|help|undo|insert'
    \ . '|home|end|pageup|pagedown|k%(home|end|page%(up|down)|plus|minus'
    \ . '|multiply|divide|enter|point|[0-9])'
    \ . '|[[:print:]]))\>'

if !empty(g:sexp_filetypes)
    augroup sexp_filetypes
        autocmd!
        execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:on_file_type()'
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

" TODO: Is this even needed now that it's in the key map? I think it should be
" one or the other... If it needs special handling, probably just remove from
" s:sexp_mappings.
function! s:create_sexp_state_toggle()
    for mode in ['n', 'x']
        execute mode . 'noremap <silent><buffer><nowait> '
            \ . s:get_sexp_state_toggle()
            \ . ' <Esc>:<C-u>call <SID>toggle_sexp_state('
            \ . (mode == "n" ? "'n'" : "'v'")
            \ . ')<CR>'
    endfor
endfunction

" Split a somewhat canonicalized lhs string into a list of even more
" canonicalized pieces: e.g.,
"   "<foo>" => ["<lt>", "f", "o", "o", ">"]
"   BUT
"   "<C-a>" => ["<C-A>"]
" Important CAVEAT: Because we're doing string comparisons on canonicalized
" forms (and case is generally sensitive in maps), we need either to
" canonicalize case here or use "\<...>" to convert to actual byte sequence:
" e.g., "\<F7>" => <80>k7
" I don't see this causing problems, since these canonical forms would be used
" only for in-memory comparisons.
" Note: lhs in both :map output and maparg() dict is somewhat canonicalized
" (i.e., <Space> rather than literal space).
" TODO: Decide which...
function! s:split_and_canonicalize_lhs(lhs)
    let ret = []
    " User's been instructed not to use literal whitespace in string.
    let s = substitute(a:lhs, '^\s*\|\s*$', '', 'g')
    if empty(s)
        return ret
    endif
    let [i, len] = [0, len(s)]
    while i < len
        let ie = matchend(s, '^' . s:re_key_notation, i)
        if ie >= 0
            let c = s[i:ie-1]
            " Check for something that *could* be special key notation.
            " TODO: Consider canonicalizing by using the "\<...>" notation in
            " a string expression to produce an actual key sequence (or
            " something beginning with '<' if it's not a valid key sequence).
            " Note: This strategy could be used to perform more reliable
            " validation than what's provided by s:re_key_notation.
            " Note: Vim does NOT translate key notation within mapleader and
            " maplocalleader.
            " Also Note: Both mapleader and maplocalleader default
            " *independently* to backslash.
            if c ==? '<Leader>'
                let c = get(g:, 'mapleader', '\')
            elseif c ==? '<LocalLeader>'
                let c = get(g:, 'maplocalleader', '\')
            elseif c !~ s:re_key_notation
                let c = '<LT>'
                " Let the rest be handled on subsequent iterations.
                let ie = i + 1
            endif
        else
            " Definitely not key notation.
            " TODO: Need to handle bare '<'.
            " Note: This match is guaranteed to succeed.
            let ie = matchend(s, '.', i)
            let c = s[i:ie-1]
            " With default 'cpo' (which we're assuming), bslash is not
            " special in mappings, but canonical form requires it to be
            " expressed in key-notation.
            " TODO: Maybe convert all the things that might be expressed
            " literally, like Space and Tab.
            if c == '\'
                let c = '<BSLASH>'
            elseif c == '<'
                let c = '<LT>'
            endif
        endif
        call add(ret, c)
        let i = ie
    endwhile
    return ret
endfunction

function! s:by_lhs_sort_fn(a, b)
    return a:a.lhs < a:b.lhs ? -1 : a:a.lhs > a:b.lhs ? 1 : 0
endfunction

function! s:usermap_sort_fn(a, b)
    " Note: Comparison must be case-sensitive.
    if a:a.lhs < a:b.lhs
        return -1
    elseif a:a.lhs > a:b.lhs
        return 1
    else
        " Assumption: Modes have already been expanded.
        let [alen, blen] = [len(a:a.modes), len(a:b.modes)]
        " Sort by decreasing modes string length so that maps requiring use of
        " :map command come first (should be 1 at most, so we could optimize).
        return alen < blen ? 1 : alen > blen ? -1 : 0
    endif
endfunction

" Mode Functions: As a general rule, the mode functions assume mode strings
" have been "expanded": i.e.,
"   1. no space chars
"   2. empty string means empty mode flags, *not* 'nvo'
"   3. v has been converted to xs
" The expand/collapse_modes functions provide an optional arg to request
" transitioning between nvo (or nxso) and the special empty string/<space>
" representations. The and/or_modes functions ensure uniqueness of mode flags
" in returned string, which allows modes to be combined/accumulated without
" fear of introducing redundancies.

" Expand the input mode string.
" Assumption: No redundant flags.
" Rationale: expand_modes is always used on a single source, and hence, should
" never contain redundant flags.
" Optional a:1 indicates what (if anything) expands to 'nvo':
"   0 = no special expansion (default)
"   1 = space only
"   2 = empty string or space
function! s:expand_modes(modes, ...)
    return a:0 && a:modes == ' ' && a:1 > 0 || a:modes == '' && a:1 > 1
        \ ? 'nxso'
        \ : substitute(a:modes, 'v', 'xs', 'g')
endfunction

" Collapse x and s into a single v, and if optional arg requests it, convert
" nvo (in any order) back to empty string or space.
" Assumption: No redundant flags.
" Rationale: All mode combining functions prevent redundancies.
" Optional a:1 indicates what (if anything) 'nvo' gets collapsed to:
"   0 = no special collapse (default)
"   1 = space only
"   2 = empty string
function! s:collapse_modes(modes, ...)
    let ret = a:modes
    " Convert s and x (not necessarily contiguous) to v
    if ret =~ 's.*x\|x.*s'
        " Note: 'v' should be impossible here, but it doesn't hurt to check.
        let ret = 'v' . substitute(ret, '[xsv]', '', 'g')
    endif
    " Return the collapsed string, possibly after converting nvo to space or
    " empty string, as requested by optional arg.
    return !a:0 || !a:1
        \ ? ret
        \ : len(ret) == 3 && ret =~ '^[nvo]\+$'
            \ ? a:1 == 1 ? ' ' : ''
            \ : ret
endfunction

function! s:or_modes(a, b)
    return join(uniq(sort(split(a:a . a:b, '\zs'))), '')
endfunction

" Return intersection of input mode strings.
function! s:and_modes(a, b)
    return join(uniq(sort(filter(
        \ split(a:a, '\zs'), 'stridx(a:b, v:val) >= 0'))), '')
endfunction

" Subtract mode strings: a - b
function! s:subtract_modes(a, b)
    return substitute(a:a, '[' . a:b . ']', '', 'g')
endfunction

function! s:is_multi_modes(modes)
    " Note: A multi-mode command, when expanded, has at least 2 modes, at
    " least one of which must be something other than xs (which would collapse
    " to single-mode command v).
    return len(a:modes) > 1 && a:modes =~ '[^xsv]'
endfunction

" Compare the head of input lhs's, returning 0 if one is a prefix of the
" other. If neither lhs is prefix of the other, return...
"   -1 if a < b
"   +1 if a > b
" Note: Used to detect ambiguity. Typically, only zero/nonzero is significant.
function! s:compare_prefix(a, b)
    let cmp_len = min([len(a:a), len(a:b)])
    let [a, b] = [a:a[:cmp_len-1], a:b[:cmp_len-1]]
    return a < b ? -1 : a > b ? 1 : 0
endfunction

" Is a prefix of b
function! s:is_prefix_of(a, b)
    return a:a == a:b[:len(a:a) - 1]
endfunction

function! s:check_ambiguity(a, b, ...)
    let warn = a:0 && !!a:1
    " In the absence of mode overlap, there can be no ambiguity.
    let omodes = s:and_modes(a:a.modes, a:b.modes)
    if !empty(omodes) && !s:compare_prefix(a:a.lhs, a:b.lhs)
        if warn
            echohl WarningMsg
            echomsg "Maps " . a:a.lhs . " and " a:b.lhs . " are ambiguous"
                \ . " in modes " . omodes
            echohl None
        endif
        return 1
    endif
    return 0
endfunction

" Input: mapinfo is basically a maparg() dict, but with a 'script' flag
" indicating whether the <script> tag was used.
function! s:build_delete_cmds(mapinfo, modes)
    let [mi, cmds] = [a:mapinfo, []]
    " Collapse modes to determine whether :unmap can be used.
    " Rationale: Might as well use plain :unmap when modes == ' ' (nvo).
    let modes = s:collapse_modes(a:modes, 1)
    for mode in modes == ' ' ? [''] : split(modes, '\zs')
        call add(cmds, mode . 'unmap '
            \ . (mi.buffer ? '<buffer>' : '') . mi.lhs)
    endfor
    " Combine the map cmds into single string.
    return join(cmds, '|')
endfunction

function! s:build_restore_cmds(mapinfo)
    " Remap overridden map, taking into account its various modifiers.
    let mi = a:mapinfo
    let modes = s:expand_modes(mi.mode, 2)
    if s:is_multi_modes(modes)
        let unmapcmds = s:collapse_modes(s:subtract_modes('nxso', modes))
        let mapcmd = ''
    else
        " Single-mode
        let mapcmd = s:collapse_modes(modes)
    endif
    " Build the *map command
    " TODO: Any advantage to using lhs we parsed from :*map, rather than the
    " one in maparg()?
    let cmd = mapcmd . (mi.noremap ? 'noremap' : 'map')
        \ . (mi.silent ? ' <silent>' : '')
        \ . (mi.expr ? ' <expr>' : '')
        \ . (mi.buffer ? ' <buffer>' : '')
        \ . (mi.nowait ? ' <nowait>' : '')
        \ . (mi.script ? ' <script>' : '')
        \ . ' ' . mi.lhs . ' ' . mi.rhs
    " Protect any Bar's within rhs.
    " Assumption: cpo& implies any literal `|' making it into rhs, must have
    " been escaped.
    let cmd = substitute(cmd, '|', '<Bar>', 'g')
    if mi.sid
        " Caveat: If the map was originally defined in a script
        " context, we need to replace <SID> in the map with the
        " proper script-specific identifier.
        " Note: Vim doesn't appear to support escaping <SID> in
        " rhs, so we don't either.
        let cmd = substitute(cmd,
            \ '<SID>', '<SNR>' . mi.sid . '_', 'g')
    endif
    if exists('unmapcmds')
        " Now handle any unmaps (required only after :map)
        " Assumption: This works only because multiple mode commands are
        " always processed *before* the single mode ones.
        let cmds = [cmd]
        for unmapcmd in split(unmapcmds, '\zs')
            call add(cmds, unmapcmd . 'unmap '
                \ . (mi.buffer ? '<buffer>' : '')
                \ . mi.lhs)
        endfor
        let cmd = join(cmds, '|')
    endif
    return cmd
endfunction

" Get list of dictionaries (keys: lhs, modes, plug) representing the sexp maps
" to be created, sorted by lhs.
function! s:get_sexp_maps(expert_mode)
    let sexp_maps = []
    for [plug, modestr] in s:plug_map_modes
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if lhs =~ '^\s*$'
            " Empty or all whitespace lhs disables map.
            continue
        endif
        if lhs[0:1] == '<>'
            " Design Decision: Strip a leading <> even if not expert mode.
            " TODO: Present non-fatal warning to user.
            let permanent = 1
            let lhs = lhs[2:]
        else
            let permanent = 0
        endif
        " Note: Need to canonicalize for subsequent comparisons.
        let lhs = join(s:split_and_canonicalize_lhs(lhs), '')
        " Note: permanent flag is needed only for expert mode, but might as
        " well set it unconditionally.
        call add(sexp_maps,
            \ {'lhs': lhs, 'permanent': !a:expert_mode || permanent,
            \ 'modes': s:expand_modes(modestr), 'plug': plug})
    endfor
    if !empty(sexp_maps)
        " Sort the sexp_maps list by lhs.
        call sort(sexp_maps, 's:by_lhs_sort_fn')
    endif
    return sexp_maps
endfunction

" Build commands for creating/deleting "sexp maps" upon entry/exit from sexp
" state, warning user about any lhs ambiguities.
function! s:build_sexp_map_cmds(sexp_maps, expert_mode)
    " TODO: Consider using map instead.
    if a:expert_mode
        " Order: OFF, ON, PERMANENT
        let cmds = [[], [], []]
    else
        let cmds = []
    endif
    let prev_sm = {}
    for sm in a:sexp_maps
        let [lhs, permanent, modes, plug] =
            \ [sm.lhs, sm.permanent, sm.modes, sm.plug]
        if !empty(prev_sm)
            " Warn user about any ambiguities.
            call s:check_ambiguity(prev_sm, sm, 1)
        endif
        for mode in split(modes, '\zs')
            if a:expert_mode && !permanent
                " Accumulate exit/entry commands.
                call add(cmds[0],
                    \ 'silent! ' . mode . 'unmap <buffer>' . lhs)
            endif
            call add(a:expert_mode ? permanent ? cmds[2] : cmds[1] : cmds,
                \ mode . 'map <nowait><silent><buffer>'
                \ . lhs . ' <Plug>(' . plug . ')')
        endfor
        let prev_sm = sm
    endfor
    " Return either single list (non-expert mode) or list of 3 lists.
    return cmds
endfunction

" Build commands for creating/deleting "escape maps" upon entry/exit from sexp
" state.
" Design Decision: Unless compelling reason is discovered, do not use :map
" command to combine modes, even when it's possible.
" Rationale: Would complicate things, and unlike the user map case (in which
" I'm endeavoring not to split up previously-defined user maps), there's no
" compelling motivation for it.
function! s:build_escape_map_cmds(esc_key, esc_maps)
    let cmds = [[], []]
    let esk_key_len = len(a:esc_key)
    for em in a:esc_maps
        " TODO: Perhaps use function capable of combining modes.
        for mode in split(em.modes, '\zs')
            " Create the escape map and the corresponding unmap.
            call add(cmds[1], mode . 'noremap <buffer><nowait> '
                \ . em.lhs . ' ' . em.lhs[esk_key_len:])
            " Record for subsequent removal.
            call add(cmds[0], mode . 'unmap <buffer> ' . em.lhs)
        endfor
    endfor
    return cmds
endfunction

" Build sorted list representing all esc maps that will be required for
" builtins.
function! s:get_escape_maps(esc_key, sexp_maps)
    let escs = {}
    for sm in a:sexp_maps
        let lhs = s:split_and_canonicalize_lhs(sm.lhs)[0]
        " Combine modes.
        let escs[lhs] = s:or_modes(get(escs, lhs, ''), sm.modes)
    endfor
    " Convert the hash (lhs=>modes) to a list of dicts sorted by lhs.
    let ret = []
    for lhs in sort(keys(escs))
        " Note: Prepending the esc key can't change sort order.
        call add(ret, {'lhs': a:esc_key . lhs, 'modes': escs[lhs]})
    endfor
    return ret
endfunction

" Pre Condition: Both lists sorted on lhs.
" TODO: Decide whether to modify in-place.
function! s:discard_conflicting_escape_maps(esc_maps, sexp_maps)
    let [i, n] = [0, len(a:esc_maps)]
    for sm in a:sexp_maps
        " Optimization: Sorted lists obviate need to restart iteration.
        while i < n
            let em = a:esc_maps[i]
            " TODO: Verify that both lhs's case is canonicalized.
            let cmp = s:compare_prefix(sm.lhs, em.lhs)
            if !cmp
                " Remove the esc map.
                " TODO: Decide whether to make it immutable.
                call remove(a:esc_maps, i)
                " Caveat: Skip i increment and record fact that there's one
                " fewer element.
                let n -= 1
                " TODO NOTE: This wouldn't be necessary if we made this
                " function non-mutating and simply accumulated.
                continue
            elseif cmp < 0
                " Short-circuit inner loop.
                break
            endif
            let i += 1
        endwhile
    endfor
    return a:esc_maps
endfunction

" Merge the sorted input lists of dictionaries representing escape and sexp
" maps.
function! s:combine_escape_and_sexp_maps(esc_maps, sexp_maps)
    let ret = []
    let [i, n] = [0, len(a:esc_maps)]
    for sm in a:sexp_maps
        " Optimization: Sorted lists obviate need to restart iteration.
        while i < n
            let em = a:esc_maps[i]
            if em.lhs <= sm.lhs
                call add(ret, em)
            else
                call add(ret, sm)
                break
            endif
            let i += 1
        endwhile
    endfor
    return ret
endfunction

" Parse output of :map command (using <buffer> arg if and only if buffer arg
" is nonzero), and return relevant results in a list whose elements have the
" following format:
"   {'lhs': <lhs>, 'modes': <modes>, 'script': 0|1}
" Note: Deferring running maparg() till we know it's needed.
" Order: by lhs (case-sensitive)
" TODO: Could simply augment maparg() dict itself with 'script' flag.
function! s:get_user_maps(esc_key, global)
    let ret = []
    redir => maps
    " Note: Get nvo maps.
    " TODO: Discard modes we don't care about.
    silent exe 'map ' . (a:global ? '' : '<buffer>') . ' ' . a:esc_key
    redir END
    for map in split(maps, '\n')
        " Extract lhs and everything else (which may be needed later).
        " Assumptions: (from map.txt)
        " -First 2 columns dedicated to mode flags (typically only <Space> or
        "  single mode flag, but with :map followed by ounmap, you can get
        "  'nv'.
        " -Whitespace ends lhs (spaces, etc. will be encoded with <...>)
        " Mode Flags: The following are significant to us:
        "   <Space> (i.e., nvo, created with :map), n, v, x, o
        " Scope Flag Ambiguity: The scope flag field is...
        "   global: [&*]\?
        "   buffer: [&*]\?@
        " The problem is that any of those characters can also appear
        " unescaped at start of rhs: hence, we have ambiguity that needs
        " to be resolved by considering both the presence/absence of
        " <buffer> arg to :map command and the "noremap" flag in the
        " maparg() dict returned by maparg() for extracted lhs.
        " Note: Though doc doesn't make it clear, maparg() dict can also
        " contain 2-char string 'nv'.
        let [_, modes, lhs, scope_and_rhs; rest] = matchlist(map,
            \ '\(.\{-}\)\%>2c\s\+\(\S\+\)\s\+\(.*\)')
        " Is this map relevant? We care only about <Space> (nvo) and nvxo.
        " Note: maparg() accepts empty string for nvo, but returns ' ' in
        " its dict for the same. Let's just use the latter, as it seems to
        " be more common, and will work the same when prepended to "map".
        " Alternative: Could also convert to 'nvo'
        if modes == ' ' || modes =~ '[xvno]'
            " Canonicalize the lhs
            let lhs = join(s:split_and_canonicalize_lhs(lhs), '')
            if !empty(lhs)
                " Get map info needed to disambiguate <script>
                " TODO: Given that we have to run this to disambiguate
                " <script>, perhaps we should go ahead and save.
                " OTOH, we have no idea whether this one will be needed yet.
                let ma = maparg(lhs, modes, 0, 1)
                " Make sure our canonicalization succeeded.
                " Note: This should work for 99+% of use cases, but :map output is
                " not deterministic, so there could be some pathological
                " corner cases.
                if !empty(ma)
                    " Now use "noremap" key to disambiguate scope field in
                    " :map output.
                    " Assumption: rhs cannot begin with whitespace.
                    let [_, scope, _, rhs; rest] = matchlist(scope_and_rhs,
                        \ (ma.noremap ? '\([&*]\)' : '\(\)')
                        \.(!a:global ? '\(@\)' : '\(\)')
                        \.'\s*\(.*\)')
                    " Determine <script> was used. If so, noremap is
                    " implied.
                    let script = scope == '&'
                    " Add to buffer/global-specific list.
                    " Note: Save just what we need till we're sure it's
                    " safe to call maparg (which for globals, will be
                    " after unmapping any identical buf-locals)
                    call add(ret,
                        \ {'lhs': lhs,
                        \  'modes': s:expand_modes(modes, 1),
                        \  'script': script})
                endif
            endif
        endif
    endfor
    " Sort and combine.
    call sort(ret, "s:usermap_sort_fn")
    return ret
endfunction

" Accept esc_key and a list of hashes (keys: lhs, modes), and return lists of
" exit/entry commands needed to delete/restore conflicting user maps.
" Inputs:
"   esk_key: single key designator (in fiducial form)
"   maps:    [{'lhs1': <>, 'modes': <>}, ...]
" Return: [exit-map-cmds, entry-map-cmds]
" Pre Condition: sexp and esc maps have been combined into a single list
" sorted by lhs.
" Rationale: Prevents problems with (albeit unlikely) scenarios such as the
" following, in which distinct, non-ambiguous sexp and esc maps are shadowed
" by the same user map, potentially in different modes.
"   user map _
"   sexp map _g
"   esc map  _h
function! s:shadow_conflicting_usermaps(esc_key, maps)
    " Initialize return list: [exit-map-cmds, entry-map-cmds]
    let cmds = [[], []]
    " Loop over provided list of hashes (keys: lhs, modes)
    " Optimization: Resetting i only here because both maps and umaps
    " are sorted.
    let [prev_ulhs, i, n] = ['', 0, len(a:maps)]
    " Loop over buffer user maps.
    " Assumption: Global maps don't matter: we won't overwrite them, and
    " they can't shadow the buffer maps we create.
    for umap in s:get_user_maps(a:esc_key, 0)
        " Extract some key values for convenience.
        " Note: umap.modes is an expanded value - not from maparg().
        let [ulhs, umodes, script] = [umap.lhs, umap.modes, umap.script]
        if prev_ulhs != ulhs
            " Starting new range of user maps with common lhs.
            " Assumption: For a given ulhs, multi-mode command will be
            " first in range or not at all.
            let [prev_ulhs, first_ulhs, force_range] = [ulhs, 1, 0]
            " Record index at which map search will start for each umap in a
            " range of umaps with identical lhs.
            let i_first = i
        else
            let first_ulhs = 0
        endif
        " Accumulate overlap between this umap and any maps it shadows.
        let omodes = ''
        " Start with first map that *could* conflict with current user
        " map, short-circuiting when we pass the last one that could,
        " or when all modes in the umap have been flagged.
        let i = i_first
        while i < n
            let map = a:maps[i]
            let [lhs, modes] = [map.lhs, map.modes]
            if s:is_prefix_of(ulhs, lhs)
                " User map would shadow the sexp/esc map. Accumulate any mode
                " overlap.
                let omodes = s:or_modes(omodes, s:and_modes(umodes, modes))
                " Note: Because and/or_modes sort the flags, could also
                " test omodes and modes for equality.
                if len(omodes) == len(umodes)
                    " No point in continuing to check maps now that
                    " we've flagged all modes.
                    break
                endif
            elseif ulhs < lhs
                " Both inner and outer lists are sorted by lhs, so we can
                " short-circuit and restart this loop at i_first after
                " moving to the subsequent umap.
                break
            endif
            let i += 1
        endwhile
        " Does this umap require delete/restore?
        if !empty(omodes) || force_range
            " Determine whether subsequent umaps with same lhs will
            " need restore, if only because of the :map command used
            " to restore the head umap in range.
            " Note: umaps that aren't first in range can't be multi-mode.
            let force_range = force_range ||
                \ (first_ulhs && s:is_multi_modes(umodes))
            let umap = maparg(ulhs, umodes, 0, 1)
            let umap.script = script
            " Add to lists for subsequent delete/restore.
            " Note: Don't delete maps that are being restored only because of
            " use of :map command on head map in range.
            if !empty(omodes)
                " Delete only the overlapping modes.
                call add(cmds[1], s:build_delete_cmds(umap, omodes))
            endif
            " 2 kinds of restore:
            " 1. maps deleted upon entry
            " 2. maps deleted by :map command used to restore
            "    a multi-mode map upon exit.
            call add(cmds[0], s:build_restore_cmds(umap))
        endif
    endfor
    return cmds
endfunction

" Note: This is probably the only global function we'll define.
function! VimSexpState()
    return exists('b:sexp_expert_mode') && b:sexp_expert_mode &&
        \ exists('b:sexp_state_enabled')
        \ ? b:sexp_state_enabled ? '[VS:ON]' : '[VS:off]'
        \ : ''
endfunction

function! s:has_stl_placeholder(stl)
    " Pattern for unescaped percent
    return a:stl =~ s:re_unesc_pct . '{VimSexpState()}'
endfunction

function! s:get_optimal_stl(stl)
    let idiv = match(a:stl, s:re_unesc_pct . '=')
    if idiv >= 0
        let sides = [a:stl[:idiv-1], a:stl[idiv:]]
    else
        let sides = [a:stl]
    endif
    " Look for spot just after lhs (or only side) flags, or just before rhs flags.
    let re_flag = '\c[mrhwy]'
    let iright = 0
    for iright in range(idiv >= 0 ? 2 : 1)
        let re = (!iright ? '.*' : '')
            \ . s:re_unesc_pct . re_flag
            \ . (!iright ? '\zs' : '')
        let i = match(sides[iright], re)
        if i >= 0
            if iright
                let i += idiv
            endif
            break
        endif
        let iright += 1
    endfor
    if i < 0
        " Still no valid location found. Place either at head of right side,
        " or at end.
        let i = idiv > 0 ? idiv + 2 : len(a:stl)
    endif

    return a:stl[:i-1] . "%{VimSexpState()}" . a:stl[i:]
endfunction

function! s:add_sexp_state_to_stl()
    " Check to see whether user has put a %{VimSexpState()} flag in his 'stl'.
    let stl = !empty(&l:stl) ? &l:stl : !empty(&g:stl) ? g:stl : ''
    if empty(stl)
        " Start with default.
        let stl = '%<%f %h%m%r%=%-14.(%l,%c%V%) %P'
    endif
    if !empty(stl)
        if s:has_stl_placeholder(stl)
            return
        else
            " Look for a good spot.
            let stl = s:get_optimal_stl(stl)
        endif
    endif
    let &l:stl = stl
endfunction

function! s:display_sexp_state(on)
    " Force update of statusline.
    " TODO: Will this do it ro flag not in 'stl'?
    let &ro = &ro
endfunction

" TODO: Eventually allow this to be list of toggles, or else support
" key-chords some other way.
function! s:get_sexp_state_toggle()
    " TODO: Needs to default
    return get(g:, 'sexp_state_toggle', s:sexp_state_toggle)
endfunction

function! s:get_sexp_escape_key()
    return get(g:, 'sexp_escape_key', '')
endfunction

function! s:sexp_create_non_insert_mappings()
    let b:sexp_expert_mode = 0
    let maps = s:get_sexp_maps(0)
    for cmd in s:build_sexp_map_cmds(maps, 0)
        exe cmd
    endfor
endfunction

function! s:sexp_toggle_non_insert_mappings()
    let b:sexp_expert_mode = 1
    " Note: First toggle leaves us in non-sexp-state (OFF), defining only the
    " permanent mappings.
    let enabled = exists('b:sexp_state_enabled') && !b:sexp_state_enabled
    let first_time = !exists('b:sexp_state_enabled')
    if first_time
        " First time toggle ON for this buffer! Build and cache data
        " structures for future use.
        " TODO: Perhaps some validation: e.g., single key.
        " TODO: Should we permit stuff like <LocalLeader>? If so, need to
        " canonicalize case...
        let esc_key = s:get_sexp_escape_key()

        let sexp_maps = s:get_sexp_maps(1)
        "echomsg "sexp_maps: " . string(sexp_maps)
        let b:sexp_map_commands = s:build_sexp_map_cmds(sexp_maps, 1)
        "echomsg "SM Exit: " . string(b:sexp_map_commands[0])
        "echomsg "SM Enter: " . string(b:sexp_map_commands[1])
        if !empty(esc_key)
            " Get list of escape maps we'll need to create for builtins.
            let esc_maps = s:get_escape_maps(esc_key, sexp_maps)
            "echomsg "esc_maps 1 :" . string(esc_maps)
            " Discard esc maps that would conflict with sexp maps.
            " User probably should probably be warned not to do this in docs.
            " Is run-time warning appropriate? (TODO)
            let esc_maps =
                \ s:discard_conflicting_escape_maps(esc_maps, sexp_maps)
            " Combine sexp and esc maps into single, sorted list for
            " shadow_conflicting_usermaps.
            let sexp_maps =
                \ s:combine_escape_and_sexp_maps(esc_maps, sexp_maps)
            "echomsg "esc_maps 2 :" . string(esc_maps)
            " Create the lists of escape map commands.
            let b:escape_map_commands = s:build_escape_map_cmds(esc_key, esc_maps)
            "echomsg "EM Exit esc: " . string(b:escape_map_commands[0])
            "echomsg "EM Enter esc: " . string(b:escape_map_commands[1])
        endif
        " Build the commands to save/restore any conflicting user maps.
        " Note: This needs to be run even if user hasn't configured escape
        " key (in which case, escape/sexp maps will not have been merged).
        let b:user_map_commands =
            \ s:shadow_conflicting_usermaps(esc_key, sexp_maps)
        "echomsg "UM Exit esc: " . string(b:user_map_commands[0])
        "echomsg "UM Enter esc: " . string(b:user_map_commands[1])
    endif
    " Record state change.
    let b:sexp_state_enabled = enabled
    if first_time
        " Create the always-active commands once-only.
        for cmd in b:sexp_map_commands[2]
            exe cmd
        endfor
    else
        " Perform entry/exit using buf-locally cached map commands.
        " Caveat: Order in which cmd groups are processed is significant: e.g.,
        " must delete user maps first on entry and restore them last on exit.
        let cmd_groups = [
            \ b:user_map_commands,
            \ b:sexp_map_commands,
            \ exists('b:escape_map_commands') ? b:escape_map_commands : []]
        for cmds in enabled ? cmd_groups : reverse(cmd_groups)
            if empty(cmds)
                " Skip either esc maps or user maps.
                continue
            endif
            "echomsg "Enabled: " . enabled
            for cmd in cmds[enabled]
                "echomsg cmd
                exe cmd
            endfor
        endfor
    endif

    " Display visual indication of state.
    call s:display_sexp_state(enabled)
    " Note: Since toggle is meant to be always active, there's no need to
    " worry about conflict with existing user maps or global maps.
    call s:create_sexp_state_toggle()
endfunction

" Toggle between 'special' and non-special modes.
function! s:toggle_sexp_state(...)
    let mode = a:0 ? a:1 : ''
    call s:sexp_toggle_non_insert_mappings()
    if mode == 'v'
        normal! gv
    endif
endfunction

function! s:on_file_type()
    " TODO: Perhaps eliminate vim_sexp_loaded, using sexp_expert_mode, or
    " something else that has to be created anyways.
    if !exists('b:vim_sexp_loaded')
        let b:vim_sexp_loaded = 1
        call s:sexp_create_mappings()
    endif
endfunction

function! s:is_sexp_expert_mode_buffer()
    return getbufvar('%', 'sexp_expert_mode', -1) == 1
endfunction

" Buffer Number Designations: % is new buffer; <abuf> is buf being left.
" Assumption: Called only for sexp expert mode buffers.
function! s:OnBufWinLeave(...)
    " Vim Idiosyncrasy: expand() returns a string, and since setbufvar() can
    " take one, there's no automatic conversion, which means a bufnr will be
    " interpreted as name if not forced to int.
    let bnr = 0 + expand('<abuf>')
    if bnr < 0
        " Note: Vim doesn't document it, but I'm pretty sure I've seen on-exit
        " invocations in which <abuf> is already invalid at this point.
        return
    endif
    " Remove this buffer from the map.
    call remove(s:loaded_buffer_map, bnr)
    if empty(s:loaded_buffer_map)
        if exists('s:laststatus_save')
            if &laststatus == 2
                " Restore the 'laststatus' setting in effect when the first
                " sexp expert mode buffer was loaded.
                let &laststatus = s:laststatus_save
            endif
            unlet! s:laststatus_save
        endif
    endif
    if getbufvar(bnr, 'sexp_state_enabled', 0)
        " Experimental: Mark a buffer for toggle to non-sexp state whenever it's
        " loaded.
        call setbufvar(bnr, 'sexp_need_toggle_off', 1)
    endif
endfunction

function! s:OnBufWinEnter()
    if s:is_sexp_expert_mode_buffer()
        " 'laststatus' logic
        let s:loaded_buffer_map[bufnr('%')] = 1
        if &laststatus != 2
            let s:laststatus_save = &laststatus
            set laststatus=2
        endif
        " Experimental: If we deferred toggling off in BufWinLeave handler,
        " toggle off now.
        if exists('b:sexp_need_toggle_off') && b:sexp_need_toggle_off
            " TODO: Perhaps use lower-level function, and possibly add an
            " argument to force desired state.
            call s:toggle_sexp_state()
        endif
    endif
endfunction

" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
function! s:sexp_create_mappings()
    if !exists('g:sexp_expert_mode') || !g:sexp_expert_mode
        call s:sexp_create_non_insert_mappings()
    else
        " Question: Should we ignore file_type event? For expert mode only, or
        " both cases? If not, we'd need to take care, since if we're in sexp
        " state, we'd probably need to toggle out (to restore user maps)
        " before doing anything else. But is there any reason not to ignore?
        " Note: Might want ability to toggle everything off for handling
        " BufWinLeave.
        call s:sexp_toggle_non_insert_mappings()
        augroup sexp_expert_mode
            au!
            au BufWinEnter * call s:OnBufWinEnter()
            au BufWinLeave <buffer> call s:OnBufWinLeave()
            let s:loaded_buffer_map = {bufnr('%'): 1}
        augroup END
        " Jumpstart the mechanism.
        " TODO: !!!!!!! UNDER CONSTRUCTION !!!!!!!
        call s:OnBufWinEnter()
        call s:add_sexp_state_to_stl()
    endif
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
