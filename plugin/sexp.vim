
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

let s:builtins = {
    \ 'nvo': [
        \ '<BS>', '<C-H>', '<Tab>', '<C-I>', '<NL>',
        \ '<C-H>', '<Tab>', '<C-I>', '<NL>', '<C-J>', '<CR>', '<C-M>',
        \ '<C-N>', '<C-P>', '<C-R>', '<C-Z>',
        \ '<Space>', '!',
        \ '#', '$', '%', '&',
        \ {
            \ 'key': '''',
            \ 'children': [
                \ {'fn': 's:key_ranges', 'args': [['a', 'z'], ['A', 'Z'], ['0', '9']]}
                \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
            \ ]
        \ },
        \ '(', ')', '*', '+', ',', '-', '/', '<CR>',
        \ '0', ':', ';', '?',
        \ 'B', 'E', 'F', 'G', 'H', 'L', 'M', 'N', 'T', 'W', '^', '_',
        \ {
            \ 'key': '`',
            \ 'children': [
                \ {'fn': 's:key_ranges', 'args': [['a', 'z'], ['A', 'Z'], ['0', '9']]}
                \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
            \ ]
        \ },
        \ 'b', 'e', 'f',
        \ {
            \ 'key': 'g',
            \ 'children': [
                \ '#', '$',
                \ {
                    \ 'key': '''',
                    \ 'children': [
                        \ {'fn': 's:key_ranges', 'args': [['a', 'z'], ['A', 'Z'], ['0', '9']]}
                        \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
                    \ ]
                \ },
                \ {
                    \ 'key': '`',
                    \ 'children': [
                        \ {'fn': 's:key_ranges', 'args': [['a', 'z'], ['A', 'Z'], ['0', '9']]}
                        \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
                    \ ]
                \ },
                \ '*', '0', 'D', 'E', 'N', '^', '_'
            \ ]
        \ },
        \ 'h', 'j', 'k', 'l', 'n', 't', 'w', '{', '|', '}',
        \ '<C-End>', '<C-Home>', '<C-Left>', '<C-Right>', '<Down>', '<End>', '<Home>',
        \ '<Left>', '<LeftMouse>', '<Right>', '<RightMouse>',
        \ '<S-Left>', '<S-LeftMouse>', '<S-Right>', '<S-RightMouse>', '<Up>',
        \ {
            \ 'key': 'a',
            \ 'children': [
                \ '"', "'", '(', ')', '<lt>', '>', 'B', 'W', '[', ']', '`',
                \ 'b', 'p', 's', 't', 'w', '{', '}'
            \ ]
        \ },
        \ {
            \ 'key': 'i',
            \ 'children': [
                \ '"', "'", '(', ')', '<lt>', '>', 'B', 'W', '[', ']', '`',
                \ 'b', 'p', 's', 't', 'w', '{', '}'
            \ ]
        \ }
    \ ],
    \ 'nv': [
        \ '<C-A>', '<C-D>', '<C-E>', '<C-F>',
        \ '<NL>', '<C-J>', '<C-L>',
        \ '<C-Q>', '<C-U>', '<C-V>',
        \ '<C-X>', '<C-Y>',
        \ '<', '=', '>', '@',
        \ 'A', 'C', 'D', 'I', 'J', 'K', 'O', 'P', 'R', 'S', 'V', 'X', 'Y'
        \ {
            \ 'key': '[',
            \ 'children': [ ]
        \ },
        \ {
            \ 'key': ']',
            \ 'children': [ ]
        \ },
        \ 'a', 'c', 'd',
        \ {
            \ 'key': 'g',
            \ 'children': [
                \ '8', '?', 'I', 'J', 'P', 'T', 'U', ']'
            \ ]
        \ },
        \  'i', 'm', 'o', 'p',
        \ {
            \ 'key': 'q',
            \ 'children': [
                \ {'fn': 's:key_ranges',
                \  'args': [['a', 'z'], ['A', 'Z'], ['0', '9']]},
                \ '"', ':', '/', '?'
            \ ]
        \ },
        \ 'r', 's', 'v', 'x', 'y',
        \ {
            \ 'key': 'z',
            \ 'children': [
            \ ]
        \ },
        \ '~',
        \ '<Del>', '<MiddleMouse>', '<PageDown>', '<PageUp>', '<S-Up>'
    \ ],
    \ 'n': [
        \ '<C-O>', '<C-G>', '<C-T>', '.', 'Q', 'u',  '<Insert>', '<F1>'
        \ {
            \ 'key': 'Z',
            \ 'children': ['Z', 'Q']
        \ },
        \ {
            \ 'key': '<C-\>',
            \ 'children': [
                \ '<C-N>', '<C-G>',
                \ {'fn': 's:key_ranges', 'args': [['a', 'z']]}
            \ ]
        \ },
        \ {
            \ 'key': '<C-W>',
            \ 'children': [
                \ '<C-B>', '<C-C>', '<C-D>', '<C-F>', '<C-G>', '<C-H>', '<C-H>', '<C-I>',
                \ '<C-J>', '<C-K>', '<C-L>', '<C-N>', '<C-O>', '<C-P>', '<C-Q>', '<C-R>',
                \ '<C-S>', '<C-T>', '<C-V>', '<C-W>', '<C-X>', '<C-Z>', '<C-]>', '<C-^>', '<C-_>',
                \ '+', '-', '<', '=', '>', 'H', 'J', 'K', 'L', 'P', 'R', 'S', 'T', 'W', ']', '^',
                \ '_', 'b', 'c', 'd', 'f', 'F',
                \ {
                    \ 'key': 'g',
                    \ 'children': [
                        \ '<C-]>', ']', '}', 'f', 'F'
                    \ ]
                \ },
                \ {'fn': 's:key_ranges',
                \  'args': [['h', 'l'], ['n', 'x'], ['z', 'z']]},
                \ '<Bar>',
                \ '}',
                \ '<Down>',
                \ '<Up>',
                \ '<Left>',
                \ '<Right>',
            \ ]
        \ },
        \ {
            \ 'key': 'g',
            \ 'children': [
                \ '+', ',', '-', ';', '<', 'H', 'Q', 'R'
            \ ]
        \ },
    \ ],
    \ 'v': [
        \ {
            \ 'key': '<C-\>',
            \ 'children': [
                \ '<C-N>', '<C-G>',
                \ {'fn': 's:key_ranges', 'args': [['a', 'z']]}
            \ ]
        \ },
        \ '<C-C>', '<C-G>', '<Esc>', '<C-]>',
        \ '!', ':', '<lt>', '=', '>', 'C', 'D', 'J', 'K',
        \ 'O', 'R', 'S', 'U', 'V', 'X', 'Y', 'c', 'd',
        \ {
            \ 'key': 'g',
            \ 'children': [
                \ 'J', 'q', 'v'
            \ ]
        \ },
        \ 'o', 'r', 's', 'u', 'v', 'x', 'y', '~'
    \ ]
\ }

let s:re_key_notation = '\c\v^\<'
    \ . '%(t_)@!'
    \ . '%([SCMAD]-)*'
    \ . '%('
    \ . 'nul|bs|tab|nl|ff|cr|return|enter|esc|space|lt|bslash|bar|del|x?csi'
    \ . '|eol|up|down|left|right|f%(10|11|12|[1-9])|help|undo|insert'
    \ . '|home|end|pageup|pagedown|k%(home|end|page%(up|down)|plus|minus'
    \ . '|multiply|divide|enter|point|[0-9])'
    \ . '|[[:print:]])\>$'

if !empty(g:sexp_filetypes)
    augroup sexp_filetypes
        autocmd!
        execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:on_buf_load()'
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

function! s:create_sexp_state_toggle()
    for mode in ['n', 'x']
        execute mode . 'noremap <buffer><nowait> '
            \ . g:sexp_toggle_map
            \ . ' <Esc>:<C-u>call <SID>toggle_sexp_state('
            \ . (mode == "n" ? "'n'" : "'v'")
            \ . ')<CR>'
    endfor
endfunction

" Split a somewhat canonicalized lhs string into a list of even more
" canonicalized pieces: e.g.,
"   "<foo>" => ["<lt>", "f", "o", "o", ">"]
"   BUT
"   "<C-A>" => ["<C-A>"]
" Important CAVEAT: Because we're doing string comparisons on canonicalized
" forms (and case is generally sensitive in maps), we need either to
" canonicalize case here or use "\<...>" to convert to actual byte sequence:
" e.g., "\<F7>" => <80>k7
" I don't see this causing problems, since these canonical forms would be used
" only for in memory comparisons.
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
        let ie = matchend(s, '^\c<[-[:alnum:]]\+>', i)
        if ie >= 0
            let c = s[i:ie-1]
            " Check for something that *could* be special key notation.
            " TODO: Consider canonicalizing by using the "\<...>" notation in
            " a string expression to produce an actual key sequence (or
            " something beginning with '<' if it's not a valid key sequence).
            " Note: This strategy could be used to perform more reliable
            " validation than what's provided by s:re_key_notation.
            if c !~ s:re_key_notation
                let c = '<LT>'
                " Let the rest be handled on subsequent iterations.
                let ie = i + 1
            endif
        else
            " Definitely not key notation.
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
            endif
        endif
        call add(ret, c)
        let i = ie
    endwhile
    return ret
endfunction

" Build commands for creating/deleting "escape maps" upon entry/exit from sexp
" state.
" Design Decision: Do not use :map command to combine modes, even when it's
" possible.
" Rationale: Would complicate things considerably, and unlike the user map
" case, there's no compelling motivation for it.
function! s:build_escape_map_cmds(esc_key, esc_maps)
    let cmds = [[], []]
    let esk_key_len = len(a:esc_key)
    for em in a:esc_maps
        " TODO: Perhaps use function capable of combining modes.
        for mode in split(em.modes, '\zs')
            " Create the escape map and the corresponding unmap.
            " TODO: Decide whether stripping off esk key here is better than
            " prepending it here (keeping in mind that some logic further
            " upstream needs it to be part of the string).
            call add(cmds[1], mode . 'noremap <buffer><nowait> '
                \ . em.lhs . ' ' . em.lhs[esk_key_len:])
            " Record for subsequent removal.
            call add(cmds[0], mode . 'unmap <buffer> ' . em.lhs)
        endfor
    endfor
    return cmds
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

function! s:motions()
    " TODO: Add square bracket commands.
    if !exists('s:motion_builtins')
        let s:motion_builtins = [
            \ '<BS>', '<C-H>', '<NL>', '<C-J>', '<CR>', '<C-M>', '<C-N>', '<C-P>',
            \ '<Space>',
            \ '#', '$', '%',
            \ {
                \ 'key': '''',
                \ 'children': [
                    \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
                \ ]
            \ },
            \ '(', ')', '*', '+', ',', '-', '/', '<CR>', '0', ':', ';', '?',
            \ 'B', 'E', 'F', 'G', 'H', 'L', 'M', 'N', 'T', 'W', '^', '_',
            \ {
                \ 'key': '`',
                \ 'children': [
                    \ {'fn': 's:key_ranges', 'args':  [['a', 'z'], ['A', 'Z'], ['0', '9']]},
                    \ '(', ')', '<', '>', '[', ']', '`', '{', '}'
                \ ]
            \ },
            \ 'b', 'e', 'f', 'h', 'j', 'k', 'l', 'n', 't', 'w', '{', '<Bar>', '}',
            \ '<C-End>', '<C-Home>', '<C-Left>', '<C-Right>', '<Down>', '<End>',
            \ '<Home>', '<Left>', '<LeftMouse>', '<Right>', '<S-Down>', '<S-Left>',
            \ '<S-Right>', '<S-Up>', '<Up>',
            \ {'fn': 's:text_objects'}
        \ ]
    endif
    return s:motion_builtins
endfunction

function! s:text_objects()
    if !exists('s:text_object_builtins')
        let children = [
            \ '"', "'", '(', ')', '<lt>', '>', 'B', 'W', '[', ']', '`',
            \ 'b', 'p', 's', 't', 'w', '{', '}']
        let s:text_object_builtins = [
            \ {
                \ 'key': 'a',
                \ 'children': children
            \ },
            \ {
                \ 'key': 'i',
                \ 'children': children
            \ }
        \ ]
    endif
    return s:text_object_builtins
endfunction

function! s:key_ranges(...)
    let ret = []
    " Loop over all supplied ranges.
    for r in a:000
        " Loop over characters in range.
        for c in range(char2nr(r[0]), char2nr(r[1]))
            call add(ret, nr2char(c))
        endfor
    endfor
    return ret
endfu

" Recursively build a tree-like builtins data structure optimized for search.
" mode    applicable mode
" keylist list of keys in canonical form at current level, with lower levels
"         represented by maps: e.g.,
"         <key1>, <key2>, {'key': <key3>, 'children': [...]}, ...
" Output: Builtins represented as search-optimized tree structure, of the
" following form:
" {<key1>: {'modes', '[nv]', 'dmodes': '[nv]', 'children': {...}},
"  <key2>: ...}
" Note: Although represented as separate hierarchies in the non-optimized
" builtins structure, the modes are merged into a common tree structure here,
" with the "modes" key indicating the modes for which element is a leaf, and
" the "dmodes" key indicating which modes are represented at deeper levels (to
" permit search short-circuit).
function! s:add_builtin_r(mode, keylist, map)
    for key in a:keylist
        if type(key) == 4
            if has_key(key, 'fn')
                " Use recursive call to splice keylist returned by named
                " function (possibly called with args) into current map.
                call s:add_builtin_r(a:mode,
                    \ call(key.fn, has_key(key, 'args') ? key.args : []),
                    \ a:map)
                " All keys returned by fn have been fully processed.
                continue
            else
                " This key has children.
                let [k, descend] = [key.key, 1]
            endif
        else
            let [k, descend] = [key, 0]
        endif
        " Canonicalize key
        " TODO: If this call returns more than 1 element, we have internal
        " error. Perhaps refactor...
        let k = s:split_and_canonicalize_lhs(k)[0]
        " TODO: Consider adding a flag that would permit having children but
        " also serving as leaf.
        " Rationale: For stuff like normal mode `d', where we might not want
        " to force user to hit the motion before timeout: e.g., we might want
        " <EscKey>d to trigger the escape after timeout.
        if !has_key(a:map, k)
            let a:map[k] = {'modes': '', 'dmodes': '', 'children': {}}
        endif
        let map = a:map[k]
        if descend
            " Facilitate short-circuiting searches when applicable mode(s) not
            " represented below this level.
            let map.dmodes = s:or_modes(map.dmodes, a:mode)
            call s:add_builtin_r(a:mode, key.children, map.children)
        else
            let map.modes = s:or_modes(map.modes, a:mode)
        endif
    endfor
endfunction

" Return a cached map of builtins, optimized for recursive search.
" Note: Generate the map on initial call.
" TODO: Perhaps hide the actual s:builtins data structure in here, and the
" function itself at the bottom of the file...
function! s:get_builtins()
    if exists('s:builtins_optimized')
        return s:builtins
    endif
    let keymap = {}
    " Need to do the first-time only conversion.
    for [mode, keylist] in items(s:builtins)
        call s:add_builtin_r(s:expand_modes(mode), keylist, keymap)
    endfor
    " Replace the original with one optimized for search.
    let s:builtins = keymap
    let s:builtins_optimized = 1
    let g:builtins = keymap
    return s:builtins
endfunction

" Process the "builtins" tree structure recursively, recording in a escs map
" all those builtins whose ambiguity/conflict with a sexp-state map
" necessitates an escape map.
" builtins  current location in builtins tree structure (implemented as map).
" blhs      cumulative builtin lhs
" lhs_lst   list of unprocessed lhs canonicalized key components
" modes     modes in which the sexp-state map being checked is active.
" escs      detected conflicts (key=lhs, val=modes)
function! s:get_escape_maps_r(builtins, blhs, lhs_lst, modes, escs)
    if empty(a:lhs_lst)
        " Note: Can't get here on first recursive call.
        " Process all leaves, which are assumed to be descended from a
        " conflicting sexp map.
        let lhs = ''
        let bmap = a:builtins
    else
        " Process only conflicting key and any of its descendants.
        let [lhs, lhs_lst] = [a:lhs_lst[0], a:lhs_lst[1:]]
        " Note: Stuffing into a single key hash permits common logic below.
        let bmap = has_key(a:builtins, lhs) ? {lhs: a:builtins[lhs]} : {}
    endif
    " Process any leaf keys at this level (indicated by non-empty modes).
    for [k, o] in items(bmap)
        let blhs = a:blhs . k
        " Loop over leaf builtins (if any).
        " Accumulate modes in which current map conflicts with builtin.
        " Caveat: Don't add key if nothing to accumulate.
        let omodes = s:and_modes(o.modes, a:modes)
        if !empty(omodes)
            let a:escs[blhs] = s:or_modes(get(a:escs, blhs, ''), omodes)
        endif

        " Descend if there are builtin children *and* the modes we're
        " interested in are represented at lower levels (dmodes).
        " Possible TODO: There are other short-circuit possibilities: e.g.,
        " could modify dmodes to reflect current state of conflict detection,
        " but this is probably not justified, given that the fraction of
        " traversals ending in short-circuit would be extremely small.
        if !empty(o.children) && o.dmodes =~ '[' . a:modes . ']'
            call s:get_escape_maps_r(o.children, blhs, lhs_lst,
                \ a:modes, a:escs)
        endif
    endfor
endfunction

" Build sorted list representing all esc maps that will be required for
" builtins.
function! s:get_escape_maps(esc_key, sexp_maps)
    let builtins = s:get_builtins()
    let escs = {}
    for sm in a:sexp_maps
        let lhs_lst = s:split_and_canonicalize_lhs(sm.lhs)
        if !empty(lhs_lst)
            " Recurse...
            call s:get_escape_maps_r(builtins, '', lhs_lst, sm.modes, escs)
        endif
    endfor
    " Convert the hash (lhs=>modes) to a list of dicts sorted by lhs.
    let ret = []
    " TODO: Consider deferring the sort to discard_conflicting_escape_maps.
    for lhs in sort(keys(escs))
        " Note: Prepending the esc key can't change sort order.
        call add(ret, {'lhs': a:esc_key . lhs, 'modes': escs[lhs]})
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

" Accept esc_key and any number of lists of hash (keys: lhs, modes), and
" return lists of exit/entry commands needed to save/restore conflicting user
" maps.
" Inputs:
"   esk_key: 
"   [[{'lhs1': <>, 'modes': <>}, ...], ...]
" Return: [exit-map-cmds, entry-map-cmds]
" CAVEAT: Unlike some of the other "builder" functions, this one must actually
" perform some map removal to prevent global user maps from being shadowed by
" the buf-locals.
" TODO: Rework this comment - much has changed!!!!!
" TODO: Consider advantages of having multiple maps passed in, as opposed to
" multiple calls... Should never have more than sexp_maps and (optional)
" esc_maps. Should this be hardcoded in parameter list?
function! s:shadow_conflicting_usermaps(esc_key, ...)
    " Initialize return list: [exit-map-cmds, entry-map-cmds]
    let cmds = [[], []]
    " Loop over provided lists (e.g., sexp, esc) of hashes (keys: lhs, modes)
    " Assumption: Lists can be processed independently because we've already
    " precluded possibility of ambiguous lhs's.
    for maps in a:000
        " Loop over [buffer, global], calling s:get_user_maps for each.
        " Caveat: s:get_user_maps won't work properly for globals if we
        " haven't already removed the conflicting buf-locals.
        " Assumption: Caller has ensured orthogonality of the input lists, so
        " we're safe to process them independently as outermost loop.
        for global in range(2)
            let prev_ulhs = ''
            " Optimization: Resetting i only here because both maps and umaps
            " are sorted.
            let [i, n] = [0, len(maps)]
            " Loop over buffer or global user maps.
            for umap in s:get_user_maps(a:esc_key, global)
                " Extract some key values for convenience.
                " Note: umap.modes is an expanded value - not from maparg().
                let [ulhs, umodes, script] = [umap.lhs, umap.modes, umap.script]
                if prev_ulhs != ulhs
                    " Starting new range of user maps with common lhs.
                    " Assumption: For a given ulhs, multi-mode command will be
                    " first or not at all.
                    let [prev_ulhs, first_ulhs, need_maybes] = [ulhs, 1, 0]
                    " Record index at which map search will start for each
                    " umap in range.
                    let i_first = i
                endif
                " Accumulate overlap between this umap and any ambiguous maps.
                let omodes = ''
                " Start with first map that *could* conflict with current user
                " map, short-circuiting when we pass the last one that could,
                " or when all modes in the umap have been flagged.
                let i = i_first
                while i < n
                    let map = maps[i]
                    let [lhs, modes] = [map.lhs, map.modes]
                    " Is one lhs a prefix of the other?
                    let cmp = s:compare_prefix(ulhs, lhs)
                    if !cmp
                        " Prefixes ambiguous; accumulate any mode overlap.
                        let omodes = s:or_modes(omodes, s:and_modes(umodes, modes))
                        if len(omodes) == len(umodes)
                            " No point in continuing to check maps now that
                            " we've flagged all modes.
                            break
                        endif
                    elseif cmp < 0
                        " Both inner and outer lists are sorted by lhs, so we can
                        " short-circuit and restart this loop at i_first after
                        " moving to the subsequent umap.
                        break
                    endif
                    let i += 1
                endwhile
                " Does this umap require delete/restore?
                if !empty(omodes) || need_maybes
                    " Determine whether subsequent umaps with same lhs will
                    " need restore, if only because of the :map command used
                    " to restore the head umap in range.
                    let need_maybes = need_maybes ||
                        \ (first_ulhs && s:is_multi_modes(umodes))
                    " TODO: Consider refactoring into function.
                    " Caveat: This call is safe only because buffer maps
                    " are processed before globals.
                    let umap = maparg(ulhs, umodes, 0, 1)
                    let umap.script = script
                    " Add to lists for subsequent delete/restore.
                    if !empty(omodes)
                        " Delete only the overlapping modes.
                        call add(cmds[1], s:build_delete_cmds(umap, omodes))
                        " Perform the delete now.
                        " Rationale: Prevents buffer maps from shadowing
                        " global.
                        exe cmds[1][-1]
                    endif
                    " 2 kinds of restore:
                    " 1. maps deleted upon entry
                    " 2. maps deleted by :map command used to restore
                    "    a multi-mode map upon exit.
                    call add(cmds[0], s:build_restore_cmds(umap))
                endif
            endfor
        endfor
        return cmds
endfunction

" Pre-condition: Both lists sorted on lhs.
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

function! s:build_sexp_map_cmds(esc_key)
    let [cmds, sexp_maps] = [[[], []], []]
    for [plug, modestr] in s:plug_map_modes
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if lhs =~ '^\s*$'
            " Empty or all whitespace lhs disables map.
            continue
        endif
        if !empty(a:esc_key)
            " If caller is creating escape maps, he'll need a list of the sexp
            " maps built.
            call add(sexp_maps, {'lhs': lhs, 'modes': s:expand_modes(modestr)})
        endif
        for mode in split(modestr, '\zs')
            " Accumulate exit/entry commands.
            call add(cmds[0],
                \ 'silent! ' . mode . 'unmap <buffer>' . lhs)
            call add(cmds[1],
                \ mode . 'map <nowait><silent><buffer>'
                \ . lhs . ' <Plug>(' . plug . ')')
        endfor
    endfor
    " Sort the sexp_maps list.
    if !empty(sexp_maps)
        call sort(sexp_maps, 's:by_lhs_sort_fn')
    endif
    return [cmds, sexp_maps]
endfunction

function! s:sexp_toggle_non_insert_mappings()
    let enabled = !exists('b:sexp_state_enabled') || !b:sexp_state_enabled
    " TODO: Probably add a try/catch/finally around all of this...
    if !exists('b:sexp_state_enabled')
        " First time toggle ON for this buffer! Build and cache data
        " structures for future use.
        let esc_key = ''
        if exists('g:sexp_escape_key') && !empty(g:sexp_escape_key)
            " TODO: Perhaps some validation: e.g., single key.
            " TODO: Should we permit stuff like <LocalLeader>? If so, need to
            " canonicalize case...
            let esc_key = g:sexp_escape_key
        endif

        let [b:sexp_map_commands, sexp_maps] = s:build_sexp_map_cmds(esc_key)
        "echomsg "SM Exit: " . string(b:sexp_map_commands[0])
        "echomsg "SM Enter: " . string(b:sexp_map_commands[1])
        if exists('l:esc_key')
            " Get list of escape maps we'll need to create for builtins.
            " TODO: Perhaps have get_sexp_maps return this as well.
            let esc_maps = s:get_escape_maps(esc_key, sexp_maps)
            "echomsg "esc_maps 1 :" . string(esc_maps)
            " Discard esc maps that would conflict with sexp maps.
            " Rationale: The logic within shadow_conflicting_usermaps requires
            " as pre-condition that no esc_map match the head of any sexp map.
            " (Moreover, we wouldn't want to shadow sexp maps with esc maps or
            " vice-versa.)
            " User probably should probably be warned not to do this in docs.
            " Is run-time warning appropriate? (TODO)
            let esc_maps =
                \ s:discard_conflicting_escape_maps(esc_maps, sexp_maps)
            "echomsg "esc_maps 2 :" . string(esc_maps)
            " Create the lists of escape map commands.
            let b:escape_map_commands = s:build_escape_map_cmds(esc_key, esc_maps)
            "echomsg "EM Exit esc: " . string(b:escape_map_commands[0])
            "echomsg "EM Enter esc: " . string(b:escape_map_commands[1])
        endif
        " Build the commands to save/restore any conflicting user maps.
        " Note: This needs to be run even if user hasn't configured escape
        " key, but in that case, esc_maps will be empty.
        let b:user_map_commands =
            \ s:shadow_conflicting_usermaps(esc_key, sexp_maps, esc_maps)
        "echomsg "UM Exit esc: " . string(b:user_map_commands[0])
        "echomsg "UM Enter esc: " . string(b:user_map_commands[1])
    endif
    " Perform entry/exit using buf-locally cached map commands.
    " Caveat: Order in which cmd groups are processed is significant: e.g.,
    " must delete user maps first on entry and restore them last on exit.
    " Caveat: On initial toggle into sexp state, user maps will already have
    " been removed above, so we'll need to skip here...
    let cmd_groups = [
        \ exists('b:sexp_state_enabled') ? b:user_map_commands : [],
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

    " Record current state.
    let b:sexp_state_enabled = enabled
    " Just in case user does something silly with his customizations, make
    " sure toggle is always available.
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

function! s:on_buf_load()
    " TEMP SOLUTION
    if !exists('b:vim_sexp_loaded')
        let b:vim_sexp_loaded = 1
        if (!exists('g:sexp_toggle_map') || empty(g:sexp_toggle_map))
            call s:sexp_create_mappings()
        else
            call s:create_sexp_state_toggle()
        endif
    endif
endfunction

" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
function! s:sexp_create_mappings()
    " TODO: Does this need to be refactored? Ok to treat the initial
    " activation as toggle when sexp-state disabled?
    call s:sexp_toggle_non_insert_mappings()
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
