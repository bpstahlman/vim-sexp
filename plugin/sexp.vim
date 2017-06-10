
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
    \ 'n': [
        \ '<C-A>',
        \ '<C-B>',
        \ '<C-C>',
        \ '<C-D>',
        \ '<C-E>',
        \ '<C-F>',
        \ '<C-G>',
        \ '<BS>',
        \ '<C-H>',
        \ '<Tab>',
        \ '<C-I>',
        \ '<NL>',
        \ '<C-J>',
        \ '<C-L>',
        \ '<CR>',
        \ '<C-M>',
        \ '<C-N>',
        \ '<C-O>',
        \ '<C-P>',
        \ '<C-Q>',
        \ '<C-R>',
        \ '<C-S>',
        \ '<C-T>',
        \ '<C-U>',
        \ '<C-V>',
        \ {
            \ 'key': '<C-W>',
            \ 'children': [
                \ '<C-B>',
                \ '<C-C>',
                \ '<C-D>',
                \ '<C-F>',
                \ '<C-G>',
                \ '<C-H>',
                \ '<C-H>',
                \ '<C-I>',
                \ '<C-J>',
                \ '<C-K>',
                \ '<C-L>',
                \ '<C-N>',
                \ '<C-O>',
                \ '<C-P>',
                \ '<C-Q>',
                \ '<C-R>',
                \ '<C-S>',
                \ '<C-T>',
                \ '<C-V>',
                \ '<C-W>',
                \ '<C-X>',
                \ '<C-Z>',
                \ '<C-]>',
                \ '<C-^>',
                \ '<C-_>',
                \ '+',
                \ '-',
                \ '<',
                \ '=',
                \ '>',
                \ 'H',
                \ 'J',
                \ 'K',
                \ 'L',
                \ 'P',
                \ 'R',
                \ 'S',
                \ 'T',
                \ 'W',
                \ ']',
                \ '^',
                \ '_',
                \ 'b',
                \ 'c',
                \ 'd',
                \ 'f',
                \ 'F',
                \ {
                    \ 'key': 'g',
                    \ 'children': [
                        \ '<C-]>', ']', '}', 'f', 'F'
                    \ ]
                \ },
                \ 'h',
                \ 'i',
                \ 'j',
                \ 'k',
                \ 'l',
                \ 'n',
                \ 'o',
                \ 'p',
                \ 'q',
                \ 'r',
                \ 's',
                \ 't',
                \ 'v',
                \ 'w',
                \ 'x',
                \ 'z',
                \ '<Bar>',
                \ '}',
                \ '<Down>',
                \ '<Up>',
                \ '<Left>',
                \ '<Right>',
            \ ]
        \ },
        \ '<C-X>',
        \ '<C-Y>',
        \ '<C-Z>',
        \ {
            \ 'key': '<C-\>',
            \ 'children': [
                \ '<C-N>', '<C-G>',
                \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                \ 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
            \ ]
        \ },
        \ '<Space>',
        \ {
            \ 'key': '!',
            \ 'children': [
                \ '!'
            \ ]
        \ },
        \ '#',
        \ '$',
        \ '%',
        \ '&',
        \ {
            \ 'key': '''',
            \ 'children': [
                \ '''', '(', ')', '<', '>', '[', ']', '{', '}'
            \ ]
        \ },
        \ '(',
        \ ')',
        \ '*',
        \ '+',
        \ ',',
        \ '-',
        \ '.',
        \ '/',
        \ '<CR>',
        \ '0',
        \ ':',
        \ ';',
        \ '<',
        \ '=',
        \ '>',
        \ '?',
        \ '@',
        \ 'A',
        \ 'B',
        \ 'C',
        \ 'D',
        \ 'E',
        \ 'F',
        \ 'G',
        \ 'H',
        \ 'I',
        \ 'J',
        \ 'K',
        \ 'L',
        \ 'M',
        \ 'N',
        \ 'O',
        \ 'P',
        \ 'Q',
        \ 'R',
        \ 'S',
        \ 'T',
        \ 'U',
        \ 'V',
        \ 'W',
        \ 'X',
        \ 'Y',
        \ {
            \ 'key': 'Z',
            \ 'children': [
                \ 'Z', 'Q'
            \ ]
        \ },
        \ {
            \ 'key': '[',
            \ 'children': [ ]
        \ },
        \ {
            \ 'key': ']',
            \ 'children': [ ]
        \ },
        \ '^',
        \ '_',
        \ {
            \ 'key': '`',
            \ 'children': [
                \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                \ 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                \ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                \ 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                \ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                \ '(', ')', '<', '>', '[', ']', '`', '{', '}'
            \ ]
        \ },
        \ 'a',
        \ 'b',
        \ 'c',
        \ 'd',
        \ 'e',
        \ 'f',
        \ 'g',
        \ 'h',
        \ 'i',
        \ 'j',
        \ 'k',
        \ 'l',
        \ 'm',
        \ 'n',
        \ 'o',
        \ 'p',
        \ {
            \ 'key': 'q',
            \ 'children': [
                \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                \ 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                \ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                \ 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                \ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                \ '"', ':', '/', '?'
            \ ]
        \ },
        \ 'r',
        \ 's',
        \ 't',
        \ 'u',
        \ 'v',
        \ 'w',
        \ 'x',
        \ 'y',
        \ 'z',
        \ '{',
        \ '<Bar>',
        \ '}',
        \ '~',
    \ ],
    \ 'v': [
        \ {
            \ 'key': '<C-\>',
            \ 'children': [
                \ '<C-N>', '<C-G>'
            \ ]
        \ },
        \ '<C-C>',
        \ '<C-G>',
        \ '<BS>',
        \ '<C-H>',
        \ '<C-O>',
        \ '<C-V>',
        \ '<Esc>',
        \ '<C-]>',
        \ '!',
        \ ':',
        \ '<lt>',
        \ '=',
        \ 'C',
        \ 'D',
        \ 'J',
        \ 'K',
        \ 'O',
        \ 'R',
        \ 'S',
        \ 'U',
        \ 'V',
        \ 'X',
        \ 'Y',
        \ {
            \ 'key': 'a',
            \ 'children': [
                \ '"', "'", '(', ')', '<lt>', '>', 'B', 'W', '[', ']', '`',
                \ 'b', 'p', 's', 't', 'w', '{', '}'
            \ ]
        \ },
        \ 'c',
        \ 'd',
        \ {
            \ 'key': 'g',
            \ 'children': [
                \ 'J', 'q', 'v'
            \ ]
        \ },
        \ {
            \ 'key': 'i',
            \ 'children': [
                \ '"', "'", '(', ')', '<lt>', '>', 'B', 'W', '[', ']', '`',
                \ 'b', 'p', 's', 't', 'w', '{', '}'
            \ ]
        \ },
        \ 'o',
        \ 'r',
        \ 's',
        \ 'u',
        \ 'v',
        \ 'x',
        \ 'y',
        \ '~',
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
" TODO: Because we're doing string comparisons on canonicalized forms (and
" case is generally sensitive in maps), we need either to canonicalize case
" here or use "\<...>" to convert to actual byte sequence: e.g.,
" "\<F7>" => <80>k7
" I don't see this causing problems, since these canonical forms would be used
" only for in memory comparisons.
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
                let c = '<lt>'
                " Let the rest be handled on subsequent iterations.
                let ie = i + 1
            endif
        else
            " This match is guaranteed to succeed.
            let ie = matchend(s, '.', i)
            let c = s[i:ie-1]
            " With default 'cpo' (which we're assuming), bslash is not
            " special in mappings, but canonical form requires it to be
            " expressed in key-notation.
            if c == '\'
                let c = '<Bslash>'
            endif
        endif
        call add(ret, c)
        let i = ie
    endwhile
    return ret
endfunction

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
        let [k, descend] = type(key) == 4 ? [key.key, 1] : [key, 0]
        if !has_key(a:map, k)
            let a:map[k] = {'modes': '', 'dmodes': '', 'children': {}}
        endif
        let map = a:map[k]
        if descend
            if map.dmodes !~ a:mode
                " Allow search to short-circuit if mode not represented below
                " this level.
                let map.dmodes .= a:mode
            endif
            call s:add_builtin_r(a:mode, key.children, map.children)
        else
            let map.modes .= a:mode
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
        call s:add_builtin_r(mode, keylist, keymap)
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
" bs      current location in builtins tree structure (implemented as map).
" blhs    cumulative builtin lhs
" lhs_lst list of unprocessed lhs canonicalized key components
" modes   modes in which the sexp-state map being checked is active.
" escs    detected conflicts (key=lhs, val=modes)
function! s:get_conflicting_builtins_r(bs, blhs, lhs_lst, modes, escs)
    if empty(a:lhs_lst)
        " Note: Can't get here on first recursive call.
        " Process all leaves, which are assumed to be descended from a
        " conflicting sexp map.
        let lhs = ''
        let bs_map = a:bs
    else
        " Process only conflicting key and any of its descendants.
        let [lhs, lhs_lst] = [a:lhs_lst[0], a:lhs_lst[1:]]
        " Note: Stuffing into a single key hash permits common logic below.
        let bs_map = has_key(a:bs, lhs) ? {lhs: a:bs[lhs]} : {}
    endif
    " Process leaf(s)
    for [k, o] in items(bs_map)
        let blhs = a:blhs . k
        " Loop over leaf builtins (if any)
        for m in split(o.modes, '\zs')
            " Do any of the leaf's modes interest us?
            if a:modes =~ m
                " Note: Possible to reach the same builtin multiple times.
                if has_key(a:escs, blhs)
                    if a:escs[blhs] !~ m
                        let a:escs[blhs] .= m
                    endif
                else
                    let a:escs[blhs] = m
                endif
            endif
        endfor
        " Descend if there are builtin children *and* the modes we're
        " interested in are represented at lower levels (dmodes).
        " Possible TODO: There are other short-circuit possibilities: e.g.,
        " could modify dmodes to reflect current state of conflict detection,
        " but this is probably not justified, given that the fraction of
        " traversals ending in short-circuit would be extremely small.
        if !empty(o.children) && o.dmodes =~ '[' . a:modes . ']'
            call s:get_conflicting_builtins_r(o, blhs, lhs_lst, a:modes, a:escs)
        endif
    endfor
endfunction

" Build a dict (key=lhs, val=modes) of all builtins that conflict with the
" specified lhs in the specified modes.
function! s:get_conflicting_builtins(modes, lhs, escs)
    let bs = s:get_builtins()
    let lhs_lst = s:split_and_canonicalize_lhs(a:lhs)
    if !empty(lhs_lst)
        " Recurse...
        call s:get_conflicting_builtins_r(bs, '', lhs_lst, a:modes, a:escs)
    endif
endfunction

" Create "escape maps" for each builtin represented in escs, using leader key
" esc_key, skipping any that would conflict with one of the sexp-state maps
" represented by lhs_map. Record escape maps created in undo list.
function! s:create_escape_maps(esc_key, escs, lhs_map, undo)
    for [lhs, modes] in items(a:escs)
        for mode in split(modes, '\zs')
            " Make sure this won't conflict with a sexp map (unlikely).
            if !has_key(a:lhs_map, lhs) || a:lhs_map[lhs] =~ mode
                " Create the escape map and the corresponding unmap.
                execute mode . 'noremap <buffer><nowait> '
                    \ . a:esc_key . lhs . ' ' . lhs
                " Record for subsequent removal.
                call add(a:undo, mode . 'unmap <buffer> ' . a:esc_key . lhs)
            endif
        endfor
    endfor
endfunction

function! s:usermap_sort_fn(a, b)
    let [ama, bma] = [a:a.maparg, a:b.maparg]
    " Note: Comparison must be case-sensitive.
    return ama.lhs < bma.lhs ? -1 : ama.lhs > bma.lhs
        \ ? 1 : ama.buffer ? -1 : bma.buffer ? 1 : 0
endfunction

" Parse output of :map and return relevant results:
" Format: [{'script': 0|1, 'maparg': maparg()}, ...]
" Order: Buffer maps before global, then ordered by lhs (case-sensitive)
" TODO: Could simply augment maparg() dict itself with 'script' flag.
function! s:get_user_maps(esc_key)
    let lsts = [[], []]
    for i in range(2)
        let buffer = i == 0
        redir => maps
        " Note: Get nvo maps.
        silent exe 'map ' . (buffer ? '<buffer>' : '') . ' ' . a:esc_key
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
                    " Get map info required for save/restore.
                    let ma = maparg(lhs, '', 0, 1)
                    " Make sure our canonicalization succeeded.
                    " Note: This should work for 99+% of use cases, but :map output is
                    " not deterministic, so there could be some pathological
                    " corner cases.
                    if !empty(ma)
                        " Now use "noremap" key to disambiguate scope field in
                        " :map output.
                        " Assumption: rhs cannot begin with whitespace.
                        if lhs =~ 'gsnore' && !buffer
                            let g:scope_and_rhs = scope_and_rhs
                            echomsg "Parsing " . scope_and_rhs . '(' . lhs . ')'
                        endif
                        let [_, scope, _, rhs; rest] = matchlist(scope_and_rhs,
                            \ (ma.noremap ? '\([&*]\)' : '\(\)')
                            \.(buffer ? '\(@\)' : '\(\)')
                            \.'\s*\(.*\)')
                        " Determine <script> was used. If so, noremap is
                        " implied.
                        let script = scope == '&'
                        " Add to buffer/global-specific list.
                        call add(lsts[i], { 'maparg': ma, 'script': script})
                    endif
                endif
            endif
        endfor
    endfor
    " Sort and combine.
    for lst in lsts
        call sort(lst, "s:usermap_sort_fn")
    endfor
    " Combine sorted buffer/global lists.
    return lsts[0] + lsts[1]
endfunction

" Accept hash of user maps (lhs => maparg()) and hash of sexp maps (lhs =>
" modes) and return list of user maps requiring save/restore.
function! s:get_conflicting_usermaps(user_maps, sexp_maps, esc_key)
    " Note: Use hash instead of list because we need set behavior.
    " TODO: If I'm going to keep user_maps as map, I'll need an extra key
    " level for buffer/global distinction, but I'm thinking I don't really
    " need map lookup (since I can't really use it).
    " TODO: maparg() doesn't give <script> information, but I can get it from
    " output of :map (&) and augment the data structure.
    let cs = {}
    let umls = sort(keys(a:user_maps))
    let smls = sort(keys(a:sexp_maps))
    for slhs in smls
        let slen = len(slhs)
        let smodes = '[' . a:sexp_maps[slhs] . ']'
        for ulhs in umls
            let ulhs = ulhs[:slen-1]
            if ulhs == slhs
                " Do we have a mode match?
                if a:user_maps[ulhs].modes =~ smodes
                    if has_key(cs, ulhs)
                endif
            elseif ulhs > slhs
                break
            endif
        endfor
    endfor

    for [lhs, modes] in items(a:sexp_maps)
        " Look for user map that matches all of the sexp map with esc leader
        " prepended.
        let lhs = a:esc_key . lhs
        
    endfor
endfunction

function! s:sexp_toggle_non_insert_mappings()
    " TODO: Store the unmap commands in buf-local data structures.
    let create = !exists('b:sexp_unmap_commands')
    if create
        let b:sexp_unmap_commands = []

        if exists('g:sexp_escape_key') && !empty(g:sexp_escape_key)
            " TODO: Perhaps some validation: e.g., single key.
            " TODO: Also, don't create escapes if not in expert mode.
            let [esc_key, escs, sexp_maps] = [g:sexp_escape_key, {}, {}]
        endif

        if exists('l:esc_key')
            let user_maps = s:get_user_maps('<buffer>')
            echomsg "user_maps: " . string(user_maps)
        endif
        for [plug, modestr] in s:plug_map_modes
            let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
            if lhs =~ '^\s*$'
                " lhs can't be empty or all whitespace
                continue
            endif
            let modes = split(modestr, '\zs')
            if exists('esc_key')
                " Record in existence map (for unlikely event in which user
                " creates sexp maps beginning with esc key).
                let sexp_maps[lhs] = modestr
                " Accumulate conflict info to escs map.
                call s:get_conflicting_builtins(modestr, lhs, escs)
            endif
            for mode in modes
                "execute mode . 'map <nowait><silent><buffer>'
                "    \ . lhs . ' <Plug>(' . plug . ')'
                call add(b:sexp_map_commands,
                    \ mode . 'map <nowait><silent><buffer>'
                    \ . lhs . ' <Plug>(' . plug . ')')
                call add(b:sexp_unmap_commands,
                    \ 'silent! execute ' . mode . 'unmap <buffer>' . lhs)
            endfor
        endfor
        if exists('l:esc_key') && !empty(l:escs)
            " Save any conflicting user maps
            let b:save_restore_user_maps =
                \ s:get_conflicting_usermaps(user_maps, sexp_maps)

            call s:create_escape_maps(esc_key, escs, sexp_maps, b:sexp_unmap_commands)
        endif
        " Now it's safe to create sexp-state maps
        for cmd in b:sexp_map_commands
            execute cmd
        endfor
    else
        " Simply unmap...
        for cmd in b:sexp_unmap_commands
            execute cmd
        endfor
        " TODO: Probably don't remove, but cache everything...
        unlet! b:sexp_unmap_commands
    endif

    " Just in case user does something silly with his customizations.
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
