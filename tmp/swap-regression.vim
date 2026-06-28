set nocompatible
syntax on
set rtp^=.
runtime plugin/sexp.vim
set ft=clojure
let g:sexp_auto_indent = 0

let s:failures = []

function! s:RunSwap(lines, cursor, steps, ...) abort
    let save_force_linewise = exists('g:sexp_swap_force_linewise')
        \ ? g:sexp_swap_force_linewise
        \ : 'mc'
    let save_slide = exists('g:sexp_swap_slide') ? g:sexp_swap_slide : 0
    let save_placement_policy = exists('g:sexp_swap_placement_policy')
        \ ? g:sexp_swap_placement_policy
        \ : 'target'
    let save_literal_sep_side = exists('g:sexp_swap__literal_sep_side')
        \ ? g:sexp_swap__literal_sep_side
        \ : 'outbound'
    if a:0
        let g:sexp_swap_force_linewise = a:1
    endif
    if a:0 > 1
        let g:sexp_swap_slide = a:2
    endif
    if a:0 > 2
        let g:sexp_swap_placement_policy = a:3
    endif
    if a:0 > 3
        let g:sexp_swap__literal_sep_side = a:4
    endif

    %delete _
    call setline(1, a:lines)
    call cursor(a:cursor[0], a:cursor[1])
    for step in a:steps
        call sexp#docount_stateful(get(step, 'count', 1), 'sexp#swap_element',
            \ 'n', step.next, step.list)
    endfor

    let ret = getline(1, '$')
    let g:sexp_swap_force_linewise = save_force_linewise
    let g:sexp_swap_slide = save_slide
    let g:sexp_swap_placement_policy = save_placement_policy
    let g:sexp_swap__literal_sep_side = save_literal_sep_side
    return ret
endfunction

function! s:Assert(name, actual, expected) abort
    if a:actual !=# a:expected
        call add(s:failures, a:name)
        call add(s:failures, '  expected:')
        call extend(s:failures, map(copy(a:expected), '"    " . v:val'))
        call add(s:failures, '  actual:')
        call extend(s:failures, map(copy(a:actual), '"    " . v:val'))
    endif
endfunction

function! s:RunPasteOverSelfThenSwap() abort
    let save_slide = exists('g:sexp_swap_slide') ? g:sexp_swap_slide : 0
    let g:sexp_swap_slide = 1

    %delete _
    call setline(1, ['A B C', '(foo', '  bar)', 'D E F'])
    normal! ggV3jy
    normal! ggV3jp
    call cursor(1, 1)
    call sexp#docount_stateful(1, 'sexp#swap_element', 'n', 1, 0)

    let ret = getline(1, '$')
    let g:sexp_swap_slide = save_slide
    return ret
endfunction

call s:Assert('default multiline moved form stays linewise',
    \ s:RunSwap(['(foo', '  bar)', 'A B'], [1, 1],
        \ [{'next': 1, 'list': 1}]),
    \ ['A', '(foo', '  bar)', 'B'])

call s:Assert('initial healed edge respects multiline target',
    \ s:RunSwap(['A B', '(foo', '  bar)', 'C D'], [1, 3],
        \ [{'next': 1, 'list': 0}]),
    \ ['A', '(foo', '  bar)', 'B', 'C D'])

call s:Assert('empty force_linewise does not force trailing multiline separator',
    \ s:RunSwap(['(foo', '  bar)', 'A B'], [1, 1],
        \ [{'next': 1, 'list': 1}], ''),
    \ ['A', '(foo', '  bar) B'])

call s:Assert('full-line comment target remains linewise when swapping back',
    \ s:RunSwap(['(foo', '  ;; comment', '  (bar)', '  (baz))'], [3, 4],
        \ [{'next': 0, 'list': 1}]),
    \ ['(foo', '  (bar)', '  ;; comment', '  (baz))'])

call s:Assert('eol comment moves with preceding form without swallowing target',
    \ s:RunSwap(['(foo', '  (bar) ; bar', '  (baz))'], [2, 4],
        \ [{'next': 1, 'list': 1}]),
    \ ['(foo', '  (baz)', '  (bar) ; bar', ')'])

call s:Assert('inline trailing comment may remain inline after moving forward',
    \ s:RunSwap(['A B ; b-comment', 'C D'], [1, 3],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['A C D', 'B ; b-comment'])

call s:Assert('line-start trailing comment is not appended after prior element',
    \ s:RunSwap(['B ; b-comment', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['C D', 'B ; b-comment'])

call s:Assert('swapping before trailing comment does not duplicate comment text',
    \ s:RunSwap(['A B ; comment', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['B ; comment', 'C A D'])

call s:Assert('swapping before trailing comment preserves hinged edge on third swap',
    \ s:RunSwap(['A B ; comment', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 3}]),
    \ ['B ; comment', 'C D A'])

call s:Assert('reversing before trailing comment restores original text',
    \ s:RunSwap(['A B ; comment', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 3},
            \ {'next': 0, 'list': 0, 'count': 3}]),
    \ ['A B ; comment', 'C D'])

call s:Assert('forward then backward restores original separators',
    \ s:RunSwap(['A', '(foo', '  bar)', 'B C'], [1, 1],
        \ [{'next': 1, 'list': 0}, {'next': 0, 'list': 0}]),
    \ ['A', '(foo', '  bar)', 'B C'])

call s:Assert('partial reversal restores last crossed separators',
    \ s:RunSwap(['A B', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}, {'next': 0, 'list': 0}]),
    \ ['B A', 'C D'])

call s:Assert('outbound healing does not append pair after swapee moved from bol',
    \ s:RunSwap(['A B', 'C D'], [1, 3],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['A C D B'])

call s:Assert('counted inline swap heals passed siblings',
    \ s:RunSwap(['(foo a b c d)'], [1, 6],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['(foo b c a d)'])

call s:Assert('inline swap preserves multibyte element text',
    \ s:RunSwap(['α β γ'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}]),
    \ ['β γ α'])

call s:Assert('blankline after target remains on far side after forward swap',
    \ s:RunSwap(['A', 'B', '', 'C'], [1, 1],
        \ [{'next': 1, 'list': 0}]),
    \ ['B', 'A', '', 'C'])

call s:Assert('healed prefix ignores blankline between moving and target',
    \ s:RunSwap(['Z', 'A', '', 'B', '', 'C'], [2, 1],
        \ [{'next': 1, 'list': 0}]),
    \ ['Z', 'B', '', 'A', '', 'C'])

call s:Assert('healed prefix preserves inline slot after multiline previous unit',
    \ s:RunSwap(['(A', '  B) C', 'D', 'E'], [2, 6],
        \ [{'next': 1, 'list': 0}]),
    \ ['(A', '  B) D', 'C', 'E'])

call s:Assert('backward swap keeps multiline swapee linewise before trailing comment',
    \ s:RunSwap(['(foo', '  bar)', 'A B ; comment', 'C D'], [3, 1],
        \ [{'next': 0, 'list': 0}]),
    \ ['A', '(foo', '  bar)', 'B ; comment', 'C D'])

call s:Assert('cross-command swap sequence preserves directional slot separators',
    \ s:RunSwap(['A', '(foo', '  bar)', 'B C D'], [1, 1],
        \ [{'next': 1, 'list': 0}, {'next': 1, 'list': 0}]),
    \ ['(foo', '  bar)', 'B A C D'])

call s:Assert('multiline outbound swap restores inline sibling run',
    \ s:RunSwap(['(foo bar', '     baz)', 'A B C'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 3}]),
    \ ['A B C', '(foo bar', '     baz)'])

call s:Assert('boundary hit does not disturb buffer',
    \ s:RunSwap(['A B'], [1, 1],
        \ [{'next': 1, 'list': 0}, {'next': 1, 'list': 0}]),
    \ ['B A'])

call s:Assert('boundary hit preserves reversal stack after multiline crossing',
    \ s:RunSwap(['A B C', '(foo', '  bar)'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 3},
            \ {'next': 1, 'list': 0},
            \ {'next': 0, 'list': 0}]),
    \ ['B C', 'A', '(foo', '  bar)'])

call s:Assert('swap slide defaults off',
    \ s:RunSwap(['A (foo', '    bar)', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0}]),
    \ ['(foo', '    bar)', 'A', 'C D'])

call s:Assert('swap slide joins existing outbound inline run',
    \ s:RunSwap(['A (foo', '    bar)', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0}], 'mc', 1),
    \ ['(foo', '    bar)', 'A C D'])

call s:Assert('swap slide keeps next outbound inline edge inline',
    \ s:RunSwap(['A B C', '(foo', '  bar)', 'D E F'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 4}], 'mc', 1),
    \ ['B C', '(foo', '  bar)', 'D A E F'])

call s:Assert('swap slide level one does not create inline run',
    \ s:RunSwap(['A (foo', '    bar)', 'C'], [1, 1],
        \ [{'next': 1, 'list': 0}], 'mc', 1),
    \ ['(foo', '    bar)', 'A', 'C'])

call s:Assert('swap slide level two creates inline row',
    \ s:RunSwap(['A (foo', '    bar)', 'C'], [1, 1],
        \ [{'next': 1, 'list': 0}], 'mc', 2),
    \ ['(foo', '    bar)', 'A C'])

call s:Assert('backward swap slide joins existing outbound inline run',
    \ s:RunSwap(['C D', '(foo', '  bar) A'], [3, 8],
        \ [{'next': 0, 'list': 0}], 'mc', 1),
    \ ['C D A', '(foo', '  bar)'])

call s:Assert('swap after linewise paste-over-self uses computed adjacent range',
    \ s:RunPasteOverSelfThenSwap(),
    \ ['B A C', '(foo', '  bar)', 'D E F'])

call s:Assert('continued backward swap preserves blank line once',
    \ s:RunSwap(['A B', '', 'C D', '(foo', '  bar)', 'E'], [6, 1],
        \ [{'next': 0, 'list': 0, 'count': 4}]),
    \ ['A E B', '', 'C D', '(foo', '  bar)'])

call s:Assert('continued carry hint preserves crossing-edge class',
    \ s:RunSwap(['A B', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}], 'mc', 0, 'carry'),
    \ ['B', 'C', 'A D'])

call s:Assert('literal target-side separator option moves blank gap before moved unit',
    \ s:RunSwap(['A B', 'C', '', 'D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}], 'mc', 0, 'target', 'target'),
    \ ['B', 'C', '', 'A', 'D'])

call s:Assert('slot placement preserves current slot shape',
    \ s:RunSwap(['A B', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 2}], 'mc', 0, 'slot'),
    \ ['B C', 'A D'])

call s:Assert('slot placement reforms outbound inline run',
    \ s:RunSwap(['A', 'B C D'], [1, 1],
        \ [{'next': 1, 'list': 0, 'count': 3}], 'mc', 0, 'slot'),
    \ ['B', 'C D A'])

call s:Assert('slot placement supports slide option',
    \ s:RunSwap(['A (foo', '    bar)', 'C D'], [1, 1],
        \ [{'next': 1, 'list': 0}], 'mc', 2, 'slot'),
    \ ['(foo', '    bar)', 'A C D'])

if empty(s:failures)
    call writefile(['PASS swap regression'], 'tmp/swap-regression.out')
    qall!
else
    call writefile(s:failures, 'tmp/swap-regression.out')
    cquit
endif
