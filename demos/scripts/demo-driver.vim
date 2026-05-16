set nocompatible
set columns=80
set lines=20
set cmdheight=1
set laststatus=0
set noruler
set noshowmode
set noshowcmd
set shortmess+=I
set fillchars=vert:\ 
set runtimepath^=.

let s:demo_name = $DEMO_NAME
let s:fixture = $DEMO_FIXTURE
let s:line = str2nr($DEMO_LINE)
let s:col = str2nr($DEMO_COL)
let s:keys = $DEMO_KEYS
let s:display_keys = $DEMO_DISPLAY_KEYS
let s:caption = $DEMO_CAPTION
let s:setup = $DEMO_SETUP
let s:ts_rtp = $DEMO_TS_RTP

if !empty(s:ts_rtp) && isdirectory(s:ts_rtp)
    execute 'set runtimepath+=' . fnameescape(s:ts_rtp)
endif

for cmd in split(s:setup, '|', 1)
    if !empty(cmd)
        execute cmd
    endif
endfor

let s:initial_sleep = get(g:, 'demo_initial_sleep', 900)
let s:step_sleep = get(g:, 'demo_step_sleep', 900)
let s:final_sleep = get(g:, 'demo_final_sleep', 700)
let s:steps = filter(split(s:keys, '|', 1), '!empty(v:val)')

function! s:state_caption(state) abort
    return a:state . ': ' . s:caption
endfunction

function! s:set_caption(state) abort
    call setbufline(s:caption_bufnr, 2, s:state_caption(a:state))
endfunction

runtime plugin/sexp.vim
filetype plugin indent on
syntax on

execute 'edit ' . fnameescape(s:fixture)
setlocal filetype=clojure
setlocal number norelativenumber nowrap scrolloff=3 sidescrolloff=5
normal! gg

" Warm Treesitter before the visible part of the recording so first-use parser work and
" warnings do not obscure the initial cursor position.
silent! lua pcall(function() vim.treesitter.start(0, vim.bo.filetype); vim.treesitter.get_parser(0, vim.bo.filetype):parse() end)
silent! messages clear

topleft 3new
setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted
setlocal nonumber norelativenumber nowrap
setlocal filetype=
call setline(1, 'Press: ' . s:display_keys)
call setline(2, s:state_caption('Before'))
call setline(3, repeat('-', 72))
let s:caption_bufnr = bufnr('%')
normal! gg
wincmd j

call cursor(s:line, s:col)
normal! zv
redraw!
" Force a no-op cursor update after the full redraw. In asciinema/agg output, redraw can
" leave the terminal cursor at the top caption window even though the active Vim cursor is
" already in the fixture window; a tiny movement makes the starting cursor position visible
" before the command runs.
normal! l
normal! h
redraw
execute 'sleep ' . s:initial_sleep . 'm'

let s:step_count = len(s:steps)
let s:step_index = 0
for step in s:steps
    if !empty(step)
        execute 'normal ' . step
        let s:step_index += 1
        if s:step_count <= 1
            call s:set_caption('After')
        else
            call s:set_caption('After ' . s:step_index . '/' . s:step_count)
        endif
        redraw!
        execute 'sleep ' . s:step_sleep . 'm'
    endif
endfor

execute 'sleep ' . s:final_sleep . 'm'

quitall!
