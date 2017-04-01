let s:bufs = {}

fu! rgn#force_load()
    call sexp#force_load()
endfu

fu! s:get_buf()
    let bnr = bufnr(0)
    if !has_key(s:bufs, bnr)
        let s:bufs[bnr] = {'last': {}, 'cur': -1, 'els': []}
    endif
    return s:bufs[bnr]
endfu

" Note: Call from a BufDelete or somesuch...
fu! s:put_buf()
    let bnr = bufnr(0)
    if has_key(s:bufs, bnr)
        call remove(s:bufs, bnr)
    endif
endfu

fu! s:housekeep(o)
    if !empty(a:o.last)
        if a:o.last.tick != b:changedtick
            \ || a:o.last.vsel[0] != getpos("'<")
            \ || a:o.last.vsel[1] != getpos("'>")
            let [a:o.last, a:o.cur, a:o.els] = [{}, -1, []]
        endif
    endif
endfu

fu! s:add(o, ...)
    let [tick, vsel] = [b:changedtick, [getpos("'<"), getpos("'>")]]
    " TODO: Decide whether to store cursor pos (or selection side).
    let a:o.last = {'tick': tick, 'vsel': vsel}
    let el = {
        \ 'tick': tick
        \,'vsel': vsel
        \,'seq_cur': undotree().seq_cur
        \,'curpos': getpos('.')
    \ }
    " Merge any inputs
    if a:0
        for [k, v] in items(a:1)
            let el[k] = v
        endfor
    endif
    let a:o.cur += 1
    call insert(a:o.els, el, a:o.cur)
    " Remove anything beyond new top.
    if len(a:o.els) > a:o.cur + 1
        call remove(a:o.els, a:o.cur + 1, -1)
    endif
endfu

fu! s:get_cur(o)
    return a:o.cur >= 0 ? a:o.els[a:o.cur] : {}
endfu

fu! s:get_end(o)
    return a:o.cur >= 0 ? a:o.els[-1] : {}
endfu

" Return:
" 0 if no movement possible
" signed value indicating how far we moved in stack.
fu! s:undo(o, n)
    if s:empty(a:o)
        " Shouldn't get here.
        return [{}, {}]
    endif
    let old = a:o.els[a:o.cur]
    if !a:n
        return [old, old]
    endif
    " positive n => undo
    let cur = a:o.cur - a:n
    if cur < 0
        " Limit undo
        let cur = 0
    elseif cur >= len(a:o.els)
        " Limit redo
        let cur = len(a:o.els) - 1
    endif
    let a:o.cur = cur
    return [old, a:o.els[cur]]
endfu

fu! s:empty(o)
    return empty(a:o.last)
endfu

fu! rgn#pre_op()
    let o = s:get_buf()
    call s:housekeep(o)
    if s:empty(o)
        " Go ahead and add original position, but without special info.
        " TODO: Think on this...
        call s:add(o)
    endif
endfu

fu! rgn#post_op(form, inner)
    let o = s:get_buf()
    call s:add(o, {'form': a:form, 'inner': a:inner})
endfu

" Note: n is signed value + for undo, - for redo
fu! rgn#undo(n)
    let [old, new] = s:undo(s:get_buf(), a:n)
    if !empty(new)
        if old.tick != new.tick
            exe 'undo' new.seq_cur
        endif
        call g:Sexp_set_visual_marks(new.vsel)
        call g:Sexp_select_current_marks('v')
    endif
endfu

" TODO: Debug only
fu! rgn#dbg_get_bufs()
    return s:bufs
endfu

map <F6> :<C-U>call rgn#pre_op()<CR>
map <F7> :<C-U>call rgn#post_op(0, 1)<CR>
map <F8> :<C-U>call rgn#undo(1)<CR>
map <F9> :<C-U>call rgn#undo(-1)<CR>

" Return value indicating which side of region cursor is on: 0=left, 1=right
fu! rgn#side()
    "return getpos('.') == a:o.info.vs ? 0 : 1
endfu

