let s:o = {
    \ 'valid': 0,
    \ 'changedtick': 0,
    \ 'info': {}
\ }

fu! rgn#set(form, inner)
    let s:o.valid = 1
    let s:o.changedtick = b:changedtick
    let s:o.info = {
        \ 'form': a:form,
        \ 'inner': a:inner,
        \ 'vs': getpos("'<"),
        \ 've': getpos("'>")
    \ }
    " TODO: Undo/redo information.
endfu

fu! rgn#is_special()
    return s:o.changedtick == b:changedtick
        \ && s:o.info.vs == getpos("'<")
        \ && s:o.info.ve == getpos("'>")
endfu

fu! rgn#info()
    return s:o.info
endfu

" Return value indicating which side of region cursor is on: 0=left, 1=right
fu! rgn#side()
    return getpos('.') == s:o.info.vs ? 0 : 1
endfu

fu! rgn#formp()
    return s:o.info.form
endfu

fu! rgn#innerp()
    return s:o.info.inner
endfu

fu! rgn#vs()
    return s:o.info.vs
endfu

fu! rgn#ve()
    return s:o.info.ve
endfu
