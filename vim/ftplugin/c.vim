setlocal foldmethod=syntax
" Open all folds by default.
normal zR
let b:undo_ftplugin .= '|setlocal foldmethod<'

" Start chromatica
au FileType c ChromaticaStart
