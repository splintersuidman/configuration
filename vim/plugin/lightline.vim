" TODO: create dracula theme.
set noshowmode
let g:lightline = {
      \ 'colorscheme': 'one',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype', ] ],
      \ },
      \ }

" To add Syntastic support:
" \ 'component_function': {
" \   'syntastic': 'SyntasticStatusLineFlag',
" \ }
