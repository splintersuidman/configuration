" Copied from https://you84815.blogspot.com/2017/02/lightlinevimdracula.html,
" which is written in a language I don't know, but the code is readable.
set noshowmode
hi WildMenu ctermbg=2 guifg=BLACK guibg=#50fa7b gui=none
hi CursorLineNr ctermbg=2 ctermfg=0 guifg=#50fa7b gui=none
hi VertSplit ctermfg=237 ctermbg=237 cterm=bold guifg=#3a3a3a guibg=#3a3a3a gui=bold
let s:black          = [ '#1c1c1c', 234 ]
let s:dark_gray      = [ '#262626', 235 ]
let s:gray           = [ '#3a3a3a', 237 ]
let s:light_gray     = [ '#4e4e4e', 239 ]
let s:white          = [ '#dadada', 253 ]
let s:dark_blue_gray = [ '#6272a4', 60  ]
let s:purple         = [ '#bd93f9', 141 ]
let s:pink           = [ '#ff79c6', 212 ]
let s:yellow         = [ '#f1fa8c', 228 ]
let s:orange         = [ '#ffb86c', 228 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left     = [ [ s:white, s:dark_blue_gray], [s:white, s:gray ] ]
let s:p.normal.right    = [ [ s:white, s:dark_blue_gray], [s:white, s:gray ] ]
let s:p.inactive.left   = [ [ s:white, s:gray], [s:white, s:gray ] ]
let s:p.inactive.right  = [ [ s:white, s:gray], [s:white, s:gray ] ]
let s:p.insert.left     = [ [ s:black, s:purple], [s:purple, s:gray ] ]
let s:p.insert.right    = [ [ s:black, s:purple], [s:purple, s:gray ] ]
let s:p.replace.left    = [ [ s:black, s:yellow], [s:yellow, s:gray ] ]
let s:p.replace.right   = [ [ s:black, s:yellow], [s:yellow, s:gray ] ]
let s:p.visual.left     = [ [ s:black, s:pink], [s:pink, s:gray ] ]
let s:p.visual.right    = [ [ s:black, s:pink], [s:pink, s:gray ] ]
let s:p.normal.middle   = [ [ s:white, s:dark_gray ] ]
let s:p.inactive.middle = [ [ s:white, s:dark_gray ] ]
let s:p.tabline.left    = [ [ s:white, s:light_gray ] ]
let s:p.tabline.tabsel  = [ [ s:white, s:dark_blue_gray ] ]
let s:p.tabline.middle  = [ [ s:white, s:gray ] ]
let s:p.tabline.right   = [ [ s:white, s:dark_gray ] ]
let s:p.normal.error    = [ [ s:black, s:orange ] ]
let s:p.normal.warning  = [ [ s:black, s:orange ] ]
let g:lightline#colorscheme#dracula#palette = lightline#colorscheme#flatten(s:p)

let g:lightline = {
      \ 'colorscheme': 'dracula',
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
