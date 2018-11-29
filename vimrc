" vim:foldmethod=marker
"      ________ ++     ________
"     /VVVVVVVV\++++  /VVVVVVVV\
"     \VVVVVVVV/++++++\VVVVVVVV/
"      |VVVVVV|++++++++/VVVVV/'
"      |VVVVVV|++++++/VVVVV/'
"     +|VVVVVV|++++/VVVVV/'+
"   +++|VVVVVV|++/VVVVV/'+++++
" +++++|VVVVVV|/VVVVV/'+++++++++
"   +++|VVVVVVVVVVV/'+++++++++
"     +|VVVVVVVVV/'+++++++++
"      |VVVVVVV/'+++++++++
"      |VVVVV/'+++++++++
"      |VVV/'+++++++++
"      'V/'   ++++++
"               ++

set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin indent on     " required

" Plugins {{{

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Easy editing {{{
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
" Plug 'terryma/vim-multiple-cursors'
Plug 'townk/vim-autoclose'
Plug 'bronson/vim-trailing-whitespace'
Plug 'easymotion/vim-easymotion'
Plug 'airblade/vim-gitgutter'
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
" Plug 'terryma/vim-smooth-scroll'
" Plug 'nathanaelkane/vim-indent-guides'
" Plug 'mattn/emmet-vim'
" Plug 'editorconfig/editorconfig-vim'
Plug '/usr/local/opt/fzf'
Plug 'https://gitlab.com/Lenovsky/nuake.git'
" }}}

" Aesthetics {{{
" Plug 'mhinz/vim-startify'
" Plug 'bling/vim-airline'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
" Plug 'sebastianmarkow/deoplete-rust', { 'for': 'rust' } " Rust
" }}}

" Autocompletion {{{
Plug '~/.vim/bundle/YouCompleteMe'
Plug 'SirVer/UltiSnips'
" Plug 'honza/vim-snippets'
" Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
" if has('nvim')
"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif
" }}}

" Language support {{{
Plug 'vim-syntastic/syntastic', { 'for': [] }
" C
if has('nvim')
    Plug 'arakashic/chromatica.nvim', { 'for': ['c', 'cpp', 'objc', 'objcpp'] }
endif
" Objective-C / Cocoa
Plug 'msanders/cocoa.vim', { 'for': 'objc' }
" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" Plug 'racer-rust/vim-racer', { 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
" (La)TeX
Plug 'lervag/vimtex', { 'for': ['tex', 'plaintex'] }
" Plug 'LaTeX-Box-Team/LaTeX-Box'
" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
" Plug 'Shougo/vimproc.vim', {'do' : 'make'}
" Plug 'eagletmt/ghcmod-vim'
" JavaScript
" Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
" Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
" Go
Plug 'fatih/vim-go', { 'for': 'go' }
" Elvish
Plug 'huiyiqun/elvish.vim'
" Pascal
" Plug 'rkennedy/vim-delphi'
" LLVM
" Plug 'Superbil/llvm.vim'
" augroup filetype
"   au! BufRead,BufNewFile *.ll set filetype=llvm
" augroup END
" Brainfuck (of course)
Plug 'kmyk/brainfuck-highlight.vim'
" Org mode
Plug 'jceb/vim-orgmode'
" }}}

" Formatting {{{
" Plug 'sbdchd/neoformat'
" }}}

" Linters {{{
" Plug 'sindresorhus/vim-xo'
" Plug 'prettier/prettier'
" }}}

" Colour theme {{{
" Plug 'ayu-theme/ayu-vim'
Plug 'morhetz/gruvbox'
Plug 'dracula/vim'
" Plug 'junegunn/seoul256.vim'
if has('nvim')
  Plug 'vim-scripts/AfterColors.vim'
endif
" Plug 'flazz/vim-colorschemes'
" Plug 'chriskempson/base16-vim'
" Plug 'nanotech/jellybeans.vim', { 'tag': 'v1.6' }
" Plug 'arcticicestudio/nord-vim'
Plug 'lilydjwg/colorizer'
" Plug 'dylanaraps/wal.vim'
" }}}

" All of your Plugs must be added before the following line
call plug#end()

" }}}

" Keymaps {{{
nnoremap <F8> :TagbarToggle<CR>
nnoremap <F7> :NERDTreeToggle<CR>
nnoremap <F2> :FZF<CR>
nnoremap <F4> :Nuake<CR>
inoremap <F4> <C-\><C-n>:Nuake<CR>
tnoremap <F4> <C-\><C-n>:Nuake<CR>

" Copy/paste from/to system clipboard
noremap <C-c> "+y
noremap <C-p> "+P

" NOTE: The § is just below esc, and is easier to reach.
" NOTE: The capslock key should also be mapped to escape.
inoremap § <Esc>
" }}}

" NERDTree {{{
let g:NERDTreeDirArrows=0
" }}}

" NERDCommenter {{{
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
" }}}

" Tagbar {{{
let g:tagbar_type_rust = {
    \ 'ctagstype' : 'rust',
    \ 'kinds' : [
        \'T:types,type definitions',
        \'f:functions,function definitions',
        \'g:enum,enumeration names',
        \'s:structure names',
        \'m:modules,module names',
        \'c:consts,static constants',
        \'t:traits',
        \'i:impls,trait implementations',
    \]
\}
let g:tagbar_type_objc = {
    \ 'ctagstype' : 'ObjectiveC',
    \ 'kinds'     : [
        \ 'i:interface',
        \ 'I:implementation',
        \ 'p:Protocol',
        \ 'm:Object_method',
        \ 'c:Class_method',
        \ 'v:Global_variable',
        \ 'F:Object field',
        \ 'f:function',
        \ 'p:property',
        \ 't:type_alias',
        \ 's:type_structure',
        \ 'e:enumeration',
        \ 'M:preprocessor_macro',
    \ ],
    \ 'sro'        : ' ',
    \ 'kind2scope' : {
        \ 'i' : 'interface',
        \ 'I' : 'implementation',
        \ 'p' : 'Protocol',
        \ 's' : 'type_structure',
        \ 'e' : 'enumeration'
    \ },
    \ 'scope2kind' : {
        \ 'interface'      : 'i',
        \ 'implementation' : 'I',
        \ 'Protocol'       : 'p',
        \ 'type_structure' : 's',
        \ 'enumeration'    : 'e'
    \ }
\ }
let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
\ }
" }}}

" Airline {{{
" let g:airline_detect_modified=1
" let g:airline_powerline_fonts=1
" }}}

" Lightline {{{
set noshowmode
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
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
" }}}

" Multicursor {{{
" let g:multi_cursor_next_key='<C-d>'
" }}}

" vimtex {{{
let g:vimtex_compiler_latexmk = {
    \ 'backend' : 'nvim',
    \ 'background' : 1,
    \ 'build_dir' : '',
    \ 'callback' : 1,
    \ 'continuous' : 1,
    \ 'executable' : 'latexmk',
    \ 'options' : [
    \   '-xelatex',
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \ ],
    \}
let g:vimtex_compiler_latexmk_engines = {
    \ '_'                : '-xelatex',
    \ 'pdflatex'         : '-pdf',
    \ 'lualatex'         : '-lualatex',
    \ 'xelatex'          : '-xelatex',
    \ 'context (pdftex)' : '-pdf -pdflatex=texexec',
    \ 'context (luatex)' : '-pdf -pdflatex=context',
    \ 'context (xetex)'  : '-pdf -pdflatex=''texexec --xtx''',
    \}
" }}}

" TeX {{{
" Create environment of the text on the current line.
au FileType tex nmap <leader>te :normal! YPI\begin{<esc>A}<esc>jI\end{<esc>A}<esc>O
" }}}

" Racer {{{
set hidden
let g:racer_cmd = "~/.cargo/bin/racer"
let g:racer_experimental_completer = 1
" au FileType rust nmap gd <Plug>(rust-def)
" au FileType rust nmap gs <Plug>(rust-def-split)
" au FileType rust nmap gx <Plug>(rust-def-vertical)
" au FileType rust nmap <leader>gd <Plug>(rust-doc)
au FileType rust nmap gd :YcmCompleter GoTo<CR>
" }}}

" Chromatica {{{
let g:chromatica#libclang_path='/usr/local/opt/llvm/lib/'
" let g:chromatica#enable_at_startup=1
if has('nvim')
  au FileType c ChromaticaStart
  au FileType cpp ChromaticaStart
  au FileType objc ChromaticaStart
  au FileType objcpp ChromaticaStart
endif
" }}}

" Haskell {{{
au FileType haskell nmap <silent> <leader>ti :GhcModInfo<CR>
au FileType haskell nmap <silent> <leader>tw :GhcModTypeInsert<CR>
au FileType haskell nmap <silent> <leader>ts :GhcModSplitFunCase<CR>
au FileType haskell nmap <silent> <leader>tq :GhcModType<CR>
au FileType haskell nmap <silent> <leader>te :GhcModTypeClear<CR>
au FileType haskell nmap <silent> <leader>hl :GhcModLint<CR>
au FileType haskell set shiftwidth=2
au FileType haskell set tabstop=2
au FileType haskell set softtabstop=2
let g:neoformat_enabled_haskell = ['brittany']
" }}}

" Rust {{{
let g:rustfmt_autosave = 1
let g:syntastic_rust_checkers = ['cargo']
let g:ycm_rust_src_path = '~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src'

" Deoplete Rust {{{
" let g:deoplete#sources#rust#racer_binary='~/.cargo/bin/racer'
" let g:deoplete#sources#rust#rust_source_path='~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src'
" }}}

" }}}

" Go {{{
au BufWritePre *.go GoFmt
" }}}

" Syntastic {{{
set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_signs = 1

let g:syntastic_cpp_gcc_args = "-std=c++11 -Wall -Wextra"
" }}}

" Deoplete {{{
" let g:deoplete#enable_at_startup = 1
" call deoplete#custom#var('clangx', 'clang_binary', '/usr/bin/clang')
" call deoplete#custom#var('clangx', 'default_c_options', '')
" call deoplete#custom#var('clangx', 'default_cpp_options', '')
" }}}

" YouCompleteMe {{{
let g:ycm_auto_trigger=1
let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
let g:ycm_key_list_accept_completion = ['<C-y>']
" }}}

" UltiSnips {{{
let g:UltiSnipsSnippetDirectories=[$HOME.'/.vim/UltiSnips']
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
" }}}

" Startify {{{
" let g:startify_bookmarks = ['~/.vimrc', '~/.zshrc', '~/dotfiles']
" let g:startify_custom_header = map(split(system('figlet -f slant neovim'), '\n'), '"   ". v:val') + ['','']
" The vim logo taken from this file.
" let g:startify_custom_header = map(split(system('sed -n 1,15p ~/.vimrc | cut -c 3-'), '\n'), '"    " . v:val') + ['','']
" }}}

" Markdown {{{
autocmd FileType markdown :hi markdownItalic gui=italic
" }}}

" Makefile {{{
au FileType make set noexpandtab
" }}}

" Khdrc {{{
" Reload khd config on write.
au BufWritePost .khdrc !khd -e reload
" }}}

" Smooth scroll {{{
" noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
" noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
" noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
" noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>
" }}}

" Terminal {{{
if has('neovim')
  autocmd TermOpen * setlocal scrollback=1000

  tnoremap <Esc> <c-\><c-n>
  inoremap <Esc> <Esc><Esc>
endif
" }}}

" Tab {{{
set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
set shiftround
" }}}

" Syntax highlighting {{{
let base16colorspace=256    " Access colors present in 256 colorspace
let t_Co = 256              " Use 256 colors in terminal

" Make sure guicolors are used when available
if has('termguicolors')
    set termguicolors
endif

" Set italics escape sequences.
set t_ZH=[3m
set t_ZR=[23m

syntax on
" filetype plugin indent on
filetype plugin on
" Gruvbox
  colo gruvbox
  let g:gruvbox_contrast_dark='medium'
" Dracula
  " colo dracula

set background=dark
highlight Comment gui=italic
let g:limelight_conceal_ctermfg = 'gray'
" let g:limelight_conceal_guifg = 'DarkGray'
" }}}

" Line number {{{
set cursorline
set number
set relativenumber
set numberwidth=3
" }}}

" Scroll {{{
" set so=3
" }}}

" Mouse {{{
set mouse=nv " Normal and Visual
" }}}

" Wrap {{{
set wrap lbr
" set nowrap
noremap  <buffer> <silent> k gk
noremap  <buffer> <silent> j gj
noremap  <buffer> <silent> 0 g0
noremap  <buffer> <silent> $ g$<Paste>
" }}}

" Python {{{
let g:python_host_prog = "/usr/local/bin/python2.7"
let g:python3_host_prog = "/usr/local/bin/python3"
" }}}
