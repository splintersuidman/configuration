let g:rustfmt_autosave = 1
let g:syntastic_rust_checkers = ['cargo']
let g:ycm_rust_src_path = '~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src'

" nmap gd <Plug>(rust-def)
" nmap gs <Plug>(rust-def-split)
" nmap gx <Plug>(rust-def-vertical)
" nmap <leader>gd <Plug>(rust-doc)
" nmap gd :YcmCompleter GoTo<CR>

" Deoplete {{{
" let g:deoplete#sources#rust#racer_binary='~/.cargo/bin/racer'
" let g:deoplete#sources#rust#rust_source_path='~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src'
" }}}
