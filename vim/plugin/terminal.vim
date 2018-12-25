if has('neovim')
  autocmd TermOpen * setlocal scrollback=1000

  tnoremap <Esc> <c-\><c-n>
  inoremap <Esc> <Esc><Esc>
endif
