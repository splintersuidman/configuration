nmap <leader>ll :VimtexCompile<CR>
nmap <leader>lc :VimtexCountWords<CR>
nmap <leader>le :VimtexErrors<CR>
nmap <leader>lv :VimtexView<CR>
" Create environment of the text on the current line.
nmap <leader>te :normal! YPI\begin{<esc>A}<esc>jI\end{<esc>A}<esc>O
