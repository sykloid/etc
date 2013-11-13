map <leader>ll :Latexmk<CR>
map <leader>lv :LatexView<CR>

nmap <leader>lce <plug>LatexChangeEnv
vmap <leader>lwc <plug>LatexWrapSelection
vmap <leader>lwe <plug>LatexEnvWrapSelection

vmap h$ <plug>LatexBox_SelectInlineMathInner
vmap he <plug>LatexBox_SelectCurrentEnvInner
vmap a$ <plug>LatexBox_SelectInlineMathOuter
vmap ae <plug>LatexBox_SelectCurrentEnvOuter
omap h$ :normal vh$<CR>
omap he :normal vhe<CR>
omap a$ :normal va$<CR>
omap ae :normal vae<CR>

let g:LatexBox_viewer = "okular"
setlocal omnifunc=LatexBox_Complete
