" Custom Colemak-friendly keybindings for vim.
" P.C. Shyamshankar <sykora@lucentbeing.com>
"
" Meant for colemak, but this file contains all keymappings I use, colemak
" specific or not.
"
" TODO
" Keys that are available:
" 
" w - Do I need the forward-word functionality?
" g, j, l
" b - Look at 'w'
" Shifted versions of most characters
" Spacebar - Mapleader?

" Marking
nnoremap , m

" Jumping forward and backward to a character in a line
nnoremap p t
onoremap p t

nnoremap P T
onoremap P T

" s <-> g mappings
nmap s g
vnoremap s g
nnoremap S G
vnoremap S G
nnoremap ss gg
vnoremap ss gg
vnoremap sq gq

" Movement
nnoremap n h
nnoremap e gj
nnoremap i gk
nnoremap o l

vnoremap n h
vnoremap e gj
vnoremap i gk
vnoremap o l

noremap <M-n> <C-w><C-h>
noremap <M-e> <C-w><C-j>
noremap <M-i> <C-w><C-k>
noremap <M-o> <C-w><C-l>

" For terminal vim
noremap <C-w><C-n> <C-w><C-h>
noremap <C-w><C-e> <C-w><C-j>
noremap <C-w><C-i> <C-w><C-k>
noremap <C-w><C-o> <C-w><C-l>

noremap <C-w>n <C-w><C-h>
noremap <C-w>e <C-w><C-j>
noremap <C-w>i <C-w><C-k>
noremap <C-w>o <C-w><C-l>

noremap y o
noremap Y O

nnoremap t n
nnoremap T N

" Insert Mode, and Text Objects.
vnoremap h i
nnoremap h i
nnoremap H I
omap h i

" Cut & Paste.
noremap k y
noremap K y$
noremap m p
noremap M P 
noremap ]m ]p
noremap ]M ]P

" Fuzzy Finder mappings.
nnoremap <Leader>fb :FufBuffer<CR>
nnoremap <Leader>ff :FufFileWithCurrentBufferDir<CR>
