" skywave.vim: A dark colorscheme for 256 color aware vim.
" P.C. Shyamshankar <sykora@lucentbeing.com>

highlight clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "skywave"
set background=dark

" Basics
highlight Normal       cterm=none         ctermfg=253         ctermbg=0
highlight NonText      cterm=none         ctermfg=253         ctermbg=0
highlight SpecialKey   cterm=none         ctermfg=236         ctermbg=0
                                  
" The Cursor
highlight Cursor       cterm=none         ctermfg=253         ctermbg=0
highlight CursorLine   cterm=none         ctermfg=253         ctermbg=233
highlight CursorColumn cterm=none         ctermfg=253         ctermbg=233

" Window Elements
highlight LineNr       cterm=none         ctermfg=242         ctermbg=0

highlight StatusLine   cterm=none         ctermfg=255         ctermbg=232
highlight StatusLineNC cterm=none         ctermfg=238         ctermbg=232

highlight SignColumn   cterm=none         ctermfg=253         ctermbg=0
highlight VertSplit    cterm=none         ctermfg=238         ctermbg=232

highlight TabLine      cterm=none         ctermfg=253         ctermbg=0
highlight TabLineSel   cterm=none         ctermfg=253         ctermbg=0
highlight TabLineFill  cterm=none         ctermfg=253         ctermbg=0

" The Popup Menu
highlight Pmenu        cterm=none         ctermfg=228         ctermbg=232
highlight PmenuSel     cterm=bold         ctermfg=232         ctermbg=228
highlight PmenuSbar                                           ctermbg=237
highlight PmenuThumb                                          ctermbg=255

" Folds
highlight Folded       cterm=bold         ctermfg=043         ctermbg=233
highlight FoldColumn   cterm=bold         ctermfg=043         ctermbg=233

" Diff
highlight DiffAdd      cterm=none         ctermfg=000         ctermbg=082
highlight DiffChange   cterm=none         ctermfg=000         ctermbg=228
highlight DiffDelete   cterm=none         ctermfg=253         ctermbg=197
highlight DiffText     cterm=none         ctermfg=228         ctermbg=099

" Spelling
highlight SpellBad     cterm=underline    ctermfg=198         ctermbg=0
highlight SpellCap     cterm=underline    ctermfg=051         ctermbg=0
highlight SpellLocal   cterm=underline    ctermfg=081         ctermbg=0
highlight SpellRare    cterm=underline    ctermfg=220         ctermbg=0

" Messages
highlight Directory    cterm=bold         ctermfg=063         ctermbg=0
highlight ErrorMsg     cterm=bold         ctermfg=197         ctermbg=0
highlight ModeMsg      cterm=bold         ctermfg=111         ctermbg=0
highlight MoreMsg      cterm=bold         ctermfg=043         ctermbg=0
highlight Question     cterm=bold         ctermfg=082         ctermbg=0
highlight Title        cterm=none         ctermfg=253         ctermbg=0
highlight WarningMsg   cterm=none         ctermfg=197         ctermbg=0
highlight WildMenu     cterm=bold         ctermfg=228         ctermbg=232

highlight Visual       cterm=none                             ctermbg=235

highlight MatchParen   cterm=bold         ctermfg=082         ctermbg=0
                                      
highlight Search       cterm=reverse      ctermfg=253         ctermbg=0
highlight IncSearch    cterm=reverse
                                      
" Syntax Groups
highlight Comment      cterm=none           ctermfg=238         ctermbg=0
                                                    
highlight Constant     cterm=none         ctermfg=147         ctermbg=0
highlight String       cterm=none         ctermfg=147         ctermbg=0
highlight Character    cterm=none         ctermfg=015         ctermbg=0
highlight Number       cterm=none         ctermfg=115         ctermbg=0
highlight Boolean      cterm=none         ctermfg=117         ctermbg=0
highlight Float        cterm=none         ctermfg=115         ctermbg=0
                                                    
highlight Identifier   cterm=none         ctermfg=117         ctermbg=0
highlight Function     cterm=bold         ctermfg=222         ctermbg=0
                                                    
highlight Statement    cterm=none         ctermfg=078         ctermbg=0
highlight Conditional  cterm=none         ctermfg=119         ctermbg=0
highlight Repeat       cterm=none         ctermfg=119         ctermbg=0
highlight Label        cterm=none         ctermfg=119         ctermbg=0
highlight Operator     cterm=bold         ctermfg=172         ctermbg=0
highlight Keyword      cterm=none         ctermfg=117         ctermbg=0
highlight Exception    cterm=bold         ctermfg=197         ctermbg=0
                                                    
highlight PreProc      cterm=none         ctermfg=167         ctermbg=0
                                                    
highlight Type         cterm=none         ctermfg=216         ctermbg=0
highlight Structure    cterm=none         ctermfg=216         ctermbg=0
                                                    
highlight Special      cterm=bold         ctermfg=224         ctermbg=0
highlight SpecialChar  cterm=none         ctermfg=224         ctermbg=0
highlight Delimiter    cterm=none         ctermfg=015         ctermbg=0
                                                    
highlight Underlined   cterm=underline    ctermfg=075         ctermbg=0
                                                    
highlight Ignore       cterm=none         ctermfg=000         ctermbg=0
                                                    
highlight Error        cterm=bold         ctermfg=197         ctermbg=0
                                                    
highlight TODO         cterm=bold         ctermfg=172         ctermbg=0

highlight User1        cterm=none         ctermfg=215         ctermbg=232
highlight User2        cterm=none         ctermfg=082         ctermbg=232
highlight User3        cterm=none         ctermfg=051         ctermbg=232
highlight User4        cterm=none         ctermfg=196         ctermbg=232
highlight User5        cterm=none         ctermfg=069         ctermbg=232
highlight User6        cterm=none         ctermfg=220         ctermbg=232
highlight User7        cterm=none         ctermfg=255         ctermbg=232

highlight SignifySignAdd    cterm=bold ctermfg=118 ctermbg=none
highlight SignifySignChange cterm=bold ctermfg=214 ctermbg=none
highlight SignifySignDelete cterm=bold ctermfg=196 ctermbg=none
