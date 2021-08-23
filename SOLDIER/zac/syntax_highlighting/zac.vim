" Vim syntax file
" Language: Zac

" Usage Instructions
" Put this file in .vim/syntax/zac.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.zac set filetype=zac

if exists("b:current_syntax")
  finish
endif

syntax keyword zacTodos TODO XXX FIXME NOTE



" Language keywords
syntax keyword zacKeywords let defn
syntax keyword zacLoopKeywords while if

" Comments
syntax region zacCommentLine start="//" end="$"   contains=zacTodos,zacCommentIdent
syntax region zacDirective start="%" end=" "

syntax match zacCommentIdent		"#[a-z_][a-z0-9_]*\>"
syntax match zacIdent		"\<[a-z_][a-z0-9_]*\>"

" Numbers
syntax match zacDecInt display "\<[0-9][0-9_]*"
syntax match zacHexInt display "\<0[xX][0-9a-fA-F][0-9_a-fA-F]*"
syntax match zacFloat  display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)"

" Functions
syntax match zacFunction display "\<[a-z_][a-z0-9_]*\>("he=e-1

" TODO: we don't have string literalls... yet? or at all?
" Strings
" syntax region zacString start=/\v"/ skip=/\v\\./ end=/\v"/
" syntax region zacString start=/\v'/ skip=/\v\\./ end=/\v'/

" Set highlights
highlight default link zacTodos Todo
highlight default link zacKeywords Keyword
" comments are highlighted as strings in this lang
" TODO: only some strings treated as values highlighted this way
" highlight default link zacCommentLine Comment
highlight default link zacLoopKeywords Repeat
highlight default link zacDecInt Number
highlight default link zacHexInt Number
highlight default link zacFloat Float
highlight default link zacCommentLine Comment
highlight default link zacIdent Identifier
highlight default link zacCommentIdent Special
highlight default link zacFunction Function

let b:current_syntax = "zac"
