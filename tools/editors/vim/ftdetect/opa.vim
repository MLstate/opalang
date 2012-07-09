augroup filetypedetect
  au! BufRead,BufNewFile *.js.opa      setfiletype opajs
  au! BufRead,BufNewFile *.classic.opa setfiletype opa
  au! BufRead,BufNewFile *.opa         setfiletype opa
augroup END
