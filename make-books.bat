REM Batch file to make "newLISP-Code.lsp" and "newLISP-Extra.lsp"
REM make newLISP-Note.lsp
del "newLISP-Code.lsp" > nul
del "newLISP-Extra.lsp" > nul
type *.lsp > newLISP-Code
rename "newLISP-Code" "newLISP-Code.lsp"
REM make newLISP-Extra.lsp
type *.lisp > newLISP-Extra
rename "newLISP-Extra" "newLISP-Extra.lsp"