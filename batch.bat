:: Generate document in html and pdf formats
quarto.exe render .\src\qmd\population-dynamics.qmd  --to html

:: Move output to appropriate location
move /y .\src\qmd\*.html .\doc
