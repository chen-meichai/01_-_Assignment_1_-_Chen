:: Generate document in html and pdf formats
:: by running an R script to render each format
:: and places it into the appropriate location
Rscript.exe --vanilla ./src/R/compile.R

:: Extract code from qmd file into a self-contained R script
Rscript.exe --vanilla ./src/R/purl_code.R
