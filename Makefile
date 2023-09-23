#!/usr/bin/bash

# Generate document in html and pdf formats
quarto render ./src/qmd/iris.qmd  --to html,pdf

# Move output to appropriate location
mv ./src/qmd/*html ./doc
mv ./src/qmd/*pdf ./doc

# Extract code from qmd file into a self-contained R script
Rscript --vanilla ./src/R/purl_code.R
