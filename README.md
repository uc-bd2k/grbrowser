# grbrowser

One of the Shiny applications running at http://www.grcalculator.org

## Installation

Install R package dependencies:

```r
# Install our "shinyLi" package
install.packages("devtools")
devtools::install_github("uc-bd2k/shinyLi")

# Install CRAN package dependencies
install.packages(c("shiny","shinyjs","shinyBS","ggplot2","plotly","jsonlite","stringr","markdown"))

```

## Running the application from R command line

```r
shiny::runGitHub('uc-bd2k/grbrowser')
```

