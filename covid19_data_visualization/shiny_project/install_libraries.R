# Example R code to install packages
# See http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages for details
#

#install packages
install.packages("shinyjs")

install.packages("rgdal")
install.packages("rgdal", repos="http://R-Forge.R-project.org")
install.packages(c("sf", "tmap"))
#brew install gdal

install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("tmap")
install.packages("sp")


install.packages("tidyverse")
install.packages("gganimate")
install.packages("gifski")
install.packages("av")

install.packages("devtools")
install.packages("geobr")  #brazilian map

# if not work use the commands below
utils::remove.packages('geobr')
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

install.packages("ggrepel")  #include label at end of line chart
install.packages("shinydashboard")
install.packages("flexdashboard")






###########################################################
# Update this line with the R packages to install:

my_packages = c(
  "shiny",
  'shinyjs',
  "tibbletime",
  'dplyr',
  'shinydashboard',
  'gridExtra',
  'ggplot2'
  )


###########################################################

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))
