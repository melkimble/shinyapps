# Maine-eDNA Survey123 Site summaries

if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runApp("shinyapps/", subdir="MeDNA")
