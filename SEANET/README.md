# SEANET Adaptation
Adapted from SuperZIP demo (see below) for the Sustainable Ecological Aquaculture Network (SEANET).

SEANET Map: https://rshiny.spatialmsk.com/SEANET/


# SuperZIP demo
See a version of it live at http://shiny.rstudio.com/gallery/superzip-example.html

You can run this demo with:
```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("rstudio/shiny-examples", subdir="063-superzip-example")
```

Data compiled for _Coming Apart: The State of White America, 1960–2010_ by Charles Murray (Crown Forum, 2012). This app was inspired by the Washington Post's interactive feature _[Washington: A world apart](http://www.washingtonpost.com/sf/local/2013/11/09/washington-a-world-apart/)_.
