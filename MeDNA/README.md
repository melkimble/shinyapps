# Maine-eDNA Survey123 Site summaries

Support for this project is provided through a National Science Foundation award to [Maine EPSCoR at the University of Maine](#https://umaine.edu/edna/) and is part of the RII Track-1: Molecule to Ecosystem: Environmental DNA as a Nexus of Coastal Ecosystem Sustainability for Maine (Maine-eDNA).

This application was built with RShiny and the field data were collected through Maine-eDNA. The HTML template, CSS, and JS were sampled from  [The Intelligentsia GitHub Repo](#https://github.com/phillyo/intelligentsia). Through the sampled HTML, CSS, and JS, the following were also utilized:
-[x] [FitText.js](#https://github.com/davatron5000/FitText.js)
-[x] [jQuery](#http://jquery.com/)
-[x] [Font Awesome](#http://fontawesome.io/)


The following R packages were used in to build this RShiny application:
|   |   |   |   |   |   |
|:---:|:---:|:---:|:---:|:---:|:---:|
|base|dplyr|data.table|DT|shinythemes|viridis|
|leaflet|lubridate|plyr|raster|rgdal|rgeos|
|spdplyr|sf|rmapshaper|shiny|ggplot2|scales|
|shinyjs|stringr|gridExtra|tidyr|utils|googlesheets4|
|zoo|---|---|---|---|---|

      
###Data Disclaimer
The data from UMaineGIS are provided "as is", and UMaineGIS assumes no responsibility for erros or omissions. The User assumes the entire risk associated with its use of these data. UMaineGIS shall not be held liable for any use or misuse of the data described and/or contained herein. The User bears all responsibility in determining whether these data are fit for the User's intended use.

The information contained in these data are dynamic and may change over time.
  
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runApp("shinyapps/", subdir="MeDNA")
