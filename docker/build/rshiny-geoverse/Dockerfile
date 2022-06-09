FROM rocker/shiny:3.6.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  unixodbc-dev \
  lbzip2 \
  libfftw3-dev \
  libgdal-dev \
  libgeos-dev \
  libgsl0-dev \
  libgl1-mesa-dev \
  libglu1-mesa-dev \
  libhdf4-alt-dev \
  libhdf5-dev \
  libjq-dev \
  liblwgeom-dev \
  libproj-dev \
  libprotobuf-dev \
  libnetcdf-dev \
  libudunits2-dev \
  netcdf-bin \
  postgis \
  protobuf-compiler \
  sqlite3 \
  tk-dev \
  libv8-dev \
  libharfbuzz-dev \
  libfribidi-dev
## required for spatialmsk
RUN  install2.r --error \
    --deps TRUE \
    devtools \
## tidyverse includes dplyr and ggplot2
    tidyverse \
    RColorBrewer \
    rgdal \
    leaflet \
    spdplyr \
    lsa \
    stringr \
    DT \
    htmlTable \
    lattice \
    scales \
    googlesheets4 \
    shinythemes \
    shinyWidgets \
    gargle \
    googledrive \
## start rocker/tidyverse
#RUN  install2.r --error FALSE \
#    --deps TRUE \
#    --skipinstalled TRUE \
#    formatR \
#    remotes \
#    selectr
## start rocker/geospatial
#RUN  install2.r --error FALSE \
#    --deps TRUE \
#    --skipinstalled TRUE \
#    RandomFields \
#    RNetCDF \
#    classInt \
#    deldir \
#    gstat \
#    hdf5r \
#    lidR \
#    mapdata \
#    maptools \
#    mapview \
#    ncdf4 \
#    proj4 \
#    raster \
#    rgeos \
#    rlas \
#    sf \
#    sp \
#    spacetime \
#    spatstat \
#    spdep \
#    geoR \
#    geosphere \
#    caTools \
#    BiocManager \
## from bioconductor
#    && R -e "BiocManager::install('rhdf5', update=FALSE, ask=FALSE)"
RUN rm -rf /tmp/downloaded_packages
