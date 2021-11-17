You can run this with:
```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("shinyapps/", subdir="SEANET")
```

Data compiled for _Maine Department of Marine Resources Lease Site Profiles by Melissa Kimble (SEANET, 2018)_. Bathymetric data obtained from the University of New Hampshire (UNH) Joint Hydrographic Center/Center for Coastal and Ocean Mapping (JHC/CCOM). Sea Surface Temperature data obtained from the Coastal Satellite Oceanography team at the University of Maine Aquaculture lease data obtained from the Maine Department of Marine Resources.

# shinyapps
RShiny applications adapted from RShiny examples ( https://github.com/rstudio/shiny-examples ).

SEANET Map: https://rshiny.spatialmsk.com/SEANET/

### Data Disclaimer
The data on spatialmsk or github.com/melkimble are provided "as is", and spatialmsk or github.com/melkimble assumes no responsibility for errors or omissions. The User assumes the entire risk associated with its use of these data. Spatialmsk or github.com/melkimble shall not be held liable for any use or misuse of the data described and/or contained herein. The User bears all responsibility in determining whether these data are fit for the User's intended use.

The information contained in these data is dynamic and may change over time. The data are not better than the original sources from which they were derived, and both scale and accuracy may vary across the data set. These data may not have the accuracy, resolution, completeness, timeliness, or other characteristics appropriate for applications that potential users of the data may contemplate. The User is encouraged to carefully consider the content of the metadata file associated with these data when available. These data are neither official records, nor legal documents and must not be used as such.

Spatialmsk or github.com/melkimble should be cited as the data source in any products derived from these data. Any Users wishing to modify the data are obligated to describe the types of modifications they have performed. The User specifically agrees not to misrepresent the data, nor to imply that changes made were approved or endorsed by spatialmsk or github.com/melkimble. This information may be updated without notification. By using these data you hereby agree to these conditions.

No warranty is made by spatialmsk or github.com/melkimble for use of the data for purposes not intended by spatialmsk or github.com/melkimble. Spatialmsk or github.com/melkimble assumes no responsibility for errors or omissions. No warranty is made by spatialmsk or github.com/melkimble as to the accuracy, reliability, relevancy, timeliness, utility, or completeness of these data, maps, geographic location for individual use or aggregate use with other data; nor shall the act of distribution to contractors, partners, or beyond, constitute any such warranty for individual or aggregate data use with other data. Although these data have been processed successfully on computers of spatialmsk or github.com/melkimble, no warranty, expressed or implied, is made by spatialmsk or github.com/melkimble regarding the use of these data on any other system, or for general or scientific purposes, nor does the fact of distribution constitute or imply any such warranty. In no event shall spatialmsk or github.com/melkimble have any liability whatsoever for payment of any consequential, incidental, indirect, special, or tort damages of any kind, including, but not limited to, any loss of profits arising out of the use or reliance on the geographic data or arising out of the delivery, installation, operation, or support by spatialmsk or github.com/melkimble. Please read the DOI disclaimer for more information.

### Disclaimer of Liability
While spatialmsk or github.com/melkimble strives to make the information on this website as timely and accurate as possible, spatialmsk or github.com/melkimble makes no claims, promises, or guarantees about the accuracy, completeness, or adequacy of the contents of this site, and expressly disclaims liability for errors and omissions in the contents of this website. No warranty of any kind, implied, expressed, or statutory, including but not limited to the warranties of non-infringement of third party rights, title, merchantability, fitness for a particular purpose or freedom from computer virus, is given with respect to the contents of this website or its links to other Internet resources.

Reference in this repository to any specific commercial product, process, or service, or the use of any trade, firm or corporation name is for the information and convenience of the public, and does not constitute endorsement, recommendation, or favoring by spatialmsk or github.com/melkimble.

*Adapted from U.S. Department of the Interior Disclaimer of Liability and BLM Navigator Data Disclaimer.

See a version of it live at http://shiny.rstudio.com/gallery/superzip-example.html
